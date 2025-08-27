use std::collections::HashMap;

use mira_spans::context::DocComment;
use pulldown_cmark::{CodeBlockKind, CowStr, Event, Options, Parser, Tag, TagEnd};

use crate::html::{HTMLEscapeExt, HTMLGenerateContext, StringExt};

const OPTIONS: Options = Options::ENABLE_TABLES
    .union(Options::ENABLE_FOOTNOTES)
    .union(Options::ENABLE_STRIKETHROUGH)
    .union(Options::ENABLE_TASKLISTS)
    .union(Options::ENABLE_SMART_PUNCTUATION);

impl HTMLGenerateContext<'_> {
    pub fn generate_markdown(&self, markdown: &str, output: &mut String) {
        if markdown.is_empty() {
            return;
        }
        let parser = Parser::new_ext(markdown, OPTIONS);
        let passes = FootnotePass::new(CodeblockPass(parser));
        pulldown_cmark::html::push_html(output, passes);
    }

    pub fn generate_doc_comment(&self, comment: DocComment, output: &mut String) {
        self.tc_ctx.ctx.with_doc_comment(comment, |v| {
            if v.is_empty() {
                return;
            }
            output.push_str(r#"<div class="description">"#);
            self.generate_markdown(v, output);
            output.push_str("</div>");
        });
    }
}

struct CodeblockPass<'a, I: Iterator<Item = Event<'a>>>(I);

impl<'a, I> Iterator for CodeblockPass<'a, I>
where
    I: Iterator<Item = Event<'a>>,
{
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let ev = self.0.next();
        let Some(Event::Start(Tag::CodeBlock(kind))) = ev else {
            return ev;
        };
        let lang = match kind {
            CodeBlockKind::Indented => String::new(),
            CodeBlockKind::Fenced(lang) if lang.is_empty() => String::new(),
            CodeBlockKind::Fenced(lang) => format!(" language-{lang}"),
        };

        let mut html = format!(r#"<pre class="code{lang}"><code>"#);
        for v in &mut self.0 {
            match v {
                Event::End(TagEnd::CodeBlock) => break,
                Event::Text(s) => html.escaped().push_str(&s),
                _ => unreachable!("codeblock: unsupported event: {v:?}"),
            }
        }
        html.push_str("</code></pre>");

        Some(Event::Html(html.into()))
    }
}

struct FootnotePass<'a, I> {
    inner: I,
    footnotes: HashMap<CowStr<'a>, FootnoteData<'a>>,
}

impl<'a, I> FootnotePass<'a, I> {
    fn new(inner: I) -> Self {
        Self {
            inner,
            footnotes: HashMap::new(),
        }
    }

    fn get_footnote_data(&mut self, v: CowStr<'a>) -> &mut FootnoteData<'a> {
        let new_id = self.footnotes.len();
        self.footnotes.entry(v).or_insert(FootnoteData {
            id: new_id,
            num_refs: 0,
            content: Vec::new(),
        })
    }

    fn handle_footnote_ref(&mut self, footnote: CowStr<'a>) -> Event<'a> {
        let data = self.get_footnote_data(footnote);
        let backref = data.num_refs;
        let id = data.id;
        data.num_refs += 1;
        let html = format!(
            r##"<sup id="footnote_ref{id}_{backref}" class="footnote_ref"><a href="#footnote{id}">{}</a></sup>"##,
            id + 1
        );
        Event::Html(html.into())
    }
}

struct FootnoteData<'a> {
    id: usize,
    num_refs: usize,
    content: Vec<Event<'a>>,
}

impl<'a, I> Iterator for FootnotePass<'a, I>
where
    I: Iterator<Item = Event<'a>>,
{
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.inner.next() {
                Some(Event::FootnoteReference(footnote)) => {
                    return Some(self.handle_footnote_ref(footnote));
                }
                Some(Event::Start(Tag::FootnoteDefinition(name))) => {
                    self.get_footnote_data(name.clone());
                    while let Some(v) = self.inner.next() {
                        let ev = match v {
                            Event::End(TagEnd::FootnoteDefinition) => break,
                            Event::FootnoteReference(footnote) => {
                                self.handle_footnote_ref(footnote)
                            }
                            _ => v,
                        };
                        self.footnotes.get_mut(&name).unwrap().content.push(ev);
                    }
                }
                Some(ev) => return Some(ev),
                None if self.footnotes.is_empty() => return None,
                // render footnotes at the end of the document.
                None => {
                    let mut html = "<div class=\"footnotes\"><hr /><ol>".to_string();
                    for (_, mut data) in self.footnotes.drain().filter(|v| v.1.num_refs > 0) {
                        let mut is_paragraph = false;
                        if let Some(&Event::End(TagEnd::Paragraph)) = data.content.last() {
                            is_paragraph = true;
                            data.content.pop();
                        }
                        let id = data.id;
                        html.push_str("<li>");
                        pulldown_cmark::html::push_html(&mut html, data.content.into_iter());
                        for ref_id in 0..data.num_refs {
                            html.push_fmt(format_args!(
                                r##"&nbsp;<a href="#footnote_ref{id}_{ref_id}">↩</a>"##
                            ));
                        }
                        if is_paragraph {
                            html.push_str("</p>");
                        }
                        html.push_str("</li>");
                    }
                    html.push_str("</ol></div>");
                    return Some(Event::Html(html.into()));
                }
            }
        }
    }
}
