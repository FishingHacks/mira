use std::collections::{HashMap, HashSet};
use std::path::Path;

use mira_common::store::StoreKey;
use mira_context::DocComment;
use mira_errors::Diagnostic;
use mira_spans::{Ident, Span};
use mira_typeck::TypedModule;
use pulldown_cmark::{BrokenLink, CodeBlockKind, CowStr, Event, Options, Parser, Tag, TagEnd};

use crate::html::{HTMLEscapeExt, HTMLGenerateContext, StringExt};

const OPTIONS: Options = Options::ENABLE_TABLES
    .union(Options::ENABLE_FOOTNOTES)
    .union(Options::ENABLE_STRIKETHROUGH)
    .union(Options::ENABLE_TASKLISTS)
    .union(Options::ENABLE_SMART_PUNCTUATION)
    .union(Options::ENABLE_GFM);

impl<'ctx> HTMLGenerateContext<'ctx> {
    fn resolve_ref(
        &self,
        mut item: &str,
        module: StoreKey<TypedModule<'ctx>>,
        current_path: &Path,
    ) -> Option<CowStr<'static>> {
        if let Some(v) = crate::default_ty_links::primitive_link_from_str(item) {
            return Some(CowStr::Borrowed(v));
        }
        if item.starts_with('`') && item.ends_with('`') {
            item = &item[1..item.len() - 1];
        }

        let mut imports = vec![];
        for entry in item.split("::") {
            // entry must not be empty, must only contain alphanumeric characters, `_`, `$` or `#`
            // and must not begin with a number.
            if entry.is_empty()
                || !entry
                    .chars()
                    .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '$' || c == '#')
                || matches!(entry.chars().next(), Some('0'..='9'))
            {
                return None;
            }
            let sym = self.tc_ctx.ctx.intern_str(entry);
            imports.push(Ident::new(sym, Span::DUMMY));
        }

        let entry = self
            .tc_ctx
            .typed_resolve_import(module, &imports, Span::DUMMY, &mut HashSet::new())
            .map_err(Diagnostic::dismiss)
            .ok()?;
        let url = self.get_item_path(entry, current_path);
        Some(url.into())
    }

    fn resolve_broken_link<'a>(
        &self,
        module: StoreKey<TypedModule<'ctx>>,
        link: BrokenLink<'a>,
        current_path: &Path,
    ) -> Option<(CowStr<'a>, CowStr<'a>)> {
        let link_url = self.resolve_ref(&link.reference, module, current_path)?;
        Some((link_url, link.reference))
    }

    pub(crate) fn generate_markdown(
        &self,
        markdown: &str,
        output: &mut String,
        module: StoreKey<TypedModule<'ctx>>,
        current_path: &Path,
    ) {
        if markdown.is_empty() {
            return;
        }
        let parser = Parser::new_with_broken_link_callback(
            markdown,
            OPTIONS,
            Some(|broken_link| self.resolve_broken_link(module, broken_link, current_path)),
        );
        let passes = FootnotePass::new(CodeblockPass(parser));
        pulldown_cmark::html::push_html(output, passes);
    }

    pub(crate) fn generate_ref_comment(
        &self,
        comment: DocComment,
        output: &mut String,
        module: StoreKey<TypedModule<'ctx>>,
        current_path: &Path,
    ) {
        self.tc_ctx.ctx.with_doc_comment(comment, |v| {
            let Some(line) = v.lines().find(|l| !l.is_empty()) else {
                return;
            };
            output.push_str(r#"<dd class="description">"#);
            self.generate_markdown(line, output, module, current_path);
            output.push_str("</dd>");
        });
    }

    pub(crate) fn generate_doc_comment(
        &self,
        comment: DocComment,
        output: &mut String,
        module: StoreKey<TypedModule<'ctx>>,
        current_path: &Path,
    ) {
        self.tc_ctx.ctx.with_doc_comment(comment, |v| {
            if v.is_empty() {
                return;
            }

            output.push_str(r#"<details class="description-toggle" open><summary><span>Expand</span></summary><div class="description">"#);
            self.generate_markdown(v, output, module, current_path);
            output.push_str("</div></details>");
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
