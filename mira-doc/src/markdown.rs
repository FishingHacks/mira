use mira_spans::context::DocComment;

use crate::html::{HTMLEscapeExt, HTMLGenerateContext};

impl HTMLGenerateContext<'_> {
    pub fn generate_markdown(&self, markdown: &str, output: &mut String) {
        if markdown.is_empty() {
            return;
        }
        output.push_str("<p class=\"description\">");
        output.escaped().push_str(markdown);
        output.push_str("</p>");
    }

    pub fn generate_doc_comment(&self, comment: DocComment, output: &mut String) {
        self.tc_ctx
            .ctx
            .with_doc_comment(comment, |v| self.generate_markdown(v, output));
    }
}
