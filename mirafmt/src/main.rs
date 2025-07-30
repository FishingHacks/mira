use mira::{
    Arena, AsciiPrinter, Output,
    annotations::Annotations,
    context::{GlobalContext, SharedContext},
    module::Module,
    parser::{FunctionContract, Path as MPath, PathWithoutGenerics, Statement, TypeRef},
    store::Store,
    tokenizer::Tokenizer,
};
use mira_errors::{DiagnosticFormatter, Diagnostics, Styles};
use mira_macros::ErrorData;
use mira_spans::SourceMap;
use std::{
    cell::Cell,
    collections::{HashMap, HashSet},
    io::{Read, stdin},
    ops::{BitOr, BitOrAssign},
    os::linux::raw::stat,
    path::Path,
    sync::Arc,
};

fn main() {
    let arena = Arena::new();
    let g_ctx = GlobalContext::new(&arena);
    let ctx = g_ctx.share();
    if let Err(e) = _main(ctx) {
        for diagnostic in e {
            DiagnosticFormatter::new(
                ctx.source_map(),
                Output::Stdout,
                AsciiPrinter::default(),
                Styles::DEFAULT,
            )
            .display_diagnostic(diagnostic)
            .expect("failed to display diagnostic");
        }
    }
}

fn _main<'arena>(ctx: SharedContext<'arena>) -> Result<(), Diagnostics<'arena>> {
    ctx.init_source_map(SourceMap::new([].into()));
    let mut errs = Diagnostics::new();
    let mut buf = String::new();
    if let Err(e) = stdin().read_to_string(&mut buf) {
        errs.add_stdin_read_error(e);
        return Err(errs);
    }
    let file =
        ctx.source_map()
            .new_file(Path::new("stdin").into(), Path::new("/").into(), buf.into());
    let mut tokenizer = Tokenizer::new(ctx, file.clone());
    if tokenizer
        .scan_tokens()
        .map_err(|e| e.into_iter().map(|v| _ = errs.add_err(v)))
        .is_err()
    {
        return Err(errs);
    }
    let mut store = Store::new();
    let key = store.insert(Module::new(
        HashMap::new(),
        file.path.clone(),
        file.package_root.clone(),
    ));
    let modules = Arc::new(store.into());
    let mut parser =
        tokenizer.to_parser(Arc::new(Vec::new().into()), &modules, ctx.source_map(), key);
    let (statements, parse_errs) = parser.parse_all();
    for err in parse_errs {
        errs.add_err(err);
    }
    if !errs.is_empty() {
        return Err(errs);
    }
    let builder = Builder::new();
    let mut last_line = 0;
    let mut generator = Generator::new(80);
    for statement in statements {
        let new_line = file
            .lookup_line(statement.span().get_span_data().pos)
            .unwrap_or(usize::MAX);
        if new_line - last_line > 1 {
            generator.generate(&Node::HardLine);
        }
        last_line = new_line;
        generator.generate(&builder.generate_statement(&statement));
        generator.new_line();
    }
    println!("{}", generator.into_string());
    Ok(())
}

#[derive(ErrorData)]
#[error("failed to read from stdin: {_0}")]
#[no_arena_lifetime]
struct StdinReadError(std::io::Error);

#[derive(Clone)]
pub enum Node {
    Text(Box<str>),
    StaticText(&'static str),
    SpaceOrLine,
    Line,
    HardLine,
    EmptyLine,
    Indent(Box<[Node]>),
    IndentNext(Box<[Node]>),
    Group(usize, Box<[Node]>),
    Nodes(Box<[Node]>),
    ZeroSized(Box<[Node]>),
    IfWrap(usize, Box<Node>, Box<Node>),
    WrapIf(usize, Box<Node>),
    Empty,
}

impl Node {
    pub const fn static_text(s: &'static str) -> Self {
        Self::StaticText(s)
    }

    pub fn text(s: impl Into<Box<str>>) -> Self {
        Self::Text(s.into())
    }

    pub fn group(id: usize, nodes: impl Into<Box<[Node]>>) -> Self {
        Self::Group(id, nodes.into())
    }

    pub fn nodes(nodes: impl Into<Box<[Node]>>) -> Self {
        Self::Nodes(nodes.into())
    }

    pub fn zero_sized(nodes: impl Into<Box<[Node]>>) -> Self {
        Self::ZeroSized(nodes.into())
    }

    pub fn indent(nodes: impl Into<Box<[Node]>>) -> Self {
        Self::Indent(nodes.into())
    }

    pub fn indent_next(nodes: impl Into<Box<[Node]>>) -> Self {
        Self::IndentNext(nodes.into())
    }

    pub fn if_wrap(group: usize, a: Node, b: Node) -> Self {
        Self::IfWrap(group, Box::new(a), Box::new(b))
    }

    pub fn wrap_if(group: usize, n: Node) -> Self {
        Self::WrapIf(group, Box::new(n))
    }

    pub fn width(&self, wrapped: &HashSet<usize>) -> usize {
        match self {
            Node::Text(t) => t.chars().count(),
            Node::StaticText(t) => t.chars().count(),
            Node::SpaceOrLine => 1,
            Node::IfWrap(id, node, _) if wrapped.contains(id) => node.width(wrapped),
            Node::IfWrap(_, _, node) | Node::WrapIf(_, node) => node.width(wrapped),
            Node::Group(_, nodes)
            | Node::Nodes(nodes)
            | Node::IndentNext(nodes)
            | Node::Indent(nodes) => nodes.iter().map(|v| v.width(wrapped)).sum(),
            Node::Line | Node::Empty | Node::HardLine | Node::EmptyLine | Node::ZeroSized(_) => 0,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Wrapping {
    Enabled,
    Detecting,
}

impl Wrapping {
    pub fn enabled(self) -> bool {
        matches!(self, Self::Enabled)
    }
}

impl BitOr for Wrapping {
    type Output = Wrapping;

    fn bitor(self, rhs: Self) -> Self::Output {
        match self {
            Wrapping::Enabled => self,
            Wrapping::Detecting => rhs,
        }
    }
}

impl BitOrAssign for Wrapping {
    fn bitor_assign(&mut self, rhs: Self) {
        if let Wrapping::Detecting = self {
            *self = rhs;
        }
    }
}

pub struct Generator {
    buf: String,
    indents: usize,
    pending_indents: usize,
    line_len: usize,
    max_line_len: usize,
    wrapped_groups: HashSet<usize>,
}

impl Generator {
    pub fn new(max_line_len: usize) -> Self {
        Self {
            max_line_len,
            buf: String::new(),
            indents: 0,
            pending_indents: 0,
            line_len: 0,
            wrapped_groups: HashSet::new(),
        }
    }

    pub fn generate(&mut self, node: &Node) {
        self.node(node, Wrapping::Detecting);
    }

    fn node(&mut self, node: &Node, wrapping: Wrapping) {
        match node {
            Node::Text(s) => self.text(s),
            Node::StaticText(s) => self.text(s),
            Node::SpaceOrLine if wrapping.enabled() => self.new_line(),
            Node::SpaceOrLine => self.text(" "),
            Node::Line if wrapping.enabled() => self.new_line(),
            Node::Indent(nodes) if wrapping.enabled() => {
                self.line_len += INDENT.len();
                self.indents += 1;
                self.buf.push_str(INDENT);
                nodes.iter().for_each(|n| self.node(n, wrapping));
                self.indents -= 1;
            }
            Node::IndentNext(nodes) if wrapping.enabled() => {
                self.pending_indents += 1;
                nodes.iter().for_each(|n| self.node(n, wrapping));
                // really remove the indent, even if this block wasn't indented and there was no
                // new line.
                if self.indents == 0 {
                    self.pending_indents = self.pending_indents.wrapping_sub(1)
                } else {
                    self.indents -= 1;
                }
            }
            Node::Group(id, nodes) => {
                let width: usize = nodes.iter().map(|v| v.width(&self.wrapped_groups)).sum();
                let wrap = if self.line_len + width > self.max_line_len {
                    self.wrapped_groups.insert(*id);
                    Wrapping::Enabled
                } else {
                    Wrapping::Detecting
                };

                nodes.iter().for_each(|n| self.node(n, wrap));
            }
            Node::WrapIf(id, n) => {
                let wrap = if self.wrapped_groups.contains(id) {
                    Wrapping::Enabled
                } else {
                    Wrapping::Detecting
                };

                self.node(n, wrap);
            }
            Node::Indent(nodes)
            | Node::Nodes(nodes)
            | Node::IndentNext(nodes)
            | Node::ZeroSized(nodes) => nodes.iter().for_each(|node| self.node(node, wrapping)),
            Node::IfWrap(id, node, _) if self.wrapped_groups.contains(id) => {
                self.node(node, Wrapping::Enabled)
            }
            Node::IfWrap(_, _, node) => self.node(node, wrapping),
            Node::HardLine => self.new_line(),
            Node::EmptyLine => {
                self.buf.push('\n');
                self.new_line();
            }
            Node::Line | Node::Empty => {}
        }
    }

    fn text(&mut self, value: &str) {
        self.line_len += value.chars().count();
        self.buf.push_str(value);
    }

    fn new_line(&mut self) {
        self.line_len = INDENT.len() * self.indents;
        self.buf.push('\n');

        if self.pending_indents > 0 {
            self.line_len += INDENT.len();
            self.indents += 1;
            self.pending_indents -= 1;
        }

        for _ in 0..self.indents {
            self.buf.push_str(INDENT);
        }
    }

    pub fn into_string(self) -> String {
        self.buf
    }
}

fn ident_is_special(ident: &str) -> bool {
    ident.is_empty()
        || matches!(ident.chars().next(), Some('0'..='9'))
        || ident
            .chars()
            .any(|v| !matches!(v, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '$' | '#'))
}

pub struct Builder {
    group_id: Cell<usize>,
}

impl Builder {
    pub const fn new() -> Self {
        Self {
            group_id: Cell::new(0),
        }
    }

    pub fn next_id(&self) -> usize {
        let id = self.group_id.get();
        self.group_id.set(id + 1);
        id
    }

    pub fn string(&self, s: &str) -> Node {
        Node::group(
            self.next_id(),
            [
                Node::static_text("\""),
                Node::text(format!("{}", s.escape_debug())),
                Node::static_text("\""),
            ],
        )
    }

    pub fn ident(&self, s: &str) -> Node {
        if ident_is_special(s) {
            Node::group(
                self.next_id(),
                [
                    Node::static_text("`"),
                    Node::text(format!("{}", s.escape_debug())),
                    Node::static_text("`"),
                ],
            )
        } else {
            Node::text(s)
        }
    }

    pub fn generate_statement(&self, statement: &Statement) -> Node {
        match statement {
            Statement::If {
                condition,
                if_stmt,
                else_stmt,
                span,
                annotations,
            } => todo!(),
            Statement::While {
                condition,
                child,
                span,
                annotations,
            } => todo!(),
            Statement::For {
                iterator,
                var_name,
                child,
                span,
                annotations,
            } => todo!(),
            Statement::Return(None, _) => Node::static_text("return;"),
            Statement::Return(expression, _) => todo!(),
            Statement::Block(statements, _, annotations) => {
                if statements.is_empty() {
                    return Node::group(
                        self.next_id(),
                        [
                            self.generate_annotations(annotations),
                            Node::static_text("{}"),
                        ],
                    );
                }
                Node::group(
                    self.next_id(),
                    [
                        Node::static_text("{"),
                        Node::HardLine,
                        Node::indent(
                            statements
                                .iter()
                                .map(|v| Node::nodes([self.generate_statement(v), Node::HardLine]))
                                .collect::<Box<[_]>>(),
                        ),
                        Node::static_text("}"),
                    ],
                )
            }
            Statement::Var(interned_str, expression, type_ref, span, annotations) => todo!(),
            Statement::Expression(expression) => todo!(),
            Statement::Function(function_contract, statement, _) => {
                if matches!(&**statement, Statement::Expression(_)) {
                    Node::group(
                        self.next_id(),
                        [
                            self.generate_func(function_contract),
                            Node::static_text(" = "),
                            self.generate_statement(statement),
                        ],
                    )
                } else {
                    Node::group(
                        self.next_id(),
                        [
                            self.generate_func(function_contract),
                            Node::static_text(" "),
                            self.generate_statement(statement),
                        ],
                    )
                }
            }
            Statement::ExternalFunction(function_contract, statement, span) => todo!(),
            Statement::Struct {
                name,
                elements,
                span,
                global_impl,
                impls,
                generics,
                annotations,
            } => todo!(),
            Statement::Trait(_) => todo!(),
            Statement::Export(left, right, _) if left == right => Node::nodes([
                Node::static_text("export "),
                Node::text(**left),
                Node::static_text(";"),
            ]),
            Statement::Export(left, right, _) => Node::nodes([
                Node::static_text("export "),
                Node::text(**left),
                Node::static_text(" as "),
                Node::text(**right),
                Node::static_text(";"),
            ]),
            Statement::ModuleAsm(_, s) => {
                if !s.contains('\n') {
                    Node::nodes([
                        Node::static_text("asm("),
                        self.string(s),
                        Node::static_text(")"),
                    ])
                } else {
                    let mut nodes = vec![Node::static_text("asm("), Node::HardLine];
                    for line in s.lines() {
                        nodes.push(Node::static_text("    "));
                        nodes.push(self.string(line));
                        nodes.push(Node::static_text(","));
                        nodes.push(Node::HardLine);
                    }
                    nodes.push(Node::StaticText(")"));
                    Node::nodes(nodes)
                }
            }
            Statement::BakedFunction(..)
            | Statement::BakedExternalFunction(..)
            | Statement::BakedStruct(..)
            | Statement::BakedStatic(..)
            | Statement::BakedTrait(..) => unreachable!(),
        }
    }

    fn generate_annotations(&self, annotations: &Annotations) -> Node {
        let mut vec = vec![];
        for annotation in annotations.iter() {
            vec.push(Node::text(format!("{annotation}")));
            vec.push(Node::HardLine);
        }
        Node::zero_sized(vec)
    }

    fn generate_func(&self, contract: &FunctionContract) -> Node {
        let args = contract.arguments.iter().map(|v| {
            Node::nodes([
                self.generate_annotations(&contract.annotations),
                self.ident(&v.name),
                Node::static_text(": "),
                self.type_(&v.typ),
            ])
        });
        let args = self.generate_list_like(PARENS.clone(), Node::static_text(","), args);
        let mut nodes = vec![self.generate_annotations(&contract.annotations)];
        if let Some(name) = contract.name {
            nodes.push(Node::static_text("fn "));
            nodes.push(self.ident(&name));
        } else {
            nodes.push(Node::static_text("fn"));
        }
        nodes.push(args);
        if !is_void(&contract.return_type) {
            nodes.push(Node::static_text(" -> "));
            nodes.push(self.type_(&contract.return_type));
        }
        Node::group(self.next_id(), nodes)
    }

    pub fn generate_list_like(
        &self,
        brackets: (Node, Node),
        seperator: Node,
        elements: impl IntoIterator<Item = Node>,
    ) -> Node {
        let id = self.next_id();
        let mut args = elements.into_iter();
        let Some(mut arg) = args.next() else {
            return Node::group(id, [brackets.0, brackets.1]);
        };

        let mut vals = Vec::with_capacity(args.size_hint().0 + 1);
        for next_arg in args {
            let arg = std::mem::replace(&mut arg, next_arg);
            vals.push(Node::nodes([arg, seperator.clone(), Node::SpaceOrLine]));
        }
        vals.push(Node::nodes([
            arg,
            Node::if_wrap(id, seperator, Node::Empty),
        ]));

        Node::group(
            id,
            [
                brackets.0,
                Node::Line,
                Node::indent(vals),
                Node::Line,
                brackets.1,
            ],
        )
    }

    fn path(&self, path: &MPath) -> Node {
        let mut nodes = vec![];

        for (name, _, generics) in &path.entries {
            if !nodes.is_empty() {
                nodes.push(Node::static_text("::"));
            }
            nodes.push(Node::text(**name));
            if !generics.is_empty() {
                nodes.push(self.generate_list_like(
                    ANGLE_BRACKETS.clone(),
                    COMMA.clone(),
                    generics.iter().map(|typ| self.type_(typ)),
                ));
            }
        }
        Node::nodes(nodes)
    }

    fn path_simple(&self, path: &PathWithoutGenerics<'_>) -> Node {
        let mut nodes = vec![];

        for name in &path.entries {
            if !nodes.is_empty() {
                nodes.push(Node::static_text("::"));
            }
            nodes.push(Node::text(**name));
        }
        Node::nodes(nodes)
    }

    fn generate_refcount(&self, refcount: u8) -> Node {
        let mut s = String::new();
        for _ in 0..refcount {
            s.push('&');
        }
        Node::text(s)
    }

    fn type_(&self, typ: &TypeRef<'_>) -> Node {
        match typ {
            TypeRef::DynReference {
                num_references,
                traits,
                ..
            } => {
                let mut nodes = vec![
                    self.generate_refcount(*num_references),
                    Node::static_text("dyn "),
                ];
                for (i, r#trait) in traits.iter().enumerate() {
                    if i != 0 {
                        nodes.push(Node::SpaceOrLine);
                        nodes.push(Node::StaticText("+ "));
                    }
                    nodes.push(self.path_simple(r#trait));
                }
                Node::indent_next(nodes)
            }
            TypeRef::Reference {
                num_references,
                type_name,
                ..
            } => Node::nodes([
                self.generate_refcount(*num_references),
                self.path(type_name),
            ]),
            TypeRef::Void(_, 0) => Node::StaticText("void"),
            TypeRef::Void(_, refcount) => {
                let mut s = String::new();
                for _ in 0..*refcount {
                    s.push('&');
                }
                s.push_str("void");
                Node::text(s)
            }
            TypeRef::Never(_) => Node::static_text("!"),
            TypeRef::UnsizedArray {
                num_references,
                child,
                ..
            } => Node::nodes([
                self.generate_refcount(*num_references),
                Node::static_text("["),
                self.type_(child),
                Node::static_text("]"),
            ]),
            TypeRef::SizedArray {
                num_references,
                child,
                number_elements,
                ..
            } => Node::nodes([
                self.generate_refcount(*num_references),
                Node::static_text("["),
                self.type_(child),
                Node::static_text("; "),
                Node::text(format!("{number_elements}")),
                Node::static_text("]"),
            ]),
            TypeRef::Function {
                return_ty,
                args,
                num_references,
                ..
            } => {
                if is_void(return_ty) {
                    Node::nodes([
                        self.generate_refcount(*num_references),
                        Node::static_text("fn"),
                        self.generate_list_like(
                            PARENS.clone(),
                            COMMA.clone(),
                            args.iter().map(|typ| self.type_(typ)),
                        ),
                    ])
                } else {
                    Node::nodes([
                        self.generate_refcount(*num_references),
                        Node::static_text("fn"),
                        self.generate_list_like(
                            PARENS.clone(),
                            COMMA.clone(),
                            args.iter().map(|typ| self.type_(typ)),
                        ),
                        Node::static_text(" -> "),
                        self.type_(return_ty),
                    ])
                }
            }
            TypeRef::Tuple {
                num_references,
                elements,
                ..
            } => Node::nodes([
                self.generate_refcount(*num_references),
                self.generate_list_like(
                    PARENS.clone(),
                    COMMA.clone(),
                    elements.iter().map(|typ| self.type_(typ)),
                ),
            ]),
        }
    }

    // pub fn func_call(&self, ident: &str, args: impl IntoIterator<Item = Node>) -> Node {
    //     let id = self.next_id();
    //     let mut args = args.into_iter();
    //     let Some(mut arg) = args.next() else {
    //         return Node::group(id, [self.ident(ident), Node::static_text("()")]);
    //     };
    //
    //     let mut vals = Vec::with_capacity(args.size_hint().0 + 1);
    //     for next_arg in args {
    //         let arg = std::mem::replace(&mut arg, next_arg);
    //         vals.push(Node::nodes([
    //             arg,
    //             Node::static_text(","),
    //             Node::SpaceOrLine,
    //         ]));
    //     }
    //     vals.push(Node::nodes([
    //         arg,
    //         Node::if_wrap(id, Node::static_text(","), Node::Empty),
    //     ]));
    //
    //     Node::group(
    //         id,
    //         [
    //             self.ident(ident),
    //             Node::group(
    //                 self.next_id(),
    //                 [
    //                     Node::static_text("("),
    //                     Node::Line,
    //                     Node::indent(vals),
    //                     Node::Line,
    //                     Node::static_text(")"),
    //                 ],
    //             ),
    //         ],
    //     )
    // }
}

fn is_void(ty: &TypeRef) -> bool {
    match ty {
        TypeRef::Void(_, 0) => true,
        TypeRef::Reference {
            num_references: 0,
            type_name,
            ..
        } if type_name.entries.len() == 1
            && type_name.entries[0].2.is_empty()
            && &**type_name.entries[0].0 == "void" =>
        {
            true
        }
        _ => false,
    }
}

impl Default for Builder {
    fn default() -> Self {
        Self::new()
    }
}

const ANGLE_BRACKETS: (Node, Node) = (Node::static_text("<"), Node::static_text(">"));
const PARENS: (Node, Node) = (Node::static_text("("), Node::static_text(")"));
const COMMA: Node = Node::static_text(",");

const INDENT: &str = "    ";
