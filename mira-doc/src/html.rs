use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Arguments, Display, Write},
    path::{Component, Path, PathBuf},
    rc::Rc,
    sync::Arc,
};

use mira_common::store::StoreKey;
use mira_errors::Diagnostic;
use mira_parser::{
    annotations::Annotations,
    module::{ExternalFunction, Function, ModuleScopeValue},
};
use mira_spans::Symbol;
use mira_typeck::{
    Ty, TyKind, TypeCtx, TypecheckingContext, TypedExternalFunction, TypedFunction, TypedModule,
    TypedStatic, TypedStruct, TypedTrait, default_types,
};

pub mod err_fs {
    use std::{fs, path::PathBuf, result::Result};

    use mira_errors::{Diagnostic, IoCreateDirError, IoWriteError};

    pub fn create_dir_all(dir: PathBuf) -> Result<PathBuf, Diagnostic<'static>> {
        match fs::create_dir_all(&dir) {
            Ok(_) => Ok(dir),
            Err(e) => Err(IoCreateDirError(dir, e).to_error()),
        }
    }

    pub fn write_file(
        file: PathBuf,
        data: impl AsRef<[u8]>,
    ) -> Result<PathBuf, Diagnostic<'static>> {
        match fs::write(&file, data) {
            Ok(_) => Ok(file),
            Err(e) => Err(IoWriteError(file, e).to_error()),
        }
    }
}

static JS: &str = include_str!("../mock/index.js");
static CSS: &str = include_str!("../mock/index.css");
static NOSCRIPT_CSS: &str = include_str!("../mock/noscript.css");

/// the path to this item's html file, gets the full qualified path (e.g.
/// std::intrinsics::type_name) and the list of modules to get there (e.g. std::intrinsics).
/// Note that if you put in a module however, the module path does *not* contain that module
/// (e.g. the result for the intrinsics module in std would be `$root/std/intrinsics/index.html`, ["std", "intrinsics"], [module<std>])
type QualifiedPaths<'ctx> = (PathBuf, Vec<Symbol<'ctx>>, Vec<StoreKey<TypedModule<'ctx>>>);

pub struct HTMLGenerateContext<'ctx> {
    pub path: PathBuf,
    pub generated: RefCell<HashMap<ModuleScopeValue<'ctx>, Rc<QualifiedPaths<'ctx>>>>,
    pub queued: RefCell<HashMap<ModuleScopeValue<'ctx>, Rc<QualifiedPaths<'ctx>>>>,
    pub tc_ctx: Arc<TypecheckingContext<'ctx>>,
    modules: Vec<StoreKey<TypedModule<'ctx>>>,
}

impl<'ctx> HTMLGenerateContext<'ctx> {
    pub fn new(
        dir: PathBuf,
        tc_ctx: Arc<TypecheckingContext<'ctx>>,
        modules: Vec<StoreKey<TypedModule<'ctx>>>,
    ) -> Result<Self, Diagnostic<'static>> {
        let dir = err_fs::create_dir_all(dir)?;
        err_fs::write_file(dir.join("index.js"), JS)?;
        err_fs::write_file(dir.join("index.css"), CSS)?;
        err_fs::write_file(dir.join("noscript.css"), NOSCRIPT_CSS)?;
        let me = Self {
            path: dir,
            generated: Default::default(),
            queued: Default::default(),
            tc_ctx,
            modules,
        };
        for module in &me.modules {
            me.ensure_exists(ModuleScopeValue::Module(module.cast()));
        }
        Ok(me)
    }

    /// gets the path to this item's html file, gets the full qualified path (e.g.
    /// std::intrinsics::type_name) and the list of modules to get there (e.g. std::intrinsics).
    /// Note that if you put in a module however, the module path does *not* contain that module
    /// (e.g. the result for the intrinsics module in std would be `$root/std/intrinsics/index.html`, ["std", "intrinsics"], [module<std>])
    pub fn get_path(&self, mut item: ModuleScopeValue<'ctx>) -> QualifiedPaths<'ctx> {
        let mut symbol_path = Vec::new();
        let mut module_path = Vec::new();
        let is_module = matches!(item, ModuleScopeValue::Module(_));
        loop {
            let (name, parent_module) = match item {
                ModuleScopeValue::Function(key) => {
                    let res = &self.tc_ctx.functions.read()[key.cast()];
                    // this should never be an anonymous function, as those can only be inside
                    // expressions, which miradoc doesn't care about.
                    (res.0.name.unwrap().symbol(), res.0.module_id)
                }
                ModuleScopeValue::ExternalFunction(key) => {
                    let res = &self.tc_ctx.external_functions.read()[key.cast()];
                    // extern fn can't be anonymous
                    (res.0.name.unwrap().symbol(), res.0.module_id)
                }
                ModuleScopeValue::Struct(key) => {
                    let res = &self.tc_ctx.structs.read()[key.cast()];
                    (res.name.symbol(), res.module_id)
                }
                ModuleScopeValue::Static(key) => {
                    let res = &self.tc_ctx.statics.read()[key.cast()];
                    (res.name.symbol(), res.module_id)
                }
                ModuleScopeValue::Trait(key) => {
                    let res = &self.tc_ctx.traits.read()[key.cast()];
                    (res.name.symbol(), res.module_id)
                }
                ModuleScopeValue::Module(key) => {
                    let res = &self.tc_ctx.modules.read()[key.cast()];
                    if res.parent.is_undefined() {
                        symbol_path.push(res.name);
                        break;
                    }
                    (res.name, res.parent.cast())
                }
            };
            symbol_path.push(name);
            module_path.push(parent_module);
            item = ModuleScopeValue::Module(parent_module.cast());
        }
        symbol_path.reverse();
        module_path.reverse();
        let mut path = self.path.clone();
        for sym in &symbol_path {
            path.push(sym.to_str());
        }
        if is_module {
            path.push("index.html");
        } else {
            path.set_extension("html");
        }
        (path, symbol_path, module_path)
    }

    pub fn ensure_exists(&self, item: ModuleScopeValue<'ctx>) {
        if self.generated.borrow().contains_key(&item) || self.queued.borrow().contains_key(&item) {
            return;
        }
        let fully_qualified_path = Rc::new(self.get_path(item));
        self.queued
            .borrow_mut()
            .insert(item, Rc::clone(&fully_qualified_path));
        for path in fully_qualified_path.2.iter() {
            self.ensure_exists(ModuleScopeValue::Module(path.cast()));
        }
    }

    pub fn get_item_path(&self, item: ModuleScopeValue<'ctx>, my_path: &Path) -> String {
        let generated = self.generated.borrow();
        let mut queued = self.queued.borrow();
        let path = match generated.get(&item).or_else(|| queued.get(&item)) {
            Some(v) => v,
            None => {
                drop(generated);
                drop(queued);
                let fully_qualified_path = self.get_path(item);
                self.queued
                    .borrow_mut()
                    .insert(item, fully_qualified_path.into());
                queued = self.queued.borrow();
                queued.get(&item).unwrap()
            }
        };

        let my_path = my_path.strip_prefix(&self.path).unwrap();
        let mut s = String::new();
        // we have to skip the first normal, because ../ in a/b/c.html goes to  a/ instead of
        // b/, so we have to remove the last normal, but because we ignore the contents of the
        // prefix, the first can act as the last.
        let mut encountered_component = false;
        for c in my_path.components() {
            match c {
                Component::Prefix(_) | Component::RootDir | Component::ParentDir => {
                    unreachable!()
                }
                Component::CurDir => {}
                Component::Normal(_) if !encountered_component => encountered_component = true,
                Component::Normal(_) => s.push_str("../"),
            }
        }
        for sym in &path.1 {
            s.push_str(sym.to_str());
            s.push('/');
        }
        match item {
            // a::b turns into a/b/index.html if b is a module.
            ModuleScopeValue::Module(_) => s.push_str("index.html"),
            _ => {
                // a::b turns into a/b/. remove the last character for a/b and add .html for
                // a/b.html.
                s.pop();
                s.push_str(".html");
            }
        }
        s
    }

    pub fn generate_css_js_links(&self, output: &mut String, my_path: &Path) {
        let my_path = my_path.strip_prefix(&self.path).unwrap();
        let mut s = String::new();
        // we have to skip the first normal, because ../ in a/b/c.html goes to  a/ instead of
        // b/, so we have to remove the last normal, but because we ignore the contents of the
        // prefix, the first can act as the last.
        let mut encountered_component = false;
        for c in my_path.components() {
            match c {
                Component::Prefix(_) | Component::RootDir | Component::ParentDir => {
                    unreachable!()
                }
                Component::CurDir => {}
                Component::Normal(_) if !encountered_component => encountered_component = true,
                Component::Normal(_) => s.push_str("../"),
            }
        }

        // <script>const root = "./";</script>
        // <link rel="stylesheet" href="./index.css">
        // <noscript><link rel="stylesheet" href="./noscript.css"></noscript>
        // <script src="./index.js"></script>
        let len = s.len();
        output.push_str("<script>const root = ");
        output.push_fmt(format_args!("{:?}", if s.is_empty() { "./" } else { &s }));
        output.push_str(";</script>");
        output.push_str(r#"<link rel="stylesheet" href="#);
        s.push_str("index.css");
        output.push_fmt(format_args!("\"{}\"", urlencode(&s)));
        output.push_str(r#"><noscript><link rel="stylesheet" href="#);
        s.truncate(len);
        s.push_str("noscript.css");
        output.push_fmt(format_args!("\"{}\"", urlencode(&s)));
        output.push_str(r#"></noscript><script src="#);
        s.truncate(len);
        s.push_str("index.js");
        output.push_fmt(format_args!("\"{}\"", urlencode(&s)));
        output.push_str("></script>");
    }

    pub fn generate_file_start(&self, curfile: &Path, item_name: &str) -> String {
        let mut s = HTML_HEAD.to_string();
        s.push_str("<title>");
        s.escaped().push_str(item_name);
        s.push_str(" - mira");
        s.push_str("</title>");
        self.generate_css_js_links(&mut s, curfile);
        s.push_str(HTML_PREAMBLE1);
        let reader = self.tc_ctx.modules.read();
        // <a href="#">root</a>
        for &key in &self.modules {
            let path = self.get_item_path(ModuleScopeValue::Module(key.cast()), curfile);
            s.push_str("<a href=\"");
            s.push_fmt(format_args!("{}", urlencode(&path)));
            s.push_str("\">");
            s.push_str(reader[key.cast()].name.to_str());
            s.push_str("</a>");
        }

        s.push_str(HTML_PREAMBLE2);
        s
    }

    pub fn generate_header(
        &self,
        output: &mut String,
        path: &QualifiedPaths<'ctx>,
        item: ModuleScopeValue<'ctx>,
    ) {
        let (my_path, sym_path, path) = path;
        // <div class="path"><a href="#">std</a><span>::</span><a href="#">intrinsics</a><span>::</span><a href="#">type_name</a></div>
        // <h1>function <span class="function">type_name</span></h1>
        output.push_str(r#"<div class="path">"#);
        for (i, sym) in sym_path[..sym_path.len() - 1].iter().enumerate() {
            if i != 0 {
                output.push_str("<span>::</span>");
            }
            output.push_str("<a href=");
            output.push_fmt(format_args!(
                "\"{}\"",
                urlencode(&self.get_item_path(ModuleScopeValue::Module(path[i].cast()), my_path))
            ));
            output.push('>');
            output.escaped().push_str(sym.to_str());
            output.push_str("</a>");
        }
        output.push_str("</div>");
        output.push_str("<h1>");
        let ty = item_ty(item);
        output.push_str(ty);
        output.push_str(" <span class=\"");
        output.push_str(ty);
        output.push_str("\">");
        output.escaped().push_str(sym_path.last().unwrap().to_str());
        output.push_str("</span></h1>");
    }

    pub fn generate_annotations(annotations: &Annotations<'_>, s: &mut String) {
        for annotation in annotations.iter() {
            s.escaped().push_fmt(format_args!("{annotation}"));
            s.push('\n');
        }
    }

    pub fn generate_function(
        &mut self,
        fully_qualified_path: &QualifiedPaths<'ctx>,
        key: StoreKey<TypedFunction<'ctx>>,
    ) -> String {
        let curfile = &fully_qualified_path.0;
        let contract = &self.tc_ctx.functions.read()[key].0;
        let mut s = self.generate_file_start(curfile, contract.name.unwrap().symbol().to_str());

        self.generate_header(
            &mut s,
            fully_qualified_path,
            ModuleScopeValue::Function(key.cast()),
        );
        // <pre class="item-decl code"><code>fn type_name&lt;unsized T&gt;() -&gt; &amp;str { .. }</code></pre>
        s.push_str(r#"<pre class="item-decl code"><code>"#);
        Self::generate_annotations(&contract.annotations, &mut s);
        s.push_str("fn ");
        s.escaped()
            .push_str(contract.name.unwrap().symbol().to_str());
        if !contract.generics.is_empty() {
            s.push_str("&lt;");
            for (i, generic) in contract.generics.iter().enumerate() {
                if i != 0 {
                    s.push_str(", ");
                }
                if !generic.sized {
                    s.push_str("unsized ");
                }
                s.escaped().push_str(&generic.name);
                // TODO: bounds
            }
            s.push_str("&gt;");
        }
        s.push('(');
        for (i, arg) in contract.arguments.iter().enumerate() {
            if i != 0 {
                s.push_str(", ");
            }
            s.escaped().push_str(arg.0.symbol().to_str());
            s.push_str(": ");
            self.write_ty(&mut s, arg.1, curfile);
        }
        s.push(')');
        if contract.return_type != default_types::void {
            s.push_str(" -> ");
            self.write_ty(&mut s, contract.return_type, curfile);
        }
        s.push_str(" { .. }</code></pre>");
        self.generate_doc_comment(contract.comment, &mut s);

        s.push_str(HTML_POSTAMBLE);
        s
    }

    pub fn generate_external_function(
        &mut self,
        fully_qualified_path: &QualifiedPaths<'ctx>,
        key: StoreKey<TypedExternalFunction<'ctx>>,
    ) -> String {
        let curfile = &fully_qualified_path.0;
        let contract = &self.tc_ctx.external_functions.read()[key].0;
        let mut s = self.generate_file_start(curfile, contract.name.unwrap().symbol().to_str());

        self.generate_header(
            &mut s,
            fully_qualified_path,
            ModuleScopeValue::ExternalFunction(key.cast()),
        );
        // <pre class="item-decl code"><code>fn type_name&lt;unsized T&gt;() -&gt; &amp;str { .. }</code></pre>
        s.push_str(r#"<pre class="item-decl code"><code>"#);
        Self::generate_annotations(&contract.annotations, &mut s);
        s.push_str("extern fn ");
        s.escaped()
            .push_str(contract.name.unwrap().symbol().to_str());
        s.push('(');
        for (i, arg) in contract.arguments.iter().enumerate() {
            if i != 0 {
                s.push_str(", ");
            }
            s.escaped().push_str(arg.0.symbol().to_str());
            s.push_str(": ");
            self.write_ty(&mut s, arg.1, curfile);
        }
        s.push(')');
        if contract.return_type != default_types::void {
            s.push_str(" -> ");
            self.write_ty(&mut s, contract.return_type, curfile);
        }
        s.push_str(" { .. }</code></pre>");
        self.generate_doc_comment(contract.comment, &mut s);

        s.push_str(HTML_POSTAMBLE);
        s
    }

    pub fn generate_static(
        &mut self,
        fully_qualified_path: &QualifiedPaths<'ctx>,
        key: StoreKey<TypedStatic<'ctx>>,
    ) -> String {
        let curfile = &fully_qualified_path.0;
        let v = &self.tc_ctx.statics.read()[key];
        let mut s = self.generate_file_start(curfile, v.name.symbol().to_str());

        self.generate_header(
            &mut s,
            fully_qualified_path,
            ModuleScopeValue::Static(key.cast()),
        );
        // <pre class="item-decl code"><code>fn type_name&lt;unsized T&gt;() -&gt; &amp;str { .. }</code></pre>
        s.push_str(r#"<pre class="item-decl code"><code>"#);
        Self::generate_annotations(&v.annotations, &mut s);
        s.push_str("let ");
        s.escaped().push_str(v.name.symbol().to_str());
        s.push_str(": ");
        self.write_ty(&mut s, v.type_, curfile);
        s.push_str("</code></pre>");
        self.generate_doc_comment(v.comment, &mut s);

        s.push_str(HTML_POSTAMBLE);
        s
    }

    pub fn generate_trait(
        &mut self,
        fully_qualified_path: &QualifiedPaths<'ctx>,
        key: StoreKey<TypedTrait<'ctx>>,
    ) -> String {
        let curfile = &fully_qualified_path.0;
        let trait_ = &self.tc_ctx.traits.read()[key];
        let mut s = self.generate_file_start(curfile, trait_.name.symbol().to_str());

        self.generate_header(
            &mut s,
            fully_qualified_path,
            ModuleScopeValue::Trait(key.cast()),
        );
        // <pre class="item-decl code"><code>fn type_name&lt;unsized T&gt;() -&gt; &amp;str { .. }</code></pre>
        s.push_str(r#"<pre class="item-decl code"><code>"#);
        Self::generate_annotations(&trait_.annotations, &mut s);
        s.push_str("trait ");
        s.escaped().push_str(trait_.name.symbol().to_str());
        s.push_str(" { .. }");
        s.push_str("</code></pre>");
        self.generate_doc_comment(trait_.comment, &mut s);

        s.push_str(r##"<h2 id="functions" class="anchorable header">Functions<a class="anchor" href="#functions">§</a></h2>"##);
        // <span id="function.allocate" class="anchorable member-function"><code>fn <a class="function" href="#function.allocate">allocate</a>(self: &amp;<span class="struct">Self</span>, size: <span class="struct">usize</span>, align: <span class="struct">usize</span>) -&gt; &amp;<span class="struct">void</span></code><a href="#function.allocate" class="anchor">§</a></span>
        for (name, args, ret_ty, _, _, comment) in &trait_.functions {
            s.push_str("<span id=\"function.");
            s.escaped().push_str(name.symbol().to_str());
            s.push_str(
                "\" class=\"anchorable member-function\"><code>fn <a class=\"function\" href=\"#function.",
            );
            s.push_fmt(format_args!("{}", urlencode(name.symbol().to_str())));
            s.push_str("\">");
            s.escaped().push_str(name.symbol().to_str());
            s.push_str("</a>");
            s.push('(');
            for (i, (arg, arg_ty)) in args.iter().enumerate() {
                if i != 0 {
                    s.push_str(", ");
                }
                s.escaped().push_str(arg);
                s.push_str(": ");
                self.write_ty(&mut s, *arg_ty, curfile);
            }
            s.push(')');

            if *ret_ty != default_types::void {
                s.push_str(" -&gt; ");
                self.write_ty(&mut s, *ret_ty, curfile);
            }

            s.push_str("</code><a href=\"#function.");
            s.push_fmt(format_args!("{}", urlencode(name.symbol().to_str())));
            s.push_str("\" class=\"anchor\">§</a></span>");

            self.generate_doc_comment(*comment, &mut s);
        }

        s.push_str(HTML_POSTAMBLE);
        s
    }

    pub fn generate_struct(
        &mut self,
        fully_qualified_path: &QualifiedPaths<'ctx>,
        key: StoreKey<TypedStruct<'ctx>>,
    ) -> String {
        let curfile = &fully_qualified_path.0;
        let structure = &self.tc_ctx.structs.read()[key];
        let mut s = self.generate_file_start(curfile, structure.name.symbol().to_str());

        self.generate_header(
            &mut s,
            fully_qualified_path,
            ModuleScopeValue::Struct(key.cast()),
        );
        // <pre class="item-decl code"><code>fn type_name&lt;unsized T&gt;() -&gt; &amp;str { .. }</code></pre>
        s.push_str(r#"<pre class="item-decl code"><code>"#);
        Self::generate_annotations(&structure.annotations, &mut s);
        s.push_str("struct ");
        s.escaped().push_str(structure.name.symbol().to_str());
        if structure.elements.is_empty() {
            s.push_str(" {}");
        } else {
            s.push_str(" {");
            for (name, ty, _) in structure.elements.iter() {
                s.push_str("\n    ");
                s.escaped().push_str(name.symbol().to_str());
                s.push_str(": ");
                self.write_ty(&mut s, *ty, curfile);
                s.push(',');
            }
            s.push_str("\n}");
        }
        s.push_str("</code></pre>");
        self.generate_doc_comment(structure.comment, &mut s);

        // <h2 id="fields" class="anchorable header">Fields<a class="anchor" href="#fields">§</a></h2>
        if !structure.elements.is_empty() {
            s.push_str(r##"<h2 id="fields" class="anchorable header">Fields<a class="anchor" href="#fields">§</a></h2>"##);
        }

        // <span id="structfield.field1" class="anchorable structfield"><code>field1: <a class="struct" href="#">u32</a></code><a href="#structfield.field1" class="anchor">§</a></span>
        for (name, ty, comment) in structure.elements.iter() {
            s.push_str("<span id=\"structfield.");
            s.escaped().push_str(name.symbol().to_str());
            s.push_str("\" class=\"anchorable structfield\"><code>");
            s.escaped().push_str(name.symbol().to_str());
            s.push_str(": ");
            self.write_ty(&mut s, *ty, curfile);
            s.push_str("</code><a href=\"#structfield.");
            s.push_fmt(format_args!("{}", urlencode(name.symbol().to_str())));
            s.push_str("\" class=\"anchor\">§</a></span>");

            self.generate_doc_comment(*comment, &mut s);
        }

        // <h2 id="functions" class="anchorable header">Functions<a class="anchor" href="#functions">§</a></h2>
        if !structure.global_impl.is_empty() {
            s.push_str(r##"<h2 id="functions" class="anchorable header">Functions<a class="anchor" href="#functions">§</a></h2>"##);
        }

        let selfty = self.tc_ctx.ctx.intern_ty(TyKind::Struct {
            struct_id: key,
            name: structure.name,
        });
        // <span id="function.len" class="anchorable member-function"><code>fn <a class="function" href="#function.len">len</a>(self: &amp;<span class="struct">Self</span>) -&gt; <span class="struct">usize</span></code><a href="#function.len" class="anchor">§</a></span>
        self.generate_struct_impl(
            curfile,
            &mut s,
            structure.global_impl.values().copied(),
            "function.",
            selfty,
        );

        // <h2 id="traits" class="anchorable header">Traits<a class="anchor" href="#traits">§</a></h2>
        // <h3 id="traits.Clone" class="anchorable header">Clone<a class="anchor" href="#traits.Clone">§</a></h3>
        // <span id="traitfunction.clone" class="anchorable member-function"><code>fn <a class="function" href="#traitfunction.Clone.clone">clone</a>(self: &amp;<span class="struct">Self</span>) -&gt; <span class="struct">Self</span></code><a href="#traitfunction.Clone.clone" class="anchor">§</a></span>
        if !structure.trait_impl.is_empty() {
            s.push_str(r##"<h2 id="traits" class="anchorable header">Traits<a class="anchor" href="#traits">§</a></h2>"##);
        }

        let traits = self.tc_ctx.traits.read();
        for (traitid, implementation) in structure.trait_impl.iter() {
            let trait_ = &traits[traitid];
            s.push_str("<h3 id=\"traits.");
            s.escaped().push_str(trait_.name.symbol().to_str());
            s.push_str("\" class=\"anchorable header\">impl <a href=\"");
            let path = self.get_item_path(ModuleScopeValue::Trait(traitid.cast()), curfile);
            s.push_fmt(format_args!("{}", urlencode(&path)));
            s.push_str("\" class=\"trait\">");
            s.escaped().push_str(trait_.name.symbol().to_str());
            s.push_str("</a><a class=\"anchor\" href=\"#traits.");
            s.push_fmt(format_args!("{}", urlencode(trait_.name.symbol().to_str())));
            s.push_str("\">§</a></h3>");

            let prefix = format!(
                "traitfunction.{}.",
                urlencode(trait_.name.symbol().to_str())
            );
            self.generate_struct_impl(
                curfile,
                &mut s,
                implementation.iter().copied(),
                &prefix,
                selfty,
            );
        }

        s.push_str(HTML_POSTAMBLE);
        s
    }

    fn generate_struct_impl(
        &self,
        curfile: &Path,
        s: &mut String,
        implementation: impl Iterator<Item = StoreKey<TypedFunction<'ctx>>>,
        function_prefix: &str,
        selfty: Ty<'ctx>,
    ) {
        let funcs = self.tc_ctx.functions.read();
        for id in implementation {
            let contract = &funcs[id].0;
            let name = contract.name.unwrap().symbol().to_str();
            s.push_str("<span id=\"");
            s.push_str(function_prefix);
            s.escaped().push_str(name);
            s.push_str(
                "\" class=\"anchorable member-function\"><code>fn <a class=\"function\"  href=\"#",
            );
            s.push_str(function_prefix);
            s.push_fmt(format_args!("{}", urlencode(name)));
            s.push_str("\">");
            s.escaped().push_str(name);
            s.push_str("</a>(");
            for (i, (name, ty)) in contract.arguments.iter().enumerate() {
                if i != 0 {
                    s.push_str(", ");
                }
                s.escaped().push_str(name.symbol().to_str());
                s.push_str(": ");
                self.write_ty(s, unsubstitute_self(self.tc_ctx.ctx, *ty, selfty), curfile);
            }
            s.push(')');
            if contract.return_type != default_types::void {
                s.push_str(" -&gt; ");
                self.write_ty(
                    s,
                    unsubstitute_self(self.tc_ctx.ctx, contract.return_type, selfty),
                    curfile,
                );
            }
            s.push_str("</code><a href=\"#");
            s.push_str(function_prefix);
            s.push_fmt(format_args!("{}", urlencode(name)));
            s.push_str("\" class=\"anchor\">§</a></span>");

            self.generate_doc_comment(contract.comment, s);
        }
    }

    pub fn generate_module(
        &mut self,
        fully_qualified_path: &QualifiedPaths<'ctx>,
        key: StoreKey<TypedModule<'ctx>>,
    ) -> String {
        let curfile = &fully_qualified_path.0;
        let module = &self.tc_ctx.modules.read()[key.cast()];
        let mut s = self.generate_file_start(curfile, module.name.to_str());

        self.generate_header(
            &mut s,
            fully_qualified_path,
            ModuleScopeValue::Module(key.cast()),
        );

        self.generate_doc_comment(module.comment, &mut s);

        let mut modules = vec![];
        let mut functions = vec![];
        let mut structs = vec![];
        let mut traits = vec![];
        let mut statics = vec![];

        for export in module.exports.iter() {
            match module.scope[export] {
                ModuleScopeValue::Function(key) => functions.push(FnId::Normal(key)),
                ModuleScopeValue::ExternalFunction(key) => functions.push(FnId::Extern(key)),
                ModuleScopeValue::Struct(key) => structs.push(key),
                ModuleScopeValue::Static(key) => statics.push(key),
                ModuleScopeValue::Module(key) => modules.push(key),
                ModuleScopeValue::Trait(key) => traits.push(key),
            }
        }

        if !modules.is_empty() {
            s.push_str(r##"<h2 id="modules" class="anchorable header">Modules<a class="anchor" href="#modules">§</a></h2>"##);
        }
        for key in modules {
            self.write_reference(ModuleScopeValue::Module(key.cast()), &mut s, curfile);
        }

        if !functions.is_empty() {
            s.push_str(r##"<h2 id="functions" class="anchorable header">Functions<a class="anchor" href="#functions">§</a></h2>"##);
        }
        for key in functions {
            let key = match key {
                FnId::Extern(key) => ModuleScopeValue::ExternalFunction(key.cast()),
                FnId::Normal(key) => ModuleScopeValue::Function(key.cast()),
            };
            self.write_reference(key, &mut s, curfile);
        }

        if !structs.is_empty() {
            s.push_str(r##"<h2 id="structs" class="anchorable header">Structs<a class="anchor" href="#structs">§</a></h2>"##);
        }
        for key in structs {
            self.write_reference(ModuleScopeValue::Struct(key.cast()), &mut s, curfile);
        }

        if !traits.is_empty() {
            s.push_str(r##"<h2 id="traits" class="anchorable header">Traits<a class="anchor" href="#traits">§</a></h2>"##);
        }
        for key in traits {
            self.write_reference(ModuleScopeValue::Trait(key.cast()), &mut s, curfile);
        }

        if !statics.is_empty() {
            s.push_str(r##"<h2 id="statics" class="anchorable header">Statics<a class="anchor" href="#statics">§</a></h2>"##);
        }
        for key in statics {
            self.write_reference(ModuleScopeValue::Static(key.cast()), &mut s, curfile);
        }

        s.push_str(HTML_POSTAMBLE);
        s
    }

    fn write_reference(&self, value: ModuleScopeValue<'ctx>, s: &mut String, curfile: &Path) {
        use ModuleScopeValue as V;

        let path = self.get_item_path(value, curfile);
        let ty = item_ty(value);
        let name = match value {
            V::Function(key) => self.tc_ctx.functions.read()[key.cast()]
                .0
                .name
                .unwrap()
                .symbol(),
            V::ExternalFunction(key) => self.tc_ctx.external_functions.read()[key.cast()]
                .0
                .name
                .unwrap()
                .symbol(),
            V::Struct(key) => self.tc_ctx.structs.read()[key.cast()].name.symbol(),
            V::Static(key) => self.tc_ctx.statics.read()[key.cast()].name.symbol(),
            V::Module(key) => self.tc_ctx.modules.read()[key].name,
            V::Trait(key) => self.tc_ctx.traits.read()[key.cast()].name.symbol(),
        };
        s.push_str("<span class=\"item-ref anchorable ");
        s.push_str(ty);
        s.push_str("\" id=\"");
        s.push_str(ty);
        s.push('.');
        s.push_fmt(format_args!("{}", urlencode(name.to_str())));
        s.push_str("\"><a href=\"");
        s.push_fmt(format_args!("{}", urlencode(&path)));
        s.push_str("\" class=\"item-ref ");
        s.push_str(ty);
        s.push_str("\">");
        s.escaped().push_str(name.to_str());
        s.push_str("</a><a class=\"anchor\" href=\"#");
        s.push_str(ty);
        s.push('.');
        s.push_fmt(format_args!("{}", urlencode(name.to_str())));
        s.push_str("\">§</a></span>");
    }

    fn write_ty(&self, s: &mut String, ty: Ty<'_>, curfile: &Path) {
        use mira_typeck::TyKind as T;

        match &*ty {
            T::DynType(bounds) => {
                s.push_str("dyn ");
                for (i, (trait_, trait_name)) in bounds.iter().enumerate() {
                    if i != 0 {
                        s.push_str(" + ");
                    }
                    s.push_str("<a class=\"trait\" href=\"");
                    self.get_item_path(ModuleScopeValue::Trait(trait_.cast()), curfile);
                    s.push_str("\">");
                    s.escaped().push_str(trait_name.symbol().to_str());
                    s.push_str("</a>");
                }
            }
            T::Struct { struct_id, name } => {
                s.push_str("<a class=\"struct\" href=\"");
                let path = self.get_item_path(ModuleScopeValue::Struct(struct_id.cast()), curfile);
                s.push_fmt(format_args!("{}", urlencode(&path)));
                s.push_str("\">");
                s.escaped().push_str(name.symbol().to_str());
                s.push_str("</a>");
            }
            T::UnsizedArray(ty) => {
                s.push('[');
                self.write_ty(s, *ty, curfile);
                s.push(']');
            }
            T::SizedArray {
                typ,
                number_elements,
            } => {
                s.push('[');
                self.write_ty(s, *typ, curfile);
                s.push_str("; ");
                s.push_fmt(format_args!("{number_elements}"));
                s.push(']');
            }
            T::Tuple(ty_list) => {
                s.push('(');
                for (i, &ty) in ty_list.iter().enumerate() {
                    if i != 0 {
                        s.push_str(", ");
                    }
                    self.write_ty(s, ty, curfile);
                }
                s.push(')');
            }
            T::Function(function_type) => {
                s.push_str("fn(");
                for (i, &ty) in function_type.arguments.iter().enumerate() {
                    if i != 0 {
                        s.push_str(", ");
                    }
                    self.write_ty(s, ty, curfile);
                }
                s.push(')');
                if function_type.return_type != default_types::void {
                    s.push_str(" -&gt; ");
                    self.write_ty(s, function_type.return_type, curfile);
                }
            }
            T::PrimitiveNever => s.push('!'),
            T::PrimitiveVoid => s.push_str("<span class=\"struct\">void</span>"),
            T::PrimitiveI8 => s.push_str("<span class=\"struct\">i8</span>"),
            T::PrimitiveI16 => s.push_str("<span class=\"struct\">i16</span>"),
            T::PrimitiveI32 => s.push_str("<span class=\"struct\">i32</span>"),
            T::PrimitiveI64 => s.push_str("<span class=\"struct\">i64</span>"),
            T::PrimitiveISize => s.push_str("<span class=\"struct\">isize</span>"),
            T::PrimitiveU8 => s.push_str("<span class=\"struct\">u8</span>"),
            T::PrimitiveU16 => s.push_str("<span class=\"struct\">u16</span>"),
            T::PrimitiveU32 => s.push_str("<span class=\"struct\">u32</span>"),
            T::PrimitiveU64 => s.push_str("<span class=\"struct\">u64</span>"),
            T::PrimitiveUSize => s.push_str("<span class=\"struct\">usize</span>"),
            T::PrimitiveF32 => s.push_str("<span class=\"struct\">f32</span>"),
            T::PrimitiveF64 => s.push_str("<span class=\"struct\">f64</span>"),
            T::PrimitiveStr => s.push_str("<span class=\"struct\">str</span>"),
            T::PrimitiveBool => s.push_str("<span class=\"struct\">bool</span>"),
            T::PrimitiveSelf => s.push_str("<span class=\"struct\">Self</span>"),
            T::Ref(ty) => {
                s.push_str("&amp;");
                self.write_ty(s, *ty, curfile);
            }
            T::Generic { name, .. } => s.escaped().push_str(name.symbol().to_str()),
        }
    }

    /// generates the search index of all generated and queued values.
    pub fn generate_search_index(&self) -> String {
        let mut s = "window.search_index = [".to_string();
        for (i, qualified_path) in self.generated.borrow().values().enumerate() {
            if i != 0 {
                s.push(',');
            }
            let path = qualified_path.0.strip_prefix(&self.path).unwrap();
            s.push('[');
            s.push_fmt(format_args!("{path:?}"));
            s.push_str(",[");
            for (i, sym) in qualified_path.1.iter().enumerate() {
                if i != 0 {
                    s.push(',');
                }
                s.push_fmt(format_args!("{:?}", sym.to_str()));
            }
            s.push_str("]]");
        }
        s.push_str("]; if(window.init_search != null) window.init_search(window.search_index);");
        s
    }
}

enum FnId<'ctx> {
    Extern(StoreKey<ExternalFunction<'ctx>>),
    Normal(StoreKey<Function<'ctx>>),
}

fn with_more_refs<'ctx>(ctx: TypeCtx<'ctx>, mut ty: Ty<'ctx>, mut refs: usize) -> Ty<'ctx> {
    while refs > 0 {
        refs -= 1;
        ty = ctx.intern_ty(TyKind::Ref(ty));
    }
    ty
}

fn unsubstitute_self<'ctx>(ctx: TypeCtx<'ctx>, ty: Ty<'ctx>, selfty: Ty<'ctx>) -> Ty<'ctx> {
    let mut refcount = 0;
    let mut newty = ty;
    loop {
        if newty == selfty {
            return match refcount {
                0 => default_types::self_,
                1 => default_types::self_ref,
                _ => with_more_refs(ctx, default_types::self_ref, refcount - 1),
            };
        }
        let TyKind::Ref(inner) = &*newty else {
            return ty;
        };
        newty = *inner;
        refcount += 1;
    }
}

pub fn urlencode(s: &str) -> UrlEncoded {
    UrlEncoded(s)
}

pub struct UrlEncoded<'a>(&'a str);
impl Display for UrlEncoded<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut c = [0u8; 4];
        for char in self.0.chars() {
            if char == '#'
                || char == '_'
                || char == '-'
                || char == '/'
                || char == '.'
                || char.is_ascii_alphanumeric()
            {
                f.write_char(char)?;
            } else {
                for c in char.encode_utf8(&mut c).as_bytes() {
                    f.write_char('%')?;
                    Display::fmt(&c, f)?;
                }
            }
        }
        Ok(())
    }
}

pub struct HTMLEscapeStr<'a>(&'a mut String);
impl Write for HTMLEscapeStr<'_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.push_str(s);
        Ok(())
    }

    fn write_char(&mut self, c: char) -> std::fmt::Result {
        self.push(c);
        Ok(())
    }
}
impl HTMLEscapeStr<'_> {
    pub fn push_fmt(&mut self, fmt: Arguments<'_>) {
        self.write_fmt(fmt).unwrap();
    }
    pub fn push_str(&mut self, s: &str) {
        for c in s.chars() {
            self.push(c);
        }
    }
    pub fn push(&mut self, c: char) {
        match c {
            '<' => self.0.push_str("&lt;"),
            '>' => self.0.push_str("&gt;"),
            '&' => self.0.push_str("&amp;"),
            '"' => self.0.push_str("&quot;"),
            _ => self.0.push(c),
        }
    }
}
pub trait StringExt {
    fn push_fmt(&mut self, args: Arguments<'_>);
}
impl StringExt for String {
    fn push_fmt(&mut self, args: Arguments<'_>) {
        self.write_fmt(args).unwrap()
    }
}
pub trait HTMLEscapeExt {
    fn escaped(&mut self) -> HTMLEscapeStr;
}
impl HTMLEscapeExt for String {
    fn escaped(&mut self) -> HTMLEscapeStr {
        HTMLEscapeStr(self)
    }
}

fn item_ty(v: ModuleScopeValue<'_>) -> &'static str {
    match v {
        ModuleScopeValue::Function(_) | ModuleScopeValue::ExternalFunction(_) => "function",
        ModuleScopeValue::Struct(_) => "struct",
        ModuleScopeValue::Static(_) => "static",
        ModuleScopeValue::Module(_) => "module",
        ModuleScopeValue::Trait(_) => "trait",
    }
}

/*
<!DOCTYPE html>
<html lang="en" color-scheme="preference">
    <head>
-------------------------------------------- ^- HTML_HEAD
has to  |<link rel="stylesheet" href="./index.css">
be gene-|<noscript><link rel="stylesheet" href="./noscript.css"></noscript>
rated   |<script>const root = "./"</script>
        |<script src="./index.js"></script>
-------------------------------------------- v- HTML_PREAMBLE1
    </head>
    <body>
        <div class="sidebar">
            <h2>Packages</h2>
-------------------------------------------- ^- HTML_PREAMBLE1
            <a href="#">std</a>
            <a href="#">root</a>
-------------------------------------------- v- HTML_PREAMBLE2
        </div>
        <div class="content">
            <div class="search-header">
                <input class="search-input" placeholder="press s or / to start searching" />
                <a class="settings-button">Settings</a>

                <div class="settings-popup hidden">
                    <div>Theme</div>
                    <label for="theme-light" class="setting-radio">
                        <input type="radio" name="theme" id="theme-light" value="light">
                        <span>light</span>
                    </label>
                    <label for="theme-dark" class="setting-radio">
                        <input type="radio" name="theme" id="theme-dark" value="dark">
                        <span>dark</span>
                    </label>
                    <label for="theme-ayu" class="setting-radio">
                        <input type="radio" name="theme" id="theme-ayu" value="ayu">
                        <span>ayu</span>
                    </label>
                    <label for="theme-preference" class="setting-radio">
                        <input type="radio" name="theme" id="theme-preference" value="preference">
                        <span>system preference</span>
                    </label>
                </div>
            </div>
            <div class="search-results hidden">
            </div>
            <div class="main-content">
-------------------------------------------- ^- HTML_PREAMBLE
            ... body
-------------------------------------------- v- HTML_POSTAMBLE
            </div>
        </div>
    </body>
</html>
*/
static HTML_HEAD: &str = r#"<!DOCTYPE html><html lang="en" color-scheme="preference"><head>"#;
static HTML_PREAMBLE1: &str = r#"</head><body><div class="sidebar"><h2>Packages</h2>"#;
static HTML_PREAMBLE2: &str = r#"</div><div class="content"><div class="search-header"><input class="search-input" placeholder="press s or / to start searching" /><a class="settings-button">Settings</a><div class="settings-popup hidden"><div>Theme</div><label for="theme-light" class="setting-radio"><input type="radio" name="theme" id="theme-light" value="light"><span>light</span></label><label for="theme-dark" class="setting-radio"><input type="radio" name="theme" id="theme-dark" value="dark"><span>dark</span></label><label for="theme-ayu" class="setting-radio"><input type="radio" name="theme" id="theme-ayu" value="ayu"><span>ayu</span></label><label for="theme-preference" class="setting-radio"><input type="radio" name="theme" id="theme-preference" value="preference"><span>system preference</span></label></div></div><div class="search-results hidden"></div><div class="main-content">"#;
static HTML_POSTAMBLE: &str = "</div></div></body></html>";
