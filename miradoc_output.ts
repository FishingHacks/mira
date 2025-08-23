type Type = PrimitiveType | RefTy | AdtTy | ArrayTy | Tuple | DynType | FunctionTy | GenericTy;
type ItemRef = number;
type Annotations = string[];

interface PrimitiveType {
    kind: "primitive";
    inner: "u8" | "u16" | "u32" | "u64" | "usize" | "i8" | "i16" | "i32" | "i64" | "isize" | "f32" | "f64" | "str" | "void" | "never" | "bool";
};

interface RefTy {
    kind: "ref";
    inner: Type;
};

interface AdtTy {
    kind: "adt";
    item: ItemRef;
    name: string;
};

interface ArrayTy {
    kind: "array";
    size?: number;
    elem: Type;
};

interface Tuple {
    kind: "tuple";
    elements: Type[];
};

interface DynType {
    kind: "dyn";
    name: string;
    item: ItemRef;
};

interface FunctionTy {
    kind: "func";
    args: Type[];
    return_ty?: Type;
};

interface GenericTy {
    kind: "generic";
    name: string;
};

type Item = ModuleItem | FunctionItem | ExternalFunctionItem | StaticItem | TraitItem;

interface ModuleItem {
    kind: "module";
    exports: ItemRef[];
    path: string[];
};

interface FunctionItem {
    kind: "function";
    path: string[];
    sig: FunctionTy;
    generics: Generic[];
    annotations: Annotations,
};

interface ExternalFunctionItem {
    kind: "external_function";
    path: string[];
    sig: FunctionTy;
    has_body: boolean;
    annotations: Annotations,
};

interface Generic {
    name: string;
    sized: boolean;
    bounds: GenericBound[];
    annotations: Annotations,
};

interface GenericBound {
    trait_name: string;
    item: ItemRef;
};

interface StaticItem {
    kind: "static";
    path: string[];
    ty: Type;
    annotations: Annotations,
};

interface TraitItem {
    kind: "trait";
    path: string[];
    funcs: TraitFunc[];
    annotations: Annotations,
};

interface TraitFunc {
    name: string;
    sig: FunctionTy;
};

interface StructItem {
    kind: "function",
    path: string[],
    elements: Record<string, Type>;
    generics: string[]
    impl: ItemRef[],
    trait_impls: TraitImplementation[],
    annotations: Annotations,
};

interface TraitImplementation {
    trait_name: string;
    trait: ItemRef;
    funcs: ItemRef[];
};

interface Output {
    libraries: ItemRef[];
    main_library: ItemRef;
    items: Item[];
}
