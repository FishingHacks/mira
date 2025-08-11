# Mira Language Reference

# Mira standard library

The [mira standard library](./std_docs.md) has its own documentation. This will not describe the standard library, but uses it in some examples.

# Hello World

```rs
pub fn main() {
    "Hello, World!".println();
}
```

```
$ mirac compile -r ./hello.mr
```

# Comments

Mira supports the standard c-style comments of line-comments using `//` and multiline comments using `/* ... */`.

# Values

```rs
pub fn main() {
    // integers
    let one_i32 = 12;      // i32 is implied for all numbers except floats
    let other_i32 = 12i32; // i32 can also be explicitly specified using number prefixes
    let some_u8: u8 = 12;  // ...or using an explicity type annotation

    let a_new_number = one_i32 + other_i32; // the resulting type of a binary operations will always be equivalent to it's first member.
    let a_new_u8     = some_u8 + other_i32; // this will fail because generally you need to have both operands be of the same type, with an exception being bitshifts (<< / >>)

    let my_float = 12f32;   // unlike in rust, floats don't need a `.0` suffix, just suffixing the number or putting the type explicitly on the let is enough.
    let other_float = 13.5; // if you have decimal digits, it is implied that the float will be f32

    let meow = some_i32 as u8 + some_u8; // if you want to add values anyway, you have to cast them. See more on casting later.

    let my_bool = true; // booleans work using the standard true/false syntax
    let my_empty_type = void; // there's the void literal which's type is void. Can be used for things like `while(true) void;` or `fn a(...) = void`

    let my_tuple = .[1, my_float];        // Tuples can be constructed like an array when prefixed with a `.`
    let other_tuple = .(some_u8, meow);   // Alternatively, parentheses can be used

    let my_string = "abcd"; // Strings, like in rust, are statically-allocated and slices/fat pointers. They use the `&str` type. Mutating them is undefined behaviour.
    let my_array = [1, 2, 3, 4];         // This will create a 4-element i32 array ([i32; 4]).
    let my_slice = &my_array as &[i32];  // A reference to it can be cast to a slice of type &[i32].
    let other_slice: &[i32] = &my_array; // This cast is explicit and this will fail because `&[i32; 4]` is not the same as `&[i32]`
}
```

# Primitive Types

| **Type** | **C Equivalent**                | **Description**                                                |
|----------|---------------------------------|----------------------------------------------------------------|
| `i8`     | `char`, `int8_t`                | signed 8-bit integer                                           |
| `i16`    | `short`, `int16_t`              | signed 16-bit integer                                          |
| `i32`    | `int`, `int32_t`                | signed 32-bit integer                                          |
| `i64`    | `long long`, `int64_t`          | signed 64-bit integer                                          |
| `isize`  | `intptr_t`                      | signed pointer-sized integer                                   |
| `u8`     | `unsigned char`, `unt8_t`       | unsigned 8-bit integer                                         |
| `u16`    | `unsigned short`, `unt16_t`     | unsigned 16-bit integer                                        |
| `u32`    | `unsigned int`, `unt32_t`       | unsigned 32-bit integer                                        |
| `u64`    | `unsigned long long`, `unt64_t` | unsigned 64-bit integer                                        |
| `usize`  | `untptr_t`, `size_t`            | unsigned pointer-sized integer                                 |
| `f32`    | `float`                         | 32-bit floating point (23-bit mantissa) IEEE-754-2008 binary32 |
| `f64`    | `double`                        | 64-bit floating point (52-bit mantissa) IEEE-754-2008 binary64 |
| `bool`   | `bool`                          | `true` or `false`                                              |
| `void`   | `void`                          | a zero-sized type, a pointer to which is used for type erasure |
| `!`      | (none)                          | indicates a function never returns                             |

# String literals

String literals are sized slices that contain utf-8 encoded data when obtained through the compiler. They are *not* null-terminated and can be cast like a slice of u8s (`&str` == `&[u8]`).

Their memory-layout is the same as a slices, so `(&u8, usize)`, the first element is the pointer to the data and the second one is the length of the data.

## Escape Sequences

| Escape Sequence | Name                 |
|-----------------|----------------------|
| `\n`            | Newline              |
| `\r`            | Carriage Return      |
| `\t`            | Tab                  |
| `\0`            | NUL characters (0x0) |

Note that any other character will be escaped to that character (`\j` is j, `\\` is \\, `\"` is " and so on)

# Assignments

Assignments work like any other language using the `=` character. Note however, that if you want to assign to a pointer, you have to dereference it first:

```rs
let a = 13i32;
let b = &a;

a  = 12;
*b = 13;
```

Variables also have to be initialized, you cannot leave them uninitialized like in other languages like rust or zig:\
```rs
let a: i32;

a = 13;
```

```
let_must_be_initialized.mr:1:10: Expected Equal but found Semicolon
```

# Identifiers

All things exposed in a module (global variables, variables, functions, types, ...) are identified using identifiers. An identifier can be used by just spelling it without any special quotation or so, as long as it only consists of ascii alphanumeric characters, _, # or $ (Using rust pattern syntax: 'a'..='Z' | 'A'..='Z' | '0'..='9' | '_' | '#' | '$')

If you identifier does *not* meet these criteria or is a used keyword (e.g. if, void or while), you can use the *identifier string literal* syntax. It is exactly like string literals with the exception that the regular quotes (") are replaced with backticks (`).

```rs
fn my_fancy_function() = void;
fn `my fancy function needs space\nand newlines`() = void;
```

# Global Variables

Global Variables can be declare just like local variables with the exception that here, a type is mandatory:

```rs
let MY_GLOBAL: i32 = 12;
```

# Operators

Note: While there is no operator overloading as of right now, it will be added using the `Add`, `Sub`, ... traits in the future.

| Name                  | Syntax                | Types                 |
|-----------------------|-----------------------|-----------------------|
| Addition              | `a += b`, `a + b`     | Integers, Floats      |
| Subtraction           | `a -= b`, `a - b`     | Integers, Floats      |
| Multiplication        | `a *= b`, `a * b`     | Integers, Floats      |
| Division              | `a /= b`, `a / b`     | Integers, Floats      |
| Remainder Devision    | `a %= b`, `a % b`     | Integers, Floats      |
| Bitwise and           | `a &= b`, `a & b`     | Integers, Booleans    |
| Bitwise or            | `a \|= b`, `a \| b`   | Integers, Booleans    |
| Bitwise xor           | `a ^= b`, `a ^ b`     | Integers              |
| Bit shift left        | `a <<= b`, `a << b`   | Integers              |
| Bit shift right       | `a >>= b`, `a >> b`   | Integers              |
| Logical and           | `a && b`              | Booleans              |
| Logical or            | `a \|\| b`            | Booleans              |
| Equality              | `a == b`              | Any Type              |
| Inequality            | `a != b`              | Any Type              |
| Greater Than          | `a > b`               | Integers              |
| Greater or Equal      | `a >= b`              | Integers              |
| Less Than             | `a < b`               | Integers              |
| Lesser or Equal       | `a <= b`              | Integers              |
| Piping                | `a \|> func()`        | Any Type              |
| Negation              | `-a`                  | Integers, Floats      |
| Bitwise not           | `~a`                  | Integers, Booleans    |
| Logical not           | `!a`                  | Booleans              |
| Dereferencing         | `*a`                  | Pointers              |
| Referencing           | `&a`                  | Any type              |
| Indexing              | `a[index]`            | Slices, Arrays        |

# Structures

Structures are a way to store multiple types of data. They have a number of known fields with known types. Structs can be initialized using `StructName { ... }` or `.{ ... }` if the type of the struct is known.

```rs
struct MyStruct {
    // this struct will have 2 fields of type i32. This means it is homogenous.
    x: i32, y: i32;

    // Structs can have inlined function implementation. This follows a semicolon after the items. Functions that take `Self` or a reference to `Self` as a first argument can be called on the struct. Self in this case always refers to the struct itself.
    fn new(x: i32, y: i32) -> Self = .{ x: x, y: y };
    fn get_x(self: &Self) -> i32 = self.x;
    fn get_y(self: &Self) -> i32 = self.y;
    fn debug(self: &Self) {
        "MyStruct { x: ".print();
        self.get_x().print();
        ", y: ".print();
        self.get_y().print();
        " }".println();
    }
}
```

# Blocks

Blocks are a container to keep variables and the like in. Variables defined in blocks are only accessible in them, and blocks are commonly used for functions or control-flow statements because they can contain multiple statements. Unlike other programming languages however, blocks are not expressions and cannot return any values. Same goes for all statements (if, while, ...)

```rs
let x = 12;
{
    let x = 13usize; // note that here x is *redeclared* with the let keyword. An assignment would still assign to the x outside of the scope. This is called shadowing.
    x.print(); // prints 13
}
x.print(); // prints 12
```

# While loops

A while loop is a very basic loop construct, accepting an expression of type `bool` as a statement and a body that will be executed as long as that expression evaluates to true. Note that the parentheses around the expression are required because an expression statement could immediately follow the condition.

```rs
fn main(argc: usize, argv: &&u8) {
    let i = 0;
    while (i < argc) {
        "Argument :3".println();
        i += 1;
    }
}
```

# If

If is a very basic control flow operation. It accepts a condition and a statement to jump to if the condition evaluates to true. It also accepts an optional else statement after the else keyword to jump to if the condition evaluates to false. Putting another if after the else allows checking multiple conditions. Just like with while, parentheses around the condition are a requirement.

```rs
fn main(argc: usize, argv: &&u8) {
    if (argc == 0) "Got no arguments".println();
    else if (argc == 1) "Got 1 argument".println();
    else if (argc == 2) "Got 2 arguments".println();
    else if (argc == 3) "Got 3 arguments".println();
    else if (argc == 4) "Got 4 arguments".println();
    else if (argc == 5) "Got 5 arguments".println();
    else "Got >5 arguments".println();

    if (argc == 0) panic("no args found");
}
```

# Functions

Functions are a unit of behavior. They contain code that will execute when calling them to perform a specific operation. They are introduced using the `fn` keyword, after which their name and generics follow (note: generics are not supported yet). Then come the arguments and finally the return type and either a block or an equal character and a statement that will be returned.

**NOTE: Generics for functions are not supported yet**

```rs
// note: generics aren't support yet
fn len<T>(value: &[T]) -> usize = std::intrinsics::metadata(value);

// if the return value is not specified, it's implied to be void. These functions also don't need a return at the end and the statement can be omitted from the return.
fn main(argc: usize, argv: &&u8) {
    print_str("args: "); print_usize(argc); print_newline();
}

// You can also define external functions that can be accessed from outside the executable (e.g. if you're making a library) or be supplied by the outside (e.g. a library):
@callconv(c)
extern fn print_my_fancy_value(my_fancy_value: u8) = print_usize(my_fancy_value as usize);
@callconv(c)
extern fn dlopen(filename: &u8, flags: i32);
```

# Casting

Casts can be done using the as keyword: `<value> as <type>`. There are a number of casts:

- `&str`    -> `&[u8]`   (any pointer)
- `&str`    -> `&u8`     (any pointer)
- `&T`      -> `&[T; 1]` (any pointer)
- `&T`      -> `&dyn _`
- `&[T; N]` -> `&[T]`
- `&[T]`    -> `&T`
- `&void`   -> `&T`       (any pointer)
- `&T`      -> `&void`    (any pointer)
- `&void`   -> `usize`
- `usize`   -> `&void`
- `fn(_)`   -> `&void`    (any pointer)

Note: The suffix (any pointer) means that it does not matter if the value is references as it is a bitcast (e.g. for &T to &[T;1], the size of T == the size of [T;1]. That means that you can do that conversion with &&T to &&[T; 1] as well). This essentially just means that the type you're casting to is the same as the current one with the difference that they're expressed differently in the typesystem.

Alongside this, you can also cast any number to any other number and booleans to u8 and u8 to booleans. Casts will generally be bitcasts for sizes of the same types, truncate in case the new type is smaller, sign extend in case the new type is larger and the original signed and zero extend in case the new type is larger and the original type is unsigned.
This means that `-1i8 as u8` would result in `255`, not `0`

# Modules

Modules in mira are seperate files, and have to be declare in the "parent" module. Declaration of modules is done by insering `mod <modname>;`.
The file of the module will live in `<parent module name>/<modname>.mr` or `<parent module name>/<modname>/mod.mr`. This means that `mod mira;` in `src/main.mr` will check `src/mira.mr` and then `src/mira/mod.mr`.

Declaring a module will also automatically put it in scope, meaning if you do `mod mira;`, you can then import things from it through use, for example `use mira::lexer;`.
In order for other modules to be able to access the module, you have to declare it as public, exposing it under it's name in your module.
E.g., `pub mod lexer;` in src/mira.mr can be imported from `src/main.mr` through `use mira::lexer;` or in `src/encoder.mr` through `use pkg::mira::lexer;`.
Note the `pkg`: This is a special module that is available in every module and points to the root module of the package.

Importing dependency is exactly like importing a module, except that all dependencies are implicitly available to each file.
For example, it is possible to import the `panic` function from the standard library using `use std::panic;`.

Just like modules, it is also possible to declare imports as public, exposing them at the current module. This gives the ability to expose items from child modules, without exposing them directly:

```rs
src/main.mr
mod mira;

fn main() {
    // the `print` function in src/mira/main.mr
    mira::print();
}

src/mira.mr
mod main;
pub use main::print;

src/mira/main.mr
pub fn print() {
    "Heyo".print();
}
```

# Assembly

There are 2 types of assembly in mira: module-level assembly and expression assembly. Module level assembly can be put anywhere in the module, and will be anywhere in the resulting binary, whereas expression assembly can take inputs and give an output and goes into expressions.
This is nearly identical to the zig way of doing it.

```rs
// a newline is automatically inserted between strings.
asm(
    ".globl _start",
    "_start:",
    "call main"
);

// won't generate a function header
extern fn len(value: &str) {
    let str_ptr_addr = &value as &void as usize;
    let value = asm(
        // you can defined inputs and outputs and reference them in assembly using `%[<name>]`.
        "movq 8(%[straddr]), %[out]"
        // here goes your optional output using the syntax [name] "register" (type). Note that it uses llvm-style registers.
        : [out] "=r" (usize)
        // here goes your comma seperated list of inputs. Syntax is like outputs with the exception that you don't specify the type but rather the variable that holds the value.
        : [straddr] "r" (str_ptr_addr)
        // here goes a comma seperated list of clobbers. Note that the `~{<clobber>}` syntax of llvm was changed to just the clobber.
        : "memory"
    );
}
```

# Annotations

You may have seen throughout the document weird calls to functions with an @ at the start. If you're coming from zig, you might think of builtins, but these are actually so-called annotations.
They provide some sort of information to the following statement such as its calling convention (`@callconv`), it's alias (`@alias`) for external function and so on.
While they may look like functions, they act more like macros because their arguments is just a list of tokens that each annotation can parse however it wants.
This is similar to `#[...]` in rust.

## Available annotations

| Name              | Valid Statements                  | Description                                                                                                   |
|-------------------|-----------------------------------|---------------------------------------------------------------------------------------------------------------|
| @alias            | External Functions                | Provides the symbol it should be exposed as in the compiled object                                            |
| @callconv         | External Functions, Functions     | Provides the calling convention of a function. Supported values are `c`, `naked`, `fast`, `cold` and `inline` |
| @ext_vararg       | External Functions                | Marks that an external function has a variable arguments (, ... in c and rust)                                |
| @function_attr    | External Functions, Functions     | Provides attributes to a function. Support attributes: `hot`, `cold`                                          |
| @noinline         | External Functions, Functions     | Marks a function as not being supposed to be inlined.                                                         |
| @section          | External Functions                | Sets the section an external function is supposed to be in                                                    |
| @lang             | Everything                        | Marks something as being a lang item. **NOTE: THIS SHOULD GENERALLY NOT BE USED.**                            |
| @intrinsic        | Functions                         | Marks something as being an intrinsic. **NOTE: THIS SHOULD NEVER BE USED, USE intrinsics.mr IN LIBSTD.**      |
