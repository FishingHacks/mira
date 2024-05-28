I want:

- types (structs)
- variables
- for loops
- duck typing
- ^- interfaces so you can go `impl Iterate for String {}`
- custom operations
- ^- trait like objects
- rust-like syntax
- should be highly controllable, aka. u can pause and resume the execution at any point. For this, we need a vm-like architecture, meaning u can step through the instruction. This assumption means that each instruction should take not a long time. Also I want to add memory limits for the VM, but that will be for later
- Format-strings (`${abcd} abc`) and format()/log() which takes a string (like "Hiii, %s. You are %d years old") and arguments for c-style templating
- copy before arguments means we clone them (aka you don't modify what you've taken).


> # TODO
> - Generics
> - `import` keyword with relative file inclusion and modules (`./incl.lang`, `@std/fs`) - Checks Bultin packages and then looks into `<project dir>/packages` (project dir = parent directory of the interpreted file), `$USER/.local/share/lang_packages`, `/usr/lib/lang_packages` and in any paths specified by `<project dir>/config.toml`. Looks into (@ is not part of the name, so @std/fs would be std/fs.lang iex) `<name>.lang`, `<name>/index.lang`, `<name>/main.lang` and `name.complang` (compiled lang bytecode)
> - .so Interoperability


Built-in Types:
- `number` (f64) A 64-Bit floating point number
- `string` A String
- `bool` A boolean
- `table` A table (HashMap) (alias: `{}`)
- `array` An Array (alias: `[]`)
- `typed_array` An array of a specific type (alias: `[type]`)
- `function` A function
- `any` Any type
- `Result`: `{ success: bool, value: any }` A return value of a function (TODO: Add Generics)
- Internal `ArrayStruct`: An array representation of a structure (to save space)
- Internal `MultiType`: A representation of a multi-type (`a | b | c`), like string and number: `string | number`

## built-in functions:
- `typeof(val: any)`: `fn typeof(val: any) { return typeof val; }`
- `panic(val: any?)`: Panics
- `println(val: any?, ...format)`: Prints something to stdout (+ a new line)
- `eprintln(val: any?, ...format)`: Prints something to stderr (+ a new line)
- `read()`: Reads something from stdin
- `print(val: any?, ...format)`: Writes something to stdout
- `eprint(val: any?, ...format)`: Writes something to stder
- `str(val: any?)`: Turns some value into a string (usually by calling val.to_string())
- `num(val: any?)`: Tries to turn a value into a number
- `format(val: string, ...args)`: Formats a string
- `copy(val)`: Copies/Clones the value
- `rawget` (like [] if there'd be no Index implemented)
- `call` calls a function with specified arguments
- `exit` exits the program (aka stops execution)

## built-in traits:
- `Stringify`
- `Iterate`
- `MathOperations` (all the mathematical operators below combined into one)
- `Add` (aka +)
- `AddAssign` (aka +=)
- `Sub` (aka -)
- `SubAssign` (aka -=)
- `Div` (aka /)
- `DivAssign` (aka /=)
- `Mul` (aka *)
- `MulAssign` (aka *=)
- `Mod` (aka %)
- `ModAssign` (aka %=)
- `And` (aka &)
- `AndAssign` (aka &=)
- `Or` (aka |)
- `OrAssign` (aka |=)
- `Xor` (aka ^)
- `XorAssign` (aka ^=)
- `Not` (aka ~)
- `Index` (aka [...])

```rs
const value1 = 12;
let value2 = { a: 12 };
let value3: string = "";

fn main(val: number, some_arg, ...spread) {
    value2.a;
    for i in range(0, val) {
        println(spread[i]);
    }
    println(typeof(some_arg));
}

trait Iterate {
    fn next(self: Self) -> any;
    fn next(self: Self) -> any { // this codeblock gets ignored, these 2 do functionally the same thing, this would throw an error due to redefinition of Iterate::next()!
        println(123);
    }
}

struct Abc {
    val: string;
}

impl Iterate for Abc { // <- While this may not look like it, this is still ducktyping. impl Iterate for Abc just hints at the compiler what im trying to do, so that the compiler itself can throw an error if too many or too little functions are implemented or if the function arguments and returns and the ones of the trait don't match.
    fn next(self) {
        return 12;
    }
}

// impl Abc {
//     fn next(self: Self) -> number {
//         return 12;
//     }
// }

// this would have the same effect as the above

struct WeirdNumber {}

impl WeirdNumber {
    fn string() { return "12" }
    fn add(self, other) {
        if typeof(other) == "number" {
            return other - 10;
        }
    }
}
let num = WeirdNumber{};
println("10 + %s: %s", num, 10 + num); // 10 + 12: 0

let a = 0;

fn add_to_number1(copy val: number) { // <- if its not copy ..., its always implicitly &int, aka &i32
    val += 12;
    println(val);
}

add_to_number1(a); // 12
println(a); // 0

fn add_to_number2(val: number) { // <- if its not copy ..., its always implicitly &int, aka &i32
    val += 12;
}

add_to_number1(a); // 12
println(a) // 12

while (true) {}

// Proposed Generics
// Replace all generics with any for right now!

struct Result<T, TErr> {
    val: T | TErr,
    success: bool
}

impl Result {
    fn is_ok(self: Self) -> bool {
        return self.success;
    }

    fn unwrap() -> T {
        if (!self.success) {
            panic("Failed to unwrap!")
        }
        return self.val as T;
    }

    fn ok(self: Self) -> T {
        return self.val as T;
    }

    fn err(self: Self) -> TErr {
        return self.val as TErr;
    }

    fn expect(self: Self, str: String?) -> T {
        if (self.success) {
            return self.val as T;
        }
        if (typeof(str) == "string") {
            panic(format("Failed to unwrap: %s", str))
        }
    }
}
```