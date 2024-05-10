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


Built-in Types:
- `number` (f64) A 64-Bit floating point number
- `string` A String
- `bool` A boolean
- `table` A table (HashMap)
- `array` An Array
- `Result`: `{ success: bool, value: any }` A return value of a function (TODO: Add Generics)
- Internal `ArrayStruct`: An array representation of a structure (to save space)

```rs
const value1 = 12;
let value2 = { a: 12 };
let value3: string = "";

fn main(val: number, some_arg, spread: ...) {
    value2.a;
    for i in range(0, val) {
        print(spread[i]);
    }
    print(typeof(some_arg));
}

trait Iterate {
    fn next(self: Self) -> any;
    fn next(self: Self) -> any { // this codeblock gets ignored, these 2 do functionally the same thing, this would throw an error due to redefinition of Iterate::next()!
        print(123);
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

this would have the same effect as the above

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
print("10 + %s: %s", num, 10 + num); // 10 + 12: 0

let a = 0;

fn add_to_number1(copy val: number) { // <- if its not copy ..., its always implicitly &int, aka &i32
    val += 12;
    print(val);
}

add_to_number1(a); // 12
print(a); // 0

fn add_to_number2(val: number) { // <- if its not copy ..., its always implicitly &int, aka &i32
    val += 12;
}

add_to_number1(a); // 12
print(a) // 12

while true {}
```