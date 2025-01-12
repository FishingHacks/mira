- C Preprocessing

### HOW TO:

1 - get the c compiler (clang, cc or gcc)
2 - resolve the import path. Use `<c-compiler> -x c -H -fsyntax-only -`. For this, the c compiler will need a c-file to preprocess. So we will have to generate one based on the includes we have.
To make one is rather simple. Firstly, iterate over all the includes and if the include starts with `./` or `/`, the include needs to be put in quotes (e.g. `./test.h` => `"./test.h"`). As such, double quotes (`"`) are invalid as part of the include path, but everything else is possible. In case the include does not start with `./` or `/`, it has to be wrapped in `<` and `>`, which as such are also invalid as a part of the include path. Finally, you prefix all of the includes with `#include ` and join them with new lines and then pass them to the aforementioned command in stdin.
You will then probably get an output similar to the one below:
```sh
$ cat "#include <stdint.h>" | cc -x c -H -fsyntax-only -

. /usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/include/stdint.h
.. /usr/include/stdint.h
... /usr/include/bits/libc-header-start.h
.... /usr/include/features.h
..... /usr/include/features-time64.h
...... /usr/include/bits/wordsize.h
...... /usr/include/bits/timesize.h
....... /usr/include/bits/wordsize.h
..... /usr/include/sys/cdefs.h
...... /usr/include/bits/wordsize.h
...... /usr/include/bits/long-double.h
..... /usr/include/gnu/stubs.h
...... /usr/include/gnu/stubs-64.h
... /usr/include/bits/types.h
.... /usr/include/bits/wordsize.h
.... /usr/include/bits/timesize.h
..... /usr/include/bits/wordsize.h
.... /usr/include/bits/typesizes.h
.... /usr/include/bits/time64.h
... /usr/include/bits/wchar.h
... /usr/include/bits/wordsize.h
... /usr/include/bits/stdint-intn.h
... /usr/include/bits/stdint-uintn.h
... /usr/include/bits/stdint-least.h
Multiple include guards may be useful for:
/usr/include/bits/libc-header-start.h
/usr/include/bits/long-double.h
/usr/include/bits/time64.h
/usr/include/bits/typesizes.h
/usr/include/features-time64.h
/usr/include/gnu/stubs-64.h
/usr/include/gnu/stubs.h
```
The program then has to look out for lines starting with `. /` and collect their paths (the trimmed line without the first 2 characters) and canonicalize them to get the absolute include paths.
3 - the program has to prompt the c compiler to process that same stdin input, but using `<c-compiler> -x c -E -fsyntax-only -`
The output should be something like this:
```c
# 0 "<stdin>"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "<stdin>"
# 1 "/usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/include/stdint.h" 1 3 4
<some stuff>

# 0 "<Some-path>"
<some stuff>
```
As you can see, the output will have a `#`, a number and a path, below which will be what that path included. That way, we can check where function and static definitions came from and only expose them to the user if the user requested it.
4 - Parse the c file and and ignore all bodies as well as functions and statics in a non-requested file. You should also resolve typedefs.
> **Note**: An exception to this are statements such as `typedef struct { ... } <name>`, so typedef statements which have anonymous types. In this case, the specific type should be resolved as the typedef name.
Anonymous types within types should be resolved as `<outer_type>$<field_name>`.

Example:
```c
typedef struct {
    int a;
    union {
        int a;
        struct { char a; char b; } meow;
    } b;
} MyType;

typedef enum {
    Idle,
    Downloading,
    Download,
    Main,
    Done,
} States;

MyType test(int count, char flags, char should_rethrow);
bool has_panicked;
```

Output
```rs
struct MyType$b$meow {
    a: u8,
    b: u8,
}

union MyType$b {
    a: i32,
    meow: MyType$b$meow,
}

@repr(i32)
enum States {
    Idle = 0x0,
    Downloading = 0x1,
    Download = 0x2,
    Main = 0x3,
    Done = 0x4,
}

struct MyType {
    a: i32,
    b: MyType$b,
}

@(C)
extern fn test(count: i32, flags: u8, should_rethrow: u8) -> MyType;

extern let has_panicked: bool;
```

Todo to get the above working:
- Union
- Enum
- Extern let (or extern static)
- c include syntax
- c parser

Proposed syntax:
```rs
use "C" {
    // Defines
    @cfg(os = windows)
    PLATFORM=1; // -DPLATFORM=1
    @cfg(os = linux)
    PLATFORM=0; // -DPLATFORM=0
    VERSION=define("VERSION_LATEST"); // -DVERSION=VERSION_LATEST

    // struct a { struct { int b } _meow; };
    //            ^^^^^^^^^^^^^^^^ - This is what the a$_meow type refers to
    rename(a$_meow, FancyAInnerStruct);
    rename_prefix(Ray, ray__); // RayInitWindow -> ray__InitWindow
    remove_prefix(ray__); // ray__InitWindow -> InitWindow
    rename_suffix(_t, _type); // int32_t -> int32_type
    remove_suffix(_type); // int32_type -> int32
    transform_identifier(struct, to_pascal_case); // typedef struct { ... } my_struct -> typedef struct { ... } MyStruct
    transform_identifier(enum, to_pascal_case); // typedef enum { ... } my_enum -> typedef enum { ... } MyEnum
    transform_identifier(enum_member, to_pascal_case); // enum { downloading_file; downloading_folder } -> enum { DownloadingFile, DownloadingFolder }
    transform_identifier(union, to_pascal_case); // union { ... } my_union -> union { ... } MyUnion
    transform_identifier(function, to_snake_case, lowercase); // int LaunchGame(...) -> int launch_game(...)
    transform_identifier(static, to_snake_case, uppercase); // bool HasPanicked -> bool has_panicked

    import("./raylib.h", c_header); // runs `echo "#include \"./raylib.h\"" | cc -x c -H -fsyntax-only -` and `echo "#include \"./raylib.h\"" | cc -x c -E -`
    link("raylib"); // links with raylib (e.g. libraylib.a on linux)
};
```
