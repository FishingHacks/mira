mod  {
    %exports: {
        "main": "main",
    },
    allocator: Static(
        0,
    ),
    print_usize_hex: Function(
        9,
    ),
    cstrlen: Function(
        12,
    ),
    print_usize: Function(
        10,
    ),
    write: Function(
        14,
    ),
    meow: Function(
        18,
    ),
    CAlloc: Struct(
        0,
    ),
    main: Function(
        4,
    ),
    print_char: Function(
        6,
    ),
    Eq: Trait(
        3,
    ),
    ret_addr: Function(
        3,
    ),
    print_str: Function(
        5,
    ),
    Allocator: Trait(
        0,
    ),
    print_str_newline: Function(
        8,
    ),
    Clone: Trait(
        2,
    ),
    print_newline: Function(
        7,
    ),
    len: Function(
        11,
    ),
    exit: Function(
        13,
    ),
    syscall1: Function(
        15,
    ),
    syscall3: Function(
        16,
    ),
    woof: Function(
        17,
    ),
    Copy: Trait(
        1,
    ),
    Neq: Trait(
        4,
    ),
}

mod  {
    %exports: {},
    main: Function(
        4,
    ),
    __mira_main: ExternalFunction(
        1,
    ),
    _start: ExternalFunction(
        0,
    ),
}

mod  {
    %exports: {
        "woof": "woof",
    },
    woof: Function(
        17,
    ),
}

mod  {
    %exports: {
        "meow": "meow",
    },
    meow: Function(
        18,
    ),
}



FunctionContract {
    name: "free",
    returns: void,
    arguments: (_0: &CAlloc, _1: &void),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    return void,
]

FunctionContract {
    name: "realloc",
    returns: &void,
    arguments: (_0: &CAlloc, _1: &void, _2: usize),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    return _1,
]

FunctionContract {
    name: "alloc",
    returns: &void,
    arguments: (_0: &CAlloc, _1: usize),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    _2 = &void,
    return _2,
]

FunctionContract {
    name: "ret_addr",
    returns: &void,
    arguments: (),
    annotations: Annotations(
        [
            (
                IntrinsicAnnotation(
                    ReturnAddress,
                ),
                Location {
                    line: 35,
                    column: 0,
                    file: "./std.mr",
                },
            ),
        ],
    ),
    module_id: 0,
}[
    _0 = &void,
    return _0,
]

FunctionContract {
    name: "main",
    returns: void,
    arguments: (_0: usize, _1: &&u8),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    block [
        _2 = f_5("meow::meow() = ",),
        _3 = f_18(),
        _4 = f_10(_3,),
        _5 = f_7(),
        _6 = f_5("main = \0",),
        _7 = f_4 as <unknown type T>,
        _8 = _7 as <unknown type T>,
        _9 = f_9(_8,),
        _10 = f_7(),
        _11 = f_5("Got ",),
        _12 = f_10(_0,),
        _13 = f_5(" arguments at ",),
        _14 = _1 as <unknown type T>,
        _15 = _14 as <unknown type T>,
        _16 = f_9(_15,),
        _17 = f_7(),
        _18 = _1 as <unknown type T>,
        _19 = _18 as <unknown type T>,
        let argv_ptr: usize = _19,
        let _20 = 0,
        let i: usize = _20,
        while_cond [
            21 = _20 < _0,
        ]while _21[
            block [
                22 = _20 * 8,
                23 = _19 + _22,
                _24 = _23 as <unknown type T>,
                _25 = _24 as <unknown type T>,
                let ptr: &&u8 = _25,
                _26 = *_25,
                _27 = *_25,
                _28 = f_12(_27,),
                _29 = f_14(1,_26,_28,),
                _30 = f_5(" ",),
                _31 = &_20,
                32 = _20 + 1,
                *_31 = _32,
            ],
        ],
        _33 = f_7(),
    ],
    return void,
]

FunctionContract {
    name: "print_str",
    returns: void,
    arguments: (_0: &str),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    _1 = _0 as <unknown type T>,
    _2 = f_11(_0,),
    _3 = f_14(1,_1,_2,),
    return _3,
]

FunctionContract {
    name: "print_char",
    returns: void,
    arguments: (_0: u8),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    _1 = &_0,
    _2 = f_14(1,_1,1,),
    return _2,
]

FunctionContract {
    name: "print_newline",
    returns: void,
    arguments: (),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    _0 = f_6(10,),
    return _0,
]

FunctionContract {
    name: "print_str_newline",
    returns: void,
    arguments: (_0: &str),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    block [
        _1 = f_5(_0,),
        _2 = f_7(),
    ],
    return void,
]

FunctionContract {
    name: "print_usize_hex",
    returns: void,
    arguments: (_0: usize),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    block [
        _1 = f_5("0x",),
        2 = _0 == 0,
        if _2[
            block [
                _3 = f_5("0",),
                return _3,
            ],
        ],
        let _4 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        let chars: [u8; 16] = _4,
        let _5 = 0,
        let char_idx: usize = _5,
        while_cond [
            6 = _0 > 0,
        ]while _6[
            block [
                7 = _0 & 15,
                _8 = _7 as <unknown type T>,
                let v: u8 = _8,
                9 = _8 < 10,
                if _9[
                    _10 = &_4,
                    _11 = offset(_10, Dynamic(5)),
                    12 = _8 + 48,
                    *_11 = _12,
                ]
                else [
                    _13 = &_4,
                    _14 = offset(_13, Dynamic(5)),
                    15 = _8 - 10,
                    16 = _15 + 97,
                    *_14 = _16,
                ],
                _17 = &_0,
                18 = _0 >> 4,
                *_17 = _18,
                _19 = &_5,
                20 = _5 + 1,
                *_19 = _20,
            ],
        ],
        while_cond [
            21 = _5 > 0,
        ]while _21[
            block [
                _22 = &_5,
                23 = _5 - 1,
                *_22 = _23,
                _24 = &_4,
                _25 = offset(_24, Dynamic(5)),
                _26 = *_25,
                _27 = f_6(_26,),
            ],
        ],
    ],
    return void,
]

FunctionContract {
    name: "print_usize",
    returns: void,
    arguments: (_0: usize),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    block [
        1 = _0 == 0,
        if _1[
            block [
                _2 = f_5("0",),
                return _2,
            ],
        ],
        let _3 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        let chars: [u8; 20] = _3,
        let _4 = 0,
        let char_idx: usize = _4,
        while_cond [
            5 = _0 > 0,
        ]while _5[
            block [
                _6 = &_3,
                _7 = offset(_6, Dynamic(4)),
                8 = _0 % 10,
                _9 = _8 as <unknown type T>,
                10 = _9 + 48,
                *_7 = _10,
                _11 = &_0,
                12 = _0 / 10,
                *_11 = _12,
                _13 = &_4,
                14 = _4 + 1,
                *_13 = _14,
            ],
        ],
        while_cond [
            15 = _4 > 0,
        ]while _15[
            block [
                _16 = &_4,
                17 = _4 - 1,
                *_16 = _17,
                _18 = &_3,
                _19 = offset(_18, Dynamic(4)),
                _20 = *_19,
                _21 = f_6(_20,),
            ],
        ],
    ],
    return void,
]

FunctionContract {
    name: "len",
    returns: usize,
    arguments: (_0: &str),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    block [
        _1 = &_0,
        _2 = _1 as <unknown type T>,
        _3 = _2 as <unknown type T>,
        let strn_addr: usize = _3,
        _4 = asm ("movq 8(${1}), ${0}"
        : "=r,r": (_3)
        ),
        return _4,
    ],
]

FunctionContract {
    name: "cstrlen",
    returns: usize,
    arguments: (_0: &u8),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    block [
        let _1 = 0,
        let len: usize = _1,
        while_cond [
            _2 = *_0,
            3 = _2 != 0,
        ]while _3[
            block [
                _4 = &_1,
                5 = _1 + 1,
                *_4 = _5,
                _6 = &_0,
                _7 = _0 as <unknown type T>,
                _8 = _7 as <unknown type T>,
                9 = _8 + 1,
                _10 = _9 as <unknown type T>,
                _11 = _10 as <unknown type T>,
                *_6 = _11,
            ],
        ],
        return _1,
    ],
]

FunctionContract {
    name: "exit",
    returns: !,
    arguments: (_0: u32),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    block [
        _1 = _0 as <unknown type T>,
        _2 = f_15(60,_1,),
        while_cond []while true[
            block [],
        ],
        unreachable,
    ],
]

FunctionContract {
    name: "write",
    returns: void,
    arguments: (_0: u32, _1: &u8, _2: usize),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    block [
        _3 = _0 as <unknown type T>,
        _4 = _1 as <unknown type T>,
        _5 = _4 as <unknown type T>,
        _6 = f_16(1,_3,_5,_2,),
    ],
    return void,
]

FunctionContract {
    name: "syscall1",
    returns: usize,
    arguments: (_0: u32, _1: usize),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    block [
        _2 = asm volatile ("syscall"
        : "={rax},{rax},{rdi},~{rcx},~{r11},~{memory}": (_0, _1)
        ),
        return _2,
    ],
]

FunctionContract {
    name: "syscall3",
    returns: usize,
    arguments: (_0: u32, _1: usize, _2: usize, _3: usize),
    annotations: Annotations(
        [],
    ),
    module_id: 0,
}[
    block [
        _4 = asm volatile ("syscall"
        : "={rax},{rax},{rdi},{rsi},{rdx},~{rcx},~{r11},~{memory}": (_0, _1, _2, _3)
        ),
        return _4,
    ],
]

FunctionContract {
    name: "woof",
    returns: i32,
    arguments: (_0: i32),
    annotations: Annotations(
        [],
    ),
    module_id: 2,
}[
    return _0,
]

FunctionContract {
    name: "meow",
    returns: usize,
    arguments: (),
    annotations: Annotations(
        [],
    ),
    module_id: 3,
}[
    return 3,
]



ExternalFunctionContract {
    name: "_start",
    returns: void,
    arguments: (),
    annotations: Annotations(
        [
            (
                Naked,
                Location {
                    line: 2,
                    column: 0,
                    file: "./start.mr",
                },
            ),
        ],
    ),
    module_id: 1,
}[
    block [
        _0 = asm (".cfi_undefined %rip"
        "xorl %ebp, %ebp"
        "popq %rdi"
        "movq %rsp, %rsi"
        "andq $$-16, %rsp"
        "call __mira_main"
        : "~{ebp},~{rdi},~{rsp},~{rsi}": ()
        ),
    ],
    return void,
]

ExternalFunctionContract {
    name: "__mira_main",
    returns: !,
    arguments: (_0: usize, _1: &&u8),
    annotations: Annotations(
        [
            (
                C,
                Location {
                    line: 15,
                    column: 0,
                    file: "./start.mr",
                },
            ),
        ],
    ),
    module_id: 1,
}[
    block [
        _2 = f_4(_0,_1,),
        let _3 = 0,
        let exit_code: i32 = _3,
        let _4 = 60,
        let num: i32 = _4,
        _5 = asm volatile ("syscall"
        : "={rax},{rax},{rdi},~{rcx},~{r11},~{memory}": (_4, _3)
        ),
        while_cond []while true[
            _6 = asm volatile ("hlt"
            : "": ()
            ),
        ],
        unreachable,
    ],
]



struct {
    name: "CAlloc",
    annotations: Annotations(
        [],
    ),
    global_impl: {},
    trait_impl: {
        0: [
            2,
            1,
            0,
        ],
    },
    elements: [],
    module_id: 0,
}



