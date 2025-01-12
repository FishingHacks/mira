## ABI

1 - structs are guaranteed to have their fields in the order they're specified in the code and alignment is being done by llvm.
2 - the vtables are a struct containing a **nullable**(!) pointer to the drop method of the function and the size of the value in __bytes__ as a `usize`. After that, it contains a bunch of function pointer to the traits methods in the order of their definition. If a dyn trait has multiple trait definitions or trait bounds, the bounds will be evaluated in order immediately after the trait, recursively.
Example:
```rs
trait Meow { fn meow(self: &Self) -> &str; }
trait A: Meow + E { fn test(self: &Self); }
trait B: A + D { fn b_test(self: &Self); }
trait C { fn bark(self: &Self) -> &str; }
trait D { fn d_test(self: &Self); }
trait E { fn e_test(self: &Self); }

// vtable for: dyn C + B
// (function pointer to the drop method (or null), the size of the type as a usize, C::bark, B::b_test, A::test, Meow::meow, E::e_test, D::d_test)
//  ^ drop                                         ^ size                           ^ dyn C  ^ dyn B    ^ B: A   ^ A: Meow   A: E       ^ B: D
```

3 - closure; they are a struct which first &lt;pointer size&gt; bytes are a function pointer to the closure which is accepting a reference to the struct (in case of `|| fn() -> _`) or the struct (in case of `|| move fn() -> _`) as the first argument, followed by the defined arguments in the closure body. After that, the closure's elements follow in the order they're defined in. Turning a `|| fn() -> _`-style closure into a `&dyn fn() -> _` gives you a vtable which has *no* traits. Calling the closure is being done by completely disregarding the vtable and treating the value as a `&|| fn() -> _`, so dereferencing the first &lt;pointer size&gt; bytes of the pointer and calling that with the pointer and the rest arguments.

> **Note**: While a `dyn fn() -> _` does not "implement" any traits, any closure which contains exclusively copyable values can be copied.

Example:
```ll
; fn call(v: &dyn fn (u32, u32) -> u32, a: u32, b: u32) -> u32
define i32 %call({ ptr, ptr } %v, i32 %a, i32 %b) {
start:
    %environment = extractvalue { ptr, ptr } %v, 0
    %fn_ptr = load ptr, ptr %environment
    %ret = call i32 %fn_ptr(ptr %environment, i32 %a, i32 %b)
    ret i32 %ret
}

; Note: this can be used for any &dyn _
; fn sizeof(v: &dyn fn (..) -> _) -> usize (on a 64-bit machine)
define i64 %sizeof({ ptr, ptr } %v) {
start:
    %vtable = extractvalue { ptr, ptr } %v, 1
    %size_ptr = getelementptr { ptr, i64 }, ptr %vtable, i64 0, i64 1
    %size = load i64, ptr %size
    ret i64 %size
}

; Note: this can be used for any &dyn _
; fn drop_in_place(v: &dyn fn (..) -> _)
define void %drop_in_place({ ptr, ptr } %v) {
start:
    %value = extractvalue { ptr, ptr } %v, 0
    %vtable = extractvalue { ptr, ptr } %v, 1
    %drop_fn_ptr = load ptr, ptr %vtable
    call void %drop_fn_ptr(ptr %value)
    ret void
}
```
