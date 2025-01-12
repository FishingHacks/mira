; fn call(v: &dyn fn (u32, u32) -> u32, a: u32, b: u32) -> u32
define i32 @call({ ptr, ptr } %v, i32 %a, i32 %b) {
start:
    %environment = extractvalue { ptr, ptr } %v, 0
    %fn_ptr = load ptr, ptr %environment
    %ret = call i32 %fn_ptr(ptr %environment, i32 %a, i32 %b)
    ret i32 %ret
}

define i64 @sizeof::<[T]>({ ptr, i64 } %v) {
start:
    %count = extractvalue { ptr, i64 } %v, 1
    %el_size = call i64 @sizeof::<T>()
    %size = mul nuw i64 %count, %el_size ; sizeof::<[T]>(v: T, length: usize) = length * sizeof::<T>();
    ret i64 %size
}

; Note: this can be used for any &dyn _
; fn sizeof(v: &dyn fn (..) -> _) -> usize (on a 64-bit machine)
define i64 @sizeof({ ptr, ptr } %v) {
start:
    %vtable = extractvalue { ptr, ptr } %v, 1
    %size_ptr = getelementptr { ptr, i64 }, ptr %vtable, i64 0, i32 1
    %size = load i64, ptr %size_ptr
    ret i64 %size
}

; Note: this can be used for any &dyn _
; fn drop_in_place(v: &dyn fn (..) -> _)
define void @drop_in_place({ ptr, ptr } %v) {
start:
    %value = extractvalue { ptr, ptr } %v, 0
    %vtable = extractvalue { ptr, ptr } %v, 1
    %drop_fn_ptr = load ptr, ptr %vtable
    call void %drop_fn_ptr(ptr %value)
    ret void
}
