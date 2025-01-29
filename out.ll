; ModuleID = 'stdin_buffer'
source_filename = "stdin_buffer"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@str_4d5187b5c702b216 = internal global [3 x i8] c"%d\00"

declare void @printf(ptr, ...) local_unnamed_addr

define void @main(i64 %0, ptr nocapture readnone %1) local_unnamed_addr {
entry_4:
  tail call void (ptr, ...) @printf(ptr nonnull @str_4d5187b5c702b216, i32 -12)
  ret void
}

!llvm.dbg.cu = !{!0}

!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "clang LLVM (mira version 0.1.0)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false)
!1 = !DIFile(filename: "stdin_buffer", directory: "/home/fishi/rust/miralang")
