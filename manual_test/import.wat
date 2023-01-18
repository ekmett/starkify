(module
  (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "fd_read" (func (;0;) (type 0)))
  (func $main (result i32)
    i32.const 0
    i32.const 0
    i32.const 0
    i32.const 0
    call 0
  )
  (export "main" (func $main)))
