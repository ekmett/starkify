(module
  (func (result i32)
    i32.const 3
    i32.const 2
    i32.const 1
    select
  )
  (func $main (result i32)
    call 0)
  (export "main" (func $main)))
