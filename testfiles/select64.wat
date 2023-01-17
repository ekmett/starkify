(module
  (func (result i64)
    i64.const 3
    i64.const 2
    i32.const 0
    select
  )
  (func $main (result i64)
    call 0)
  (export "main" (func $main)))
