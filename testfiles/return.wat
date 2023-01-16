(module
  (func (result i32 i32)
    i32.const 5
    (block
      i32.const 4
      i32.const 3
      i32.const 2
      return
      )
    i32.const 6)
  (func $main (result i32 i32)
    call 0)
  (export "main" (func $main)))
