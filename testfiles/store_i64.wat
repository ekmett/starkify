(module
  (type (;0;) (func (result i64)))
  (func (;0;) (type 0) (result i64)
    i32.const 8
    i64.const 23
    i64.store offset=0

    i32.const 16
    i64.const 13
    i64.store offset=0

    i32.const 8
    i64.load offset=0

    i32.const 16
    i64.load offset=0

    i64.add
  )
  (memory (;0;) 2)
  (export "main" (func 0))
)
