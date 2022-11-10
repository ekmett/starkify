(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0) (result i32)
    i32.const 4
    i32.const 23
    i32.store offset=0

    i32.const 8
    i32.const 13
    i32.store offset=0

    i32.const 4
    i32.load offset=0

    i32.const 8
    i32.load offset=0

    i32.add
  )
  (memory (;0;) 2)
  (export "main" (func 0))
)
