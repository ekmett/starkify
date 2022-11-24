(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0) (result i32)
    i32.const 12
    i32.const 4
    i32.store8 offset=3
    i32.const 12
    i32.load8_u offset=3
  )
  (memory (;0;) 2)
  (export "main" (func 0))
)
