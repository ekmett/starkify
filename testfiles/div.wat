(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0) (result i32)
    i32.const 8
    i32.const 4
    i32.div_u

    i32.const -1
    i32.div_s
  )
  (memory (;0;) 2)
  (export "main" (func 0))
)
