(module
  (type (;0;) (func (result i64)))
  (func (;0;) (type 0) (result i64)
    i32.const -1
    i64.extend_i32_s
    i32.const 2
    i64.extend_i32_s
    i64.add
    i64.const -1
    i64.mul
  )
  (memory (;0;) 2)
  (export "main" (func 0))
)
