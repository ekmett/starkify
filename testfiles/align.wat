(module
  (type (;0;) (func (result i64)))
  (func (;0;) (type 0) (result i64)
    i32.const 0
    i64.load align=1
  )
  (memory (;0;) 2)
  (export "main" (func 0))
)
