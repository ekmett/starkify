(module
  (type (;0;) (func (result i64)))
  (func (;0;) (type 0) (result i64)
    global.get 0
  )
  (global (;0;) (mut i64) (i64.const 123))
  (memory (;0;) 2)
  (export "main" (func 0))
)
