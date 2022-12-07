(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0) (result i32)
    i32.const -1

    i32.const 0
    i32.const -1
    i32.store

    i32.const 0
    i32.load8_s

    i32.add
  )
  (memory (;0;) 2)
  (export "main" (func 0))
)
