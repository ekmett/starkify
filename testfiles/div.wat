(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0) (result i32)
    i32.const 8
    i32.const 2
    i32.div_u ;; 4

    i32.const -1
    i32.div_s ;; -4

    i32.const 2
    i32.shr_s ;; -1
  )
  (memory (;0;) 2)
  (export "main" (func 0))
)
