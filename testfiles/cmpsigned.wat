(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0) (result i32)
    i32.const -1
    i32.const 1
    i32.lt_s ;; 1, because -1 < 1

    i32.const -1
    i32.gt_s ;; 1, because 1 > -1

    i32.const 5
    i32.ge_s ;; 0, because 1 < 5

    i32.const -1
    i32.ge_s ;; 1, because 0 >= -1
  )
  (memory (;0;) 2)
  (export "main" (func 0))
)
