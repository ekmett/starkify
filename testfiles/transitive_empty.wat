(module
  (type (;0;) (func))
  (func (;0;) (type 0)
    call 1
    i32.const 23
    drop
  )
  (func (;1;) (type 0)
    call 2
  )
  (func (;2;) (type 0)
    ;; If you uncomment the two instructions below, starkify works.
    ;; i32.const 23
    ;; drop
  )
  (memory (;0;) 2)
  (export "main" (func 0))
)
