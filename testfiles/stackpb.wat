(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param i32 i32)))
  (type (;2;) (func (param i32 i32 i32 i32 i32)))
  (type (;3;) (func (result i32)))
  (func (;0;) (type 3) (result i32)
    i32.const 13
    i32.const 37
    call 1
    i32.const 12
  )
  (func (;1;) (type 1) (param i32 i32)
                ;; []
    local.get 0 ;; [i32]
    local.get 0 ;; [i32, i32]
    call 2      ;; [i32, i32]
    local.get 0 ;; [i32, i32, i32]
    call 2      ;; [i32, i32, i32]
    local.get 0 ;; [i32, i32, i32, i32]
    call 3      ;; [i32, i32, i32, i32]
    i32.const 3 ;; [i32, i32, i32, i32, i32]
    i32.shl     ;; [i32, i32, i32, i32]
    i32.add     ;; [i32, i32, i32]
    local.get 0 ;; [i32, i32, i32, i32]
    call 2      ;; [i32, i32, i32, i32]
    local.get 1 ;; [i32, i32, i32, i32, i32]
    i32.const 3 ;; [i32, i32, i32, i32, i32, i32]
    i32.shl     ;; [i32, i32, i32, i32, i32]
    i32.add     ;; [i32, i32, i32, i32]
    local.get 0 ;; [i32, i32, i32, i32, i32]
    call 2      ;; [i32, i32, i32, i32, i32]
    local.get 0 ;; [i32, i32, i32, i32, i32, i32]
    call 5      ;; [i32, i32, i32, i32, i32, i32]
    i32.const 3 ;; [i32, i32, i32, i32, i32, i32, i32]
    i32.shl     ;; [i32, i32, i32, i32, i32, i32]
    i32.add     ;; [i32, i32, i32, i32, i32]
    call 4      ;; []
  )
  (func (;2;) (type 0) (param i32) (result i32)
    local.get 0
  )
  (func (;3;) (type 0) (param i32) (result i32)
    local.get 0
  )
  (func (;4;) (type 2) (param i32 i32 i32 i32 i32)
  )
  (func (;5;) (type 0) (param i32) (result i32)
    local.get 0
  )
  (export "main" (func 0))
  (export "foo" (func 1))
  (export "f1" (func 2))
  (export "f2" (func 3))
  (export "f3" (func 4))
  (export "f4" (func 5))
)
