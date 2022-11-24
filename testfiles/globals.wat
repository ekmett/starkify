(module
  (type (;0;) (func (result i64)))
  (func (;0;) (type 0) (result i64)
    global.get 0
    global.get 1
    i32.add
    i64.extend_i32_u
    global.get 2
    global.get 5
    i64.add
    i64.add
  )
  (global (;0;) (mut i32) (i32.const 1))
  (global (;1;) i32 (i32.const 2))
  (global (;2;) i64 (i64.const 3))
  (global (;3;) i32 (i32.const 4))
  (global (;4;) i64 (i64.const 5))
  (global (;5;) i64 (i64.const 6))
  (memory (;0;) 2)
  (export "main" (func 0))
)
