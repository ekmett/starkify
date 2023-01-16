(module
  (func $main (result i32)
    (local $i i32)
    i32.const 1
    local.set $i   ;; $i = 9
    i32.const 0    ;; accumulator = 0
    (loop (param i32) (result i32)
      local.get $i
      i32.add      ;; accumulator += $i
      local.get $i
      i32.const 1
      i32.add
      local.tee $i ;; $i = $i + 1
      i32.const 10
      i32.le_u     ;; $i ≤ 10
      br_if 0))
  (export "main" (func $main)))
