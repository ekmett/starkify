(module
  (func $main (result i32 i32)
    (block
      i32.const 4
      i32.const 3
      i32.const 2
      return
    )
    ;; Without `unreachable` here, our stack type checker would complain.
    ;; And rightly so.
    unreachable
  )
  (export "main" (func $main)))
