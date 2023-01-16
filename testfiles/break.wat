(module
  (func $main (result i32)
    i32.const 2
    (block (result i32)
      i32.const 5
      (block (result i32)
        i32.const 3
        br 1)
      i32.add)
    i32.add)
  (export "main" (func $main)))
