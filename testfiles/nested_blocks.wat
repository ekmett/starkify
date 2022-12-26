(module
  (func $main (result i32 i32)
    i32.const 1
    (loop (param i32) (result i32 i32)
      i32.const 2
      i32.add
      (block (result i32)
        i32.const 3
        i32.const 4
        (loop (param i32) (param i32) (result i32)
          drop
          drop
          i32.const 5))))
  (export "main" (func $main)))
