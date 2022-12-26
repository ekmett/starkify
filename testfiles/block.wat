(module
  (func $main (result i32)
    i32.const 1
    i32.const 2
    (block (param i32) (param i32) (result i32)
      i32.add))
  (export "main" (func $main)))
