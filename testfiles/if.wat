(module
  (func (result i32)
    i32.const 1
    (if (result i32)
      (then
        i32.const 2)
      (else
        i32.const 3)))
  (func $main (result i32)
    call 0)
  (export "main" (func $main)))
