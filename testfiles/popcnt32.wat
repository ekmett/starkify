(module
  (func $main (result i32 i32)
    i32.const 555
    i32.popcnt
    i32.const 65536
    i32.popcnt
  )
  (export "main" (func $main))
)
