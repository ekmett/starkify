(module
  (func $main (result i64 i64)
    i64.const 0x0001000100110400
    i64.popcnt
    i64.const 0xffffffffffffffff
    i64.popcnt
  )
  (export "main" (func $main))
)
