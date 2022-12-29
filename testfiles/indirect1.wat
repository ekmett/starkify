(module
    (table 2 funcref)
    (type (func (result i32)))
    (func $g (param $x i32) (param $fun i32) (result i32)
        (i32.mul
            (call_indirect (type 0) (local.get $fun))
            (local.get $x)
        )
    )
    (func (export "main") (result i32)
        (local $fun i32)

        (local.set $fun (i32.const 0))
        (call $g (i32.const 10) (local.get $fun))

        (local.set $fun (i32.const 1))
        (call $g (i32.const 10) (local.get $fun))

        i32.mul ;; we should get 600
    )
    (func $f1 (result i32)
        i32.const 3
    )
    (func $f2 (result i32)
        i32.const 2
    )
    (elem (i32.const 0) $f1 $f2)
)