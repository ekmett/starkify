(module
    (table 3 funcref)
    (type (func (result i32)))
    (func $g (param $fun i32) (result i32)
        (call_indirect (type 0) (local.get $fun))
    )
    (func (export "main") (result i32)
        (call $g (i32.const 0)) ;; 1
        (call $g (i32.const 1)) ;; 2

        i32.mul ;; 2

        (call $g (i32.const 2)) ;; 3

        i32.add ;; 5
    )
    (func $f1 (result i32)
        i32.const 1
    )
    (func $f2 (result i32)
        i32.const 2
    )
    (func $f3 (result i32)
        i32.const 3
    )
    (elem (i32.const 0) $f1 $f2 $f3)
)
