;; .out for this was computed with:
;; ghci> let f_i i x = i*x + 1
;; ghci> go i acc = if i > 100 then acc else go (i+1) (f_i i acc)
;; ghci> go 1 0 :: Data.Word.Word32
;; 1180553153
(module
    (table 100 funcref)
    (type (func (param i32) (result i32)))
    (func $g (param $x i32) (param $fun i32) (result i32)
        local.get $x
        local.get $fun
        call_indirect (type 0)
        i32.const 1
        i32.add
    )
    (func (export "main") (result i32)
        i32.const 0 ;; initial x

        (i32.const 0) (call $g) ;; f1(x)
        (i32.const 1) (call $g) ;; f2(f1(x))
        (i32.const 2) (call $g) ;; ...
        (i32.const 3) (call $g)
        (i32.const 4) (call $g)
        (i32.const 5) (call $g)
        (i32.const 6) (call $g)
        (i32.const 7) (call $g)
        (i32.const 8) (call $g)
        (i32.const 9) (call $g)
        (i32.const 10) (call $g)
        (i32.const 11) (call $g)
        (i32.const 12) (call $g)
        (i32.const 13) (call $g)
        (i32.const 14) (call $g)
        (i32.const 15) (call $g)
        (i32.const 16) (call $g)
        (i32.const 17) (call $g)
        (i32.const 18) (call $g)
        (i32.const 19) (call $g)
        (i32.const 20) (call $g)
        (i32.const 21) (call $g)
        (i32.const 22) (call $g)
        (i32.const 23) (call $g)
        (i32.const 24) (call $g)
        (i32.const 25) (call $g)
        (i32.const 26) (call $g)
        (i32.const 27) (call $g)
        (i32.const 28) (call $g)
        (i32.const 29) (call $g)
        (i32.const 30) (call $g)
        (i32.const 31) (call $g)
        (i32.const 32) (call $g)
        (i32.const 33) (call $g)
        (i32.const 34) (call $g)
        (i32.const 35) (call $g)
        (i32.const 36) (call $g)
        (i32.const 37) (call $g)
        (i32.const 38) (call $g)
        (i32.const 39) (call $g)
        (i32.const 40) (call $g)
        (i32.const 41) (call $g)
        (i32.const 42) (call $g)
        (i32.const 43) (call $g)
        (i32.const 44) (call $g)
        (i32.const 45) (call $g)
        (i32.const 46) (call $g)
        (i32.const 47) (call $g)
        (i32.const 48) (call $g)
        (i32.const 49) (call $g)
        (i32.const 50) (call $g)
        (i32.const 51) (call $g)
        (i32.const 52) (call $g)
        (i32.const 53) (call $g)
        (i32.const 54) (call $g)
        (i32.const 55) (call $g)
        (i32.const 56) (call $g)
        (i32.const 57) (call $g)
        (i32.const 58) (call $g)
        (i32.const 59) (call $g)
        (i32.const 60) (call $g)
        (i32.const 61) (call $g)
        (i32.const 62) (call $g)
        (i32.const 63) (call $g)
        (i32.const 64) (call $g)
        (i32.const 65) (call $g)
        (i32.const 66) (call $g)
        (i32.const 67) (call $g)
        (i32.const 68) (call $g)
        (i32.const 69) (call $g)
        (i32.const 70) (call $g)
        (i32.const 71) (call $g)
        (i32.const 72) (call $g)
        (i32.const 73) (call $g)
        (i32.const 74) (call $g)
        (i32.const 75) (call $g)
        (i32.const 76) (call $g)
        (i32.const 77) (call $g)
        (i32.const 78) (call $g)
        (i32.const 79) (call $g)
        (i32.const 80) (call $g)
        (i32.const 81) (call $g)
        (i32.const 82) (call $g)
        (i32.const 83) (call $g)
        (i32.const 84) (call $g)
        (i32.const 85) (call $g)
        (i32.const 86) (call $g)
        (i32.const 87) (call $g)
        (i32.const 88) (call $g)
        (i32.const 89) (call $g)
        (i32.const 90) (call $g)
        (i32.const 91) (call $g)
        (i32.const 92) (call $g)
        (i32.const 93) (call $g)
        (i32.const 94) (call $g)
        (i32.const 95) (call $g)
        (i32.const 96) (call $g)
        (i32.const 97) (call $g)
        (i32.const 98) (call $g)
        (i32.const 99) (call $g)
    )
    (func $f1 (param $x i32) (result i32)
        (i32.mul (i32.const 1) (local.get $x))
    )

    (func $f2 (param $x i32) (result i32)
        (i32.mul (i32.const 2) (local.get $x))
    )

    (func $f3 (param $x i32) (result i32)
        (i32.mul (i32.const 3) (local.get $x))
    )

    (func $f4 (param $x i32) (result i32)
        (i32.mul (i32.const 4) (local.get $x))
    )

    (func $f5 (param $x i32) (result i32)
        (i32.mul (i32.const 5) (local.get $x))
    )

    (func $f6 (param $x i32) (result i32)
        (i32.mul (i32.const 6) (local.get $x))
    )

    (func $f7 (param $x i32) (result i32)
        (i32.mul (i32.const 7) (local.get $x))
    )

    (func $f8 (param $x i32) (result i32)
        (i32.mul (i32.const 8) (local.get $x))
    )

    (func $f9 (param $x i32) (result i32)
        (i32.mul (i32.const 9) (local.get $x))
    )

    (func $f10 (param $x i32) (result i32)
        (i32.mul (i32.const 10) (local.get $x))
    )

    (func $f11 (param $x i32) (result i32)
        (i32.mul (i32.const 11) (local.get $x))
    )

    (func $f12 (param $x i32) (result i32)
        (i32.mul (i32.const 12) (local.get $x))
    )

    (func $f13 (param $x i32) (result i32)
        (i32.mul (i32.const 13) (local.get $x))
    )

    (func $f14 (param $x i32) (result i32)
        (i32.mul (i32.const 14) (local.get $x))
    )

    (func $f15 (param $x i32) (result i32)
        (i32.mul (i32.const 15) (local.get $x))
    )

    (func $f16 (param $x i32) (result i32)
        (i32.mul (i32.const 16) (local.get $x))
    )

    (func $f17 (param $x i32) (result i32)
        (i32.mul (i32.const 17) (local.get $x))
    )

    (func $f18 (param $x i32) (result i32)
        (i32.mul (i32.const 18) (local.get $x))
    )

    (func $f19 (param $x i32) (result i32)
        (i32.mul (i32.const 19) (local.get $x))
    )

    (func $f20 (param $x i32) (result i32)
        (i32.mul (i32.const 20) (local.get $x))
    )

    (func $f21 (param $x i32) (result i32)
        (i32.mul (i32.const 21) (local.get $x))
    )

    (func $f22 (param $x i32) (result i32)
        (i32.mul (i32.const 22) (local.get $x))
    )

    (func $f23 (param $x i32) (result i32)
        (i32.mul (i32.const 23) (local.get $x))
    )

    (func $f24 (param $x i32) (result i32)
        (i32.mul (i32.const 24) (local.get $x))
    )

    (func $f25 (param $x i32) (result i32)
        (i32.mul (i32.const 25) (local.get $x))
    )

    (func $f26 (param $x i32) (result i32)
        (i32.mul (i32.const 26) (local.get $x))
    )

    (func $f27 (param $x i32) (result i32)
        (i32.mul (i32.const 27) (local.get $x))
    )

    (func $f28 (param $x i32) (result i32)
        (i32.mul (i32.const 28) (local.get $x))
    )

    (func $f29 (param $x i32) (result i32)
        (i32.mul (i32.const 29) (local.get $x))
    )

    (func $f30 (param $x i32) (result i32)
        (i32.mul (i32.const 30) (local.get $x))
    )

    (func $f31 (param $x i32) (result i32)
        (i32.mul (i32.const 31) (local.get $x))
    )

    (func $f32 (param $x i32) (result i32)
        (i32.mul (i32.const 32) (local.get $x))
    )

    (func $f33 (param $x i32) (result i32)
        (i32.mul (i32.const 33) (local.get $x))
    )

    (func $f34 (param $x i32) (result i32)
        (i32.mul (i32.const 34) (local.get $x))
    )

    (func $f35 (param $x i32) (result i32)
        (i32.mul (i32.const 35) (local.get $x))
    )

    (func $f36 (param $x i32) (result i32)
        (i32.mul (i32.const 36) (local.get $x))
    )

    (func $f37 (param $x i32) (result i32)
        (i32.mul (i32.const 37) (local.get $x))
    )

    (func $f38 (param $x i32) (result i32)
        (i32.mul (i32.const 38) (local.get $x))
    )

    (func $f39 (param $x i32) (result i32)
        (i32.mul (i32.const 39) (local.get $x))
    )

    (func $f40 (param $x i32) (result i32)
        (i32.mul (i32.const 40) (local.get $x))
    )

    (func $f41 (param $x i32) (result i32)
        (i32.mul (i32.const 41) (local.get $x))
    )

    (func $f42 (param $x i32) (result i32)
        (i32.mul (i32.const 42) (local.get $x))
    )

    (func $f43 (param $x i32) (result i32)
        (i32.mul (i32.const 43) (local.get $x))
    )

    (func $f44 (param $x i32) (result i32)
        (i32.mul (i32.const 44) (local.get $x))
    )

    (func $f45 (param $x i32) (result i32)
        (i32.mul (i32.const 45) (local.get $x))
    )

    (func $f46 (param $x i32) (result i32)
        (i32.mul (i32.const 46) (local.get $x))
    )

    (func $f47 (param $x i32) (result i32)
        (i32.mul (i32.const 47) (local.get $x))
    )

    (func $f48 (param $x i32) (result i32)
        (i32.mul (i32.const 48) (local.get $x))
    )

    (func $f49 (param $x i32) (result i32)
        (i32.mul (i32.const 49) (local.get $x))
    )

    (func $f50 (param $x i32) (result i32)
        (i32.mul (i32.const 50) (local.get $x))
    )

    (func $f51 (param $x i32) (result i32)
        (i32.mul (i32.const 51) (local.get $x))
    )

    (func $f52 (param $x i32) (result i32)
        (i32.mul (i32.const 52) (local.get $x))
    )

    (func $f53 (param $x i32) (result i32)
        (i32.mul (i32.const 53) (local.get $x))
    )

    (func $f54 (param $x i32) (result i32)
        (i32.mul (i32.const 54) (local.get $x))
    )

    (func $f55 (param $x i32) (result i32)
        (i32.mul (i32.const 55) (local.get $x))
    )

    (func $f56 (param $x i32) (result i32)
        (i32.mul (i32.const 56) (local.get $x))
    )

    (func $f57 (param $x i32) (result i32)
        (i32.mul (i32.const 57) (local.get $x))
    )

    (func $f58 (param $x i32) (result i32)
        (i32.mul (i32.const 58) (local.get $x))
    )

    (func $f59 (param $x i32) (result i32)
        (i32.mul (i32.const 59) (local.get $x))
    )

    (func $f60 (param $x i32) (result i32)
        (i32.mul (i32.const 60) (local.get $x))
    )

    (func $f61 (param $x i32) (result i32)
        (i32.mul (i32.const 61) (local.get $x))
    )

    (func $f62 (param $x i32) (result i32)
        (i32.mul (i32.const 62) (local.get $x))
    )

    (func $f63 (param $x i32) (result i32)
        (i32.mul (i32.const 63) (local.get $x))
    )

    (func $f64 (param $x i32) (result i32)
        (i32.mul (i32.const 64) (local.get $x))
    )

    (func $f65 (param $x i32) (result i32)
        (i32.mul (i32.const 65) (local.get $x))
    )

    (func $f66 (param $x i32) (result i32)
        (i32.mul (i32.const 66) (local.get $x))
    )

    (func $f67 (param $x i32) (result i32)
        (i32.mul (i32.const 67) (local.get $x))
    )

    (func $f68 (param $x i32) (result i32)
        (i32.mul (i32.const 68) (local.get $x))
    )

    (func $f69 (param $x i32) (result i32)
        (i32.mul (i32.const 69) (local.get $x))
    )

    (func $f70 (param $x i32) (result i32)
        (i32.mul (i32.const 70) (local.get $x))
    )

    (func $f71 (param $x i32) (result i32)
        (i32.mul (i32.const 71) (local.get $x))
    )

    (func $f72 (param $x i32) (result i32)
        (i32.mul (i32.const 72) (local.get $x))
    )

    (func $f73 (param $x i32) (result i32)
        (i32.mul (i32.const 73) (local.get $x))
    )

    (func $f74 (param $x i32) (result i32)
        (i32.mul (i32.const 74) (local.get $x))
    )

    (func $f75 (param $x i32) (result i32)
        (i32.mul (i32.const 75) (local.get $x))
    )

    (func $f76 (param $x i32) (result i32)
        (i32.mul (i32.const 76) (local.get $x))
    )

    (func $f77 (param $x i32) (result i32)
        (i32.mul (i32.const 77) (local.get $x))
    )

    (func $f78 (param $x i32) (result i32)
        (i32.mul (i32.const 78) (local.get $x))
    )

    (func $f79 (param $x i32) (result i32)
        (i32.mul (i32.const 79) (local.get $x))
    )

    (func $f80 (param $x i32) (result i32)
        (i32.mul (i32.const 80) (local.get $x))
    )

    (func $f81 (param $x i32) (result i32)
        (i32.mul (i32.const 81) (local.get $x))
    )

    (func $f82 (param $x i32) (result i32)
        (i32.mul (i32.const 82) (local.get $x))
    )

    (func $f83 (param $x i32) (result i32)
        (i32.mul (i32.const 83) (local.get $x))
    )

    (func $f84 (param $x i32) (result i32)
        (i32.mul (i32.const 84) (local.get $x))
    )

    (func $f85 (param $x i32) (result i32)
        (i32.mul (i32.const 85) (local.get $x))
    )

    (func $f86 (param $x i32) (result i32)
        (i32.mul (i32.const 86) (local.get $x))
    )

    (func $f87 (param $x i32) (result i32)
        (i32.mul (i32.const 87) (local.get $x))
    )

    (func $f88 (param $x i32) (result i32)
        (i32.mul (i32.const 88) (local.get $x))
    )

    (func $f89 (param $x i32) (result i32)
        (i32.mul (i32.const 89) (local.get $x))
    )

    (func $f90 (param $x i32) (result i32)
        (i32.mul (i32.const 90) (local.get $x))
    )

    (func $f91 (param $x i32) (result i32)
        (i32.mul (i32.const 91) (local.get $x))
    )

    (func $f92 (param $x i32) (result i32)
        (i32.mul (i32.const 92) (local.get $x))
    )

    (func $f93 (param $x i32) (result i32)
        (i32.mul (i32.const 93) (local.get $x))
    )

    (func $f94 (param $x i32) (result i32)
        (i32.mul (i32.const 94) (local.get $x))
    )

    (func $f95 (param $x i32) (result i32)
        (i32.mul (i32.const 95) (local.get $x))
    )

    (func $f96 (param $x i32) (result i32)
        (i32.mul (i32.const 96) (local.get $x))
    )

    (func $f97 (param $x i32) (result i32)
        (i32.mul (i32.const 97) (local.get $x))
    )

    (func $f98 (param $x i32) (result i32)
        (i32.mul (i32.const 98) (local.get $x))
    )

    (func $f99 (param $x i32) (result i32)
        (i32.mul (i32.const 99) (local.get $x))
    )

    (func $f100 (param $x i32) (result i32)
        (i32.mul (i32.const 100) (local.get $x))
    )
    (elem (i32.const 0) $f1 $f2 $f3 $f4 $f5 $f6 $f7 $f8 $f9 $f10 $f11 $f12 $f13 $f14 $f15 $f16 $f17 $f18 $f19 $f20 $f21 $f22 $f23 $f24 $f25 $f26 $f27 $f28 $f29 $f30 $f31 $f32 $f33 $f34 $f35 $f36 $f37 $f38 $f39 $f40 $f41 $f42 $f43 $f44 $f45 $f46 $f47 $f48 $f49 $f50 $f51 $f52 $f53 $f54 $f55 $f56 $f57 $f58 $f59 $f60 $f61 $f62 $f63 $f64 $f65 $f66 $f67 $f68 $f69 $f70 $f71 $f72 $f73 $f74 $f75 $f76 $f77 $f78 $f79 $f80 $f81 $f82 $f83 $f84 $f85 $f86 $f87 $f88 $f89 $f90 $f91 $f92 $f93 $f94 $f95 $f96 $f97 $f98 $f99 $f100)
)
