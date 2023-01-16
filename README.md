This is the hone of the starkify project, compiling WASM to STARKs.

# Commands

``` sh
$ nix-shell
$ cabal run starkify -- --help
[...]
Usage: starkify COMMAND

  starkify - WASM to Miden VM compiler

Available options:
  -h,--help                Show this help text

Available commands:
  build                    Compile C or WASM to MASM
  run                      Run MASM programs and generate proofs
  verify                   Verify a proof

$ cabal run starkify -- build --help
Up to date
Usage: starkify build (-i|--input FILE) (-o|--output FILE) [-w|--dump-wasm]
                      [--dump-wasm-ast] [-m|--dump-masm] [--dump-masm-ast]
                      [-r|--run] [-v|--verify]

  Compile C or WASM to MASM

Available options:
  -i,--input FILE          path to .c or .wat (textual WASM) input file to
                           compile
  -o,--output FILE         path to .masm output file
  -w,--dump-wasm           dump textual WASM code
  --dump-wasm-ast          dump the Haskell structures representing the WASM
                           syntax tree
  -m,--dump-masm           dump textual WASM code
  --dump-masm-ast          dump textual WASM code
  -r,--run                 run the program as well, generate a proof of
                           execution along the way
  -v,--verify              verify the proof of execution for the program as well
  -h,--help                Show this help text

$ cabal run starkify -- run --help
Up to date
Usage: starkify run (-i|--input FILE) (-p|--proof FILE) (-o|--output FILE)
                    [-v|--verify]

  Run MASM programs and generate proofs

Available options:
  -i,--input FILE          path to .masm file to run
  -p,--proof FILE          path to a proof file to be produced, the proof is
                           only generated when this option is set
  -o,--output FILE         path to file in which to store program output
  -v,--verify              verify the proof of execution for the program as well
  -h,--help                Show this help text

$ cabal run starkify -- verify --help
Up to date
Usage: starkify verify (-p|--proof FILE) (-o|--output FILE) (-h|--hash HASH)

  Verify a proof

Available options:
  -p,--proof FILE          path to the proof to verify
  -o,--output FILE         path to the output of the program for the run we want
                           to verify
  -h,--hash HASH           hash of the program
  -h,--help                Show this help text
```

# Example

``` sh
$ cabal run starkify -- build -i testfiles/struct.c -o /tmp/blah.masm --dump-wasm --dump-masm --run --verify
Up to date
------------------------------------------------------------
| WASM code (/private/tmp/struct.c48917-0.wasm48917-1.wat) |
------------------------------------------------------------
| (module                                                  |
|   (type (;0;) (func))                                    |
|   (type (;1;) (func (param i32) (result i32)))           |
|   (type (;2;) (func (result i32)))                       |
|   (type (;3;) (func (param i32 i32) (result i32)))       |
|   (func (;0;) (type 0))                                  |
|   (func (;1;) (type 1) (param i32) (result i32)          |
|     (local i32)                                          |
|     local.get 0                                          |
|     i32.load offset=4                                    |
|     local.tee 1                                          |
|     local.get 1                                          |
|     i32.mul                                              |
|     local.get 0                                          |
|     i32.load                                             |
|     local.tee 0                                          |
|     local.get 0                                          |
|     i32.mul                                              |
|     i32.add)                                             |
|   (func (;2;) (type 2) (result i32)                      |
|     (local i32 i32)                                      |
|     global.get 0                                         |
|     i32.const 16                                         |
|     i32.sub                                              |
|     local.tee 0                                          |
|     global.set 0                                         |
|     local.get 0                                          |
|     i64.const 4294967298                                 |
|     i64.store offset=8                                   |
|     local.get 0                                          |
|     i32.const 8                                          |
|     i32.add                                              |
|     call 1                                               |
|     local.set 1                                          |
|     local.get 0                                          |
|     i32.const 16                                         |
|     i32.add                                              |
|     global.set 0                                         |
|     local.get 1)                                         |
|   (func (;3;) (type 3) (param i32 i32) (result i32)      |
|     call 2)                                              |
|   (table (;0;) 1 1 funcref)                              |
|   (memory (;0;) 2)                                       |
|   (global (;0;) (mut i32) (i32.const 66560))             |
|   (global (;1;) i32 (i32.const 1024))                    |
|   (global (;2;) i32 (i32.const 1024))                    |
|   (global (;3;) i32 (i32.const 1024))                    |
|   (global (;4;) i32 (i32.const 66560))                   |
|   (global (;5;) i32 (i32.const 0))                       |
|   (global (;6;) i32 (i32.const 1))                       |
|   (export "memory" (memory 0))                           |
|   (export "__wasm_call_ctors" (func 0))                  |
|   (export "sq" (func 1))                                 |
|   (export "__original_main" (func 2))                    |
|   (export "main" (func 3))                               |
|   (export "__main_void" (func 2))                        |
|   (export "__indirect_function_table" (table 0))         |
|   (export "__dso_handle" (global 1))                     |
|   (export "__data_end" (global 2))                       |
|   (export "__global_base" (global 3))                    |
|   (export "__heap_base" (global 4))                      |
|   (export "__memory_base" (global 5))                    |
|   (export "__table_base" (global 6)))                    |
------------------------------------------------------------

------------------------------
| MASM code (/tmp/blah.masm) |
------------------------------
| use.std::sys               |
| use.std::math::u64         |
| proc.sq.2                  |
|   loc_store.0              |
|   drop                     |
|   loc_load.0               |
|   push.4                   |
|   u32checked_div           |
|   push.1                   |
|   u32checked_add           |
|   push.7                   |
|   u32checked_add           |
|   mem_load                 |
|   loc_store.1              |
|   drop                     |
|   loc_load.1               |
|   loc_load.1               |
|   u32checked_mul           |
|   loc_load.0               |
|   push.4                   |
|   u32checked_div           |
|   push.0                   |
|   u32checked_add           |
|   push.7                   |
|   u32checked_add           |
|   mem_load                 |
|   loc_store.0              |
|   drop                     |
|   loc_load.0               |
|   loc_load.0               |
|   u32checked_mul           |
|   u32checked_add           |
| end                        |
| proc.zz__main_void.2       |
|   mem_load.0               |
|   push.16                  |
|   u32checked_sub           |
|   loc_store.0              |
|   drop                     |
|   loc_load.0               |
|   mem_store.0              |
|   drop                     |
|   loc_load.0               |
|   push.2                   |
|   push.1                   |
|   swap                     |
|   swap.2                   |
|   push.4                   |
|   u32checked_div           |
|   push.2                   |
|   u32checked_add           |
|   push.7                   |
|   u32checked_add           |
|   dup.0                    |
|   swap.2                   |
|   swap                     |
|   push.1                   |
|   u32checked_add           |
|   mem_store                |
|   drop                     |
|   mem_store                |
|   drop                     |
|   loc_load.0               |
|   push.8                   |
|   u32checked_add           |
|   exec.sq                  |
|   loc_store.1              |
|   drop                     |
|   loc_load.0               |
|   push.16                  |
|   u32checked_add           |
|   mem_store.0              |
|   drop                     |
|   loc_load.1               |
| end                        |
| proc.main.2                |
|   loc_store.1              |
|   drop                     |
|   loc_store.0              |
|   drop                     |
|   exec.zz__main_void       |
| end                        |
| begin                      |
|   push.66560               |
|   mem_store.0              |
|   drop                     |
|   push.1024                |
|   mem_store.1              |
|   drop                     |
|   push.1024                |
|   mem_store.2              |
|   drop                     |
|   push.1024                |
|   mem_store.3              |
|   drop                     |
|   push.66560               |
|   mem_store.4              |
|   drop                     |
|   push.0                   |
|   mem_store.5              |
|   drop                     |
|   push.1                   |
|   mem_store.6              |
|   drop                     |
|   exec.main                |
|   exec.sys::truncate_stack |
| end                        |
------------------------------

Execution of program /tmp/blah.masm ...
Successfullt generated proof /tmp/blah.masm.proof
Output of the program stored in /tmp/blah.masm.out
Program hash: 6b3950bf9509d8a34e89296f4829f8ae74bc7d07013b86eecad8ced80f52f5bb
Final state of the stack:
	[5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

Verification of proof /tmp/blah.masm.proof ...
Verification successful
```

# Tests

``` sh
$ cabal run testfiles
Up to date

testfiles/bitwise.c
  gives the same, correct result with wasm and miden [✔]
  can be executed to get a proof which can be successfully verified [✔]
testfiles/branch.c
  gives the same, correct result with wasm and miden [✔]
  can be executed to get a proof which can be successfully verified [✔]
[...]
```
