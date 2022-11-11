#!/usr/bin/env sh

set -e

cprog=$1

echo "Building starkify..."
cabal build exe:starkify

echo "Source: $cprog"
echo "--- C program ---"
cat testfiles/$cprog
echo "-----------------"
echo ""

masmout="/tmp/$cprog.masm"
echo "Running starkify..."
$(cabal list-bin exe:starkify) build \
	-i testfiles/$cprog -o $masmout \
	--dump-wasm --dump-masm

compile_cmd="miden compile --assembly $masmout"
echo "[info] Compiling Miden Assembly: $compile_cmd ..."
$compile_cmd | tee /tmp/$cprog.masm.compile.out
prog_hash=$(grep "program hash" /tmp/$cprog.masm.compile.out | cut -d' ' -f4)
echo "[info] hash = $prog_hash"
midenout="/tmp/$cprog.masm.out"
run_cmd="miden run --assembly $masmout -o $midenout"
echo "[info] Running Miden program: $run_cmd ..."
$run_cmd

echo "Program output: $midenout"
echo "-------------"
cat $midenout
echo ""
echo "-------------"

proof="/tmp/$cprog.masm.proof"
prove_cmd="miden prove --assembly $masmout -o $midenout -p $proof"
echo "[info] Generating proof of program execution: $prove_cmd ..."
$prove_cmd
verify_cmd="miden verify -p $proof -o $midenout -h $prog_hash"
echo "[info] Verify generated proof: $verify_cmd ..."
$verify_cmd
