#!/bin/bash
FILES=tests/*.tgr

let runs=0
let passed=0
let failed=0

echo "Generating tokenss for tests"
for f in $FILES
do
  let runs++
  #echo "Processing $f to ${f%.tgr}.tokens"
  # generate tokens file
  ./compiler $f --tokens > ${f%.tgr}.tokens
  # generate diff file
  diff -iw ${f%.tgr}_correct.tokens ${f%.tgr}.tokens > ${f%.tgr}.diff
  # if diff file is non-empty (indicating a difference)
  if [ -s ${f%.tgr}.diff ]
  then
    echo "The tokens for ${f%.tgr} differs:"
    diffstat ${f%.tgr}.diff
    let failed++
  else
    let passed++
  fi
done

echo "passed: $passed, failed: $failed, your grade: $passed/$runs"
