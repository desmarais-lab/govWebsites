#!/bin/bash
shopt -s globstar

for f in ./websites/**/*.html; do
html2text -nobs -o "${f%.*}.txt" "$f"
done
