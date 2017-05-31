#!/bin/bash
shopt -s globstar

for f in ./**/*.html; do
html2text -o "${f%.*}.txt" "$f"
done