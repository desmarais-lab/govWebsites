#!/bin/bash
shopt -s globstar

for f in ./**/*.doc; do
antiword "$f" > "${f%.*}.txt" 
done