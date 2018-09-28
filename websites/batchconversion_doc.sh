#!/bin/bash
shopt -s globstar

for f in ./websites/**/*.doc; do
antiword "$f" > "${f%.*}.txt"
done
