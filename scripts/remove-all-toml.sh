#!/bin/bash

# List all .toml files in the current directory
files=$(find . -name "*.toml")

# Loop through each file and remove it from the Git repository
for file in $files; do
    git rm --cached "$file"
done