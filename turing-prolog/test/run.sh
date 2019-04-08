#!/bin/bash

for file in "$@"; do
    echo -n "$file... "
    out=$(mktemp)
    diffout=$(mktemp)
    ./flp19-log < "$file" > "$out"
    if diff -y "$out" "${file/input/output}" > "$diffout" 2>&1; then
        echo "ok"
    else
        echo "error"
        cat "$diffout"
    fi
    rm "$out" "$diffout"
done
