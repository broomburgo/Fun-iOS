#!/bin/bash

if [ $# -ne 1 ]; then
    echo $0: "add a single name parameter for naming the file (without .md)"
    exit 1
fi

name=$1

hugo new post/$name.md

open content/post/$name.md
