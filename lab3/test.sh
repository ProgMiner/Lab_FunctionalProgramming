#!/bin/bash


if [[ -z "$1" ]] ; then
    filename='test.input.txt'
else
    filename="$1"
fi

racket main.rkt < "$filename" | ./test.gpi
