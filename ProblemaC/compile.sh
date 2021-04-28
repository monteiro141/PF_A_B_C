#!/bin/bash
ocamlopt main.ml -o main
rm *.cmi *.cmx *.o &> /dev/null