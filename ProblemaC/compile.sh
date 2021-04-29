#!/bin/bash
ocamlopt mainv2.ml -o mainv2
rm *.cmi *.cmx *.o &> /dev/null