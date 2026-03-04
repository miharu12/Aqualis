#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test6_1.f90 -ffree-line-length-none  -o test6_1.exe
./test6_1.exe
