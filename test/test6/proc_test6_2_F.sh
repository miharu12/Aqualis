#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test6_2.f90 -ffree-line-length-none -lfftw3 -I/usr/local/include -o test6_2.exe
./test6_2.exe
