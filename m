#!/bin/sh
g++ -O0 -Wall -g interp.cpp -o interp_exe && ./interp_exe --tests
