#!/bin/bash
#SBATCH -N 1
#SBATCH -n 60
./compile all_wrfvar >& compile.out
