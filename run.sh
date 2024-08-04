#!/bin/bash
#shell script to compile and run program

#clear terminal screen
clear

#remove files from constants module
rm physical_constants.o physical_constants.mod

#remove files from main program
rm serial.o serial.exe

#remove files from vectors module
rm vectors.o vectors.mod

#remove files from magnetic field module
rm mag_field_calculations.o mag_field_calculations.mod

#remove files from initialisation module
rm initialise.o initialise.mod

#choose to remove any text files from previous runs, such as profile, or any text files
#the user wishes to reproduce.
rm -i *.txt

#number of threads if running in parallel, can be changed by the user as desired.
export OMP_NUM_THREADS=8

#compile constants module
gfortran -pg -O3 -march=native -c physical_constants.f90
echo "Constants module compiled"

#compile initialise module
gfortran -pg -O3 -march=native -c initialise.f90
echo "Initialise module compiled"

#compile vectors module
gfortran -pg -O3 -march=native -c vectors.f90
echo "Vectors module compliled"

#compile magnetic field module
gfortran -pg -fopenmp -O3 -march=native -c mag_field_calculations.f90
echo "Magnetic field module compiled"

#compile main program
gfortran -pg -O3 -march=native -c -fopenmp mainprog.f90
echo "Main program compiled"

#Link main program with modules
gfortran -o mainprog.exe -pg -fopenmp -O3 -march=native physical_constants.o \
initialise.o vectors.o mag_field_calculations.o mainprog.o
echo "Main program and modules linked"

./mainprog.exe
#print profiling to file
gprof mainprog.exe > profile.txt

#From here down can be used to automate multiple runs of the program with different values
#of N, mag (equaivalent to mag_direction in code) and sORp.
#All lines to be uncommented and can be edited by user as desired.
#Currently set to run the problem 6 times with different values of N, with the 
#magnetic moment direction in the x-direction and all runs in serial.
#declare -i N
#mag=x
#sORp=s
#for N in 1 2 5 10 20 30
#do
#  echo "${N} ${mag} ${sORp}" | ./mainprog.exe >> out.txt
#done
