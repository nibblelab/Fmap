# --------------------------------------------------
# Makefile for calling std::map in Fortran
# --------------------------------------------------

# Files Folder
src = ../src
example = ../examples

# Exe name
exe = test

# Compiler
FC = gfortran
CC = g++

# Flags
W_flags = -Wall -Wextra -Werror
Opt_flags = #-O3 -march=native -flto=8 -fno-fat-lto-objects -fno-strict-aliasing
COMM_flags = -g 
FC_flags = -ffree-line-length-none -fcheck=all 
CC_flags =
LFLAGS =  -lstdc++

# Valgrind debug
Val_flags = --leak-check=full --show-reachable=yes --track-origins=yes

# Concatenate flags and compiler into one command
FC_Mod = $(FC) -c $(COMM_flags) $(FC_flags) $(W_flags) $(Opt_flags)
CC_Mod = $(CC) -c $(COMM_flags) $(CC_flags) $(W_flags)
FC_Lin = $(FC) -g $(Opt_flags) 

#List of objects to compile
File_list = test.o fmap.o cmap.o

#------------------------
# Build
all: $(File_list)
	$(FC_Lin) *.o -o $(exe) $(LFLAGS)


#------------------------
# Generic rules
%.mod: %.o
	@true

%.o: $(src)/%.f90
	$(FC_Mod) -std=f2008 $<

%.o: $(example)/%.f90
	$(FC_Mod) -std=f2008 $<

%.o: $(src)/%.cpp
	$(CC_Mod) $<

# Objects list and dependencies
fmap.o: $(src)/fmap.f90
cmap.o: $(src)/cmap.cpp
test.o: $(example)/test.f90 fmap.mod cmap.o


#------------------------
# Clean make dir
clean:
	rm *.o *.mod $(exe)

#------------------------
# Run valgrind to debug memory
mem-debug:
	valgrind $(Val_flags) ./$(exe)
