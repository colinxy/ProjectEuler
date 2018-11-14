# https://stackoverflow.com/questions/18007326/how-to-change-default-values-of-variables-like-cc-in-makefile
ifeq ($(origin CXX),default)
CXX = g++
endif

CXXFLAGS ?= -std=c++11 -O2 -Wall -Wextra -Werror=return-type

OBJ = mathutil.o
SRC = mathutil.cpp
HDR = mathutil.h

all : $(OBJ)

debug : CXXFLAGS += -g
debug : all

$(OBJ) : $(SRC) $(HDR)
	$(CXX) -c -o $(OBJ) $(SRC) $(CXXFLAGS)

clean :
	-rm $(OBJ)
