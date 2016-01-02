CXX = g++

CPPFLAGS = -std=c++11 -O2
OBJ = mathutil.o
SRC = mathutil.cpp
HDR = mathutil.h

$(OBJ) : $(SRC) $(HDR)
	$(CXX) -c -o $(OBJ) $(SRC) $(CPPFLAGS)

clean :
	rm $(OBJ)
