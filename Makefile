CXX=g++
#CXXFLAGS=-O2 -pedantic -Wall -Wextra -ansi -std=c++11
CXXFLAGS=-g -pedantic -Wall -Wextra -Wno-unused-parameter -ansi -std=c++11

all: demo

exif.o: exif.cpp exif.h
	$(CXX) $(CXXFLAGS) -c exif.cpp

demo: exif.o demo.cpp
	$(CXX) $(CXXFLAGS) -o demo exif.o demo.cpp

clean:
	rm -f *.o demo demo.exe
	
format:
	clang-format -style=Google -i demo.cpp exif.cpp exif.h
	
test: demo valgrind
	./test.sh

valgrind: 
	valgrind --leak-check=full --show-leak-kinds=all ./demo test-images/test1.jpg
	
contrib: format test valgrind