APP_NAME=graphs

CC = g++ -std=c++17
CFLAGS = -Wall
TEST_CFLAGS = -fsanitize=leak
BUILD_CFLAGS = -fpic -DNDEBUG=1 -O2

SC = swig
SFLAGS = -python -c++

SRC = main.cpp
OBJ = graphs_wrap.o
INT = graphs.i
GEN = graphs_wrap.cxx

LIBS = -I/usr/include/python3.7m

default:
	make build

build: $(SRC)
	$(SC) $(SFLAGS) $(INT)
	$(CC) $(CFLAGS) $(BUILD_CFLAGS) $(LIBS) -c $(GEN)
	$(CC) -shared -o _$(APP_NAME).so $(OBJ)

test: $(SRC)
	$(CC) $(CFLAGS) $(TEST_CFLAGS) -o graph_test.out $(SRC)

all:
	make test
	make build

clean:
	-rm *.o
	-rm graph_test.out
	-rm *_wrap.cxx
	-rm _$(APP_NAME).so
	-rm $(APP_NAME).py
	-rm -R __pycache__
