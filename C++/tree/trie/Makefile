APP_NAME=dictrie

CC = g++
CFLAGS = -Wall #-std=c++11

TEST_CFLAGS = -fsanitize=leak
DEBUG_CFLAGS = $(TEST_CFLAGS) -Werror -D DEBUG -O0
BUILD_CFLAGS = -D NDEBUG -O2

TARGETS = main.cpp
DEPS = $(TARGETS) trie.hpp

default: $(DEPS)
	$(CC) $(CFLAGS) $(TEST_CFLAGS) -o $(APP_NAME).out $(TARGETS)

test:
	make default
	./$(APP_NAME).out

debug: $(DEPS)
	$(CC) $(CFLAGS) $(DEBUG_CFLAGS) -o $(APP_NAME).out -g $(TARGETS)

gdb:
	make debug
	gdb $(APP_NAME).out

build: $(DEPS)
	$(CC) $(CFLAGS) $(BUILD_CFLAGS) -o $(APP_NAME).out $(TARGETS)

docs:
	doxygen
	find doc/ -type f -print0 | xargs -0 sed -i 's/Ficheiro/Arquivo/g'
	find doc/ -type f -print0 | xargs -0 sed -i 's/ficheiro/arquivo/g'

clean:
	rm *.out
