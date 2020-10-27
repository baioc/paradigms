CC ?= clang
CFLAGS += -Wall -std=c99
LDFLAGS += -lm -lpthread

BUILD_FLAGS = -O2 -DNDEBUG
DEBUG_FLAGS = -g -O0 -DDEBUG

SRCS = $(patsubst %.h, %.c, $(wildcard *.h))
OBJS = $(wildcard *.o)

.PHONY: clean all build debug

all: build

build:
	CFLAGS="$(BUILD_FLAGS)" $(MAKE) multilang.out
	CFLAGS="$(BUILD_FLAGS)" $(MAKE) predict.out

debug:
	CFLAGS="$(DEBUG_FLAGS)" $(MAKE) multilang.out
	CFLAGS="$(DEBUG_FLAGS)" $(MAKE) predict.out

multilang.out: multilang.c $(SRCS)
	$(CC) $(CFLAGS) $(LDFLAGS) $(SRCS) -DTOKEN_CHARACTER -DGRAMSIZE=3 $< -o $@

predict.out: predict.c $(SRCS)
	$(CC) $(CFLAGS) $(LDFLAGS) $(SRCS) -DTOKEN_WORD -DGRAMSIZE=3 $< -o $@

clean:
	$(RM) $(OBJS) $(wildcard *.out)
