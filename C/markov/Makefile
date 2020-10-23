CFLAGS = -Wall -std=c99 -O2
LDFLAGS = -lm -lpthread

BIN = multilang
SRCS = $(wildcard *.c)
OBJS = $(patsubst %.c, %.o, $(SRCS))

.PHONY: all clean run

all: $(BIN)

clean:
	$(RM) $(OBJS) $(BIN)

$(BIN): $(OBJS)
