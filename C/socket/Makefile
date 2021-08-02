.PHONY: clean all

# default
all: server client

clean:
	-@ rm *.o
	-@ rm server client

server: server.c shared.o
	$(CC) $(CFLAGS) $^ -o $@

client: client.c shared.o
	$(CC) $(CFLAGS) $^ -o $@
