#include "common.h"

#include <stdlib.h>

#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/socket.h>


int parse_address(const char *ip, const char *port, struct sockaddr_in *addr)
{
	const in_addr_t address = inet_addr(ip);
	if (address == INADDR_NONE) return 1;

	const int p = atoi(port);
	if (p < 0 || p > 65535) return 2;

	addr->sin_family = AF_INET;
	addr->sin_addr = (struct in_addr){ .s_addr = address };
	addr->sin_port = htons(p);
	return 0;
}
