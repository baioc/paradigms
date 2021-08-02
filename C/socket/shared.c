#include "shared.h"

#include <stdlib.h> // atoi

#include <arpa/inet.h>  // inet_aton, htons


int parse_address(const char *ip, const char *port, struct sockaddr_in *addr)
{
	const int ok = inet_aton(ip, &addr->sin_addr);
	if (!ok) return 1;

	const int p = atoi(port);
	if (p < 0 || p >= 65536) return 2;

	addr->sin_family = AF_INET;
	addr->sin_port = htons(p);
	return 0;
}
