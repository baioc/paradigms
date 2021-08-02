#include "shared.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

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

int eprintf(const char* format, ...)
{
	int ret;
	va_list args;
	va_start(args, format);
	ret = vfprintf(stderr, format, args);
	va_end(args);
	return ret;
}
