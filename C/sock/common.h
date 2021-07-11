#ifndef H_COMMON
#define H_COMMON

#include <netinet/in.h>

int parse_address(const char *ip, const char *port, struct sockaddr_in *addr);

#endif // H_COMMON
