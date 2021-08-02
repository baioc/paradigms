#ifndef H_SHARED
#define H_SHARED

#include <netinet/in.h> // sockaddr_in

/**
 * Fills an address structure from strings containing IP and port numbers.
 *
 * Returns 0 on success or 1 (2) when the 1st (2nd) argument is badly formatted.
 */
int parse_address(const char *ip, const char *port, struct sockaddr_in *addr);

#endif // H_SHARED
