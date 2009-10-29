/*
** dcc.c (for irchat-jp)
** Copyright (C) 1995,1996,1998,1999 KIKUCHI Takahiro
**
** Author:        KIKUCHI Takahiro <kick@kyoto.wide.ad.jp>
** Created:       Mar 19, 1995
** Last Modified: Jul  7, 1999
*/

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <netinet/in.h>
#include <stdio.h>
#include <fcntl.h>
#include <netdb.h>
#ifdef	AIX
# include <sys/select.h>
#endif
#ifdef	linux
# ifndef __GLIBC__
#  include <linux/time.h>
# endif
#endif
#ifdef	SOCKS
# include <socks.h>
#endif

int main(argc, argv)
int argc;
char **argv;
{
    char *command, *type, *action;
    int status = 1;
    extern u_long atoul();
    
    command = *argv++, argc--;
    if (argc < 2) {
	printf("DCC ERROR Not enough parameters\n");
	fprintf(stderr, "Usage: %s <type> <command> <arg>...\n", command);
	exit(1);
    }

    type = *argv++, argc--;
    action = *argv++, argc--;

    if (!strcmp(type, "file")) {
	if (!strcmp(action, "send")) {
	    if (argc == 1) {
		status = file_send(argv[0], 0, 0);
	    } else if (argc == 3) {
		status = file_send(argv[0], atoi(argv[1]), argv[2]);
	    } else {
		printf("DCC ERROR Wrong number of parameters\n");
		fprintf(stderr,
			"Usage: %s file send <filename> <port> <ircserver>\n",
			command);
		status = 1;
	    }
	} else if (!strcmp(action, "get")) {
	    if (argc == 4) {
		status = file_get(atoul(argv[0]), atoi(argv[1]),
				  atoi(argv[2]), argv[3]);
	    } else {
		printf("DCC ERROR Wrong number of parameters\n");
		fprintf(stderr,
			"Usage: %s file get <addr> <port> <size> <filename>\n",
			command);
		status = 1;
	    }
	} else {
	    printf("DCC ERROR Unsupported command %s %s\n", type, action);
	    status = 1;
	}
    } else if (!strcmp(type, "chat")) {
	if (!strcmp(action, "listen")) {
	    if (argc == 0) {
		status = chat_listen(0, 0);
	    } else if (argc == 2) {
		status = chat_listen(atoi(argv[0]), argv[1]);
	    } else {
		printf("DCC ERROR Wrong number of parameters\n");
		fprintf(stderr, "Usage: %s chat listen <port> <ircserver>\n",
			command);
		status = 1;
	    }
	} else if (!strcmp(action, "connect")) {
	    if (argc == 2) {
		status = chat_connect(atoul(argv[0]), atoi(argv[1]));
	    } else {
		printf("DCC ERROR Wrong number of parameters\n");
		fprintf(stderr, "Usage: %s chat connect <addr> <port>\n",
			command);
		status = 1;
	    }
	} else {
	    printf("DCC ERROR Unsupported command %s %s\n", type, action);
	    status = 1;
	}
    } else if (!strcmp(type, "tcp")) {
	if (!strcmp(action, "connect")) {
	    if (argc == 2) {
		status = tcp_connect(argv[0], argv[1]);
	    } else {
		printf("DCC ERROR Wrong number of parameters\n");
		fprintf(stderr, "Usage: %s chat connect <addr> <port>\n",
			command);
		status = 1;
	    }
	} else {
	    printf("DCC ERROR Unsupported command %s %s\n", type, action);
	    status = 1;
	}
    } else {
	printf("DCC ERROR Unsupported command %s\n", type);
	status = 1;
    }
    if (status) {
	sleep(3);
    }
    exit(status);
}

u_long getmyaddr(ircserver)
char *ircserver;
{
    int i, len, dummy;
    u_long addr;
    char *p;
    struct hostent *hp;
    struct sockaddr_in server, client;

    addr = 0xc6290004;				/* dummy addr --- rootA */
    if (ircserver && (hp = gethostbyname(ircserver)) != NULL) {
	addr = ntohl(((struct in_addr *)hp->h_addr_list[0])->s_addr);
    }
    if ((dummy = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
	printf("DCC ERROR Cannot create socket\n");
	return -1;
    }
    for (i = 0, p = (char *)&server; i < sizeof(server); i++, *p++ = 0);
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = htonl(addr);
    server.sin_port = htons(7);			/* dummy port --- echo */
    if (connect(dummy, (struct sockaddr *)&server, sizeof(server)) < 0) {
	printf("DCC ERROR Cannot connect socket\n");
	return -1;
    }
    len = sizeof(client);
    if (getsockname(dummy, (struct sockaddr *)&client, &len) < 0) {
	printf("DCC ERROR Cannot getsockname\n");
	return -1;
    }
    close(dummy);
    return ntohl(client.sin_addr.s_addr);
}

int dcc_listen(paddr, pport, ircserver)
u_long *paddr;
u_short *pport;
char *ircserver;
{
    int i, s, len;
    char *p;
    struct sockaddr_in server;

    if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	printf("DCC ERROR Cannot create socket\n");
	return -1;
    }
    for (i = 0, p = (char *)&server; i < sizeof(server); i++, *p++ = 0);
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = htonl(getmyaddr(ircserver));
    server.sin_port = htons(*pport);
    while (bind(s, (struct sockaddr *)&server, sizeof(server)) < 0) {
	if (!*pport) {
	    printf("DCC ERROR Cannot bind socket\n");
	    return -1;
	}
	server.sin_port = htons(++(*pport));
    }
    if (listen(s, 1) < 0) {
	printf("DCC ERROR Cannot listen socket\n");
	return -1;
    }
    len = sizeof(server);
    if (getsockname(s, (struct sockaddr *)&server, &len) < 0) {
	printf("DCC ERROR Cannot getsockname\n");
	return -1;
    }	
    *pport = ntohs(server.sin_port);
    *paddr = ntohl(server.sin_addr.s_addr);

    return s;
}

int dcc_connect(addr, port)
u_long addr;
u_short port;
{
    int i, remote;
    char *p;
    struct sockaddr_in server;

    if ((remote = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	printf("DCC ERROR Cannot create socket\n");
	return -1;
    }
    for (i = 0, p = (char *)&server; i < sizeof(server); i++, *p++ = 0);
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = htonl(addr);
    server.sin_port = htons(port);
    if (connect(remote, (struct sockaddr *)&server, sizeof(server)) < 0) {
	printf("DCC ERROR Cannot connect %u/%d\n", addr, port);
	return -1;
    }
    return remote;
}

u_long atoul(str)
char *str;
{
    u_long val = 0;

    while (*str) {
	val = val * 10 + *str - '0';
	str++;
    }
    return val;
}

u_long dottoul(str)
char *str;
{
    u_long val, v;

    for (val = v = 0; *str; str++) {
	if (*str == '.') {
	    val = val * 256 + v;
	    v = 0;
	} else {
	    v = v * 10 + *str - '0';
	}
    }
    val = val * 256 + v;
    return val;
}

int file_send(filename, port, ircserver)
char *filename;
u_short port;
char *ircserver;
{
    int file, size, s, remote, len, count = 0;
    u_long addr;
    struct stat statbuf;
    u_long done = 0;
    u_long report;
    char buf[4096];

    if ((file = open(filename, O_RDONLY)) < 0) {
	printf("DCC ERROR Cannot open file %s\n", filename);
	return 1;
    }
    if (fstat(file, &statbuf) < 0) {
	printf("DCC ERROR Cannot stat file %s\n", filename);
	return 1;
    }
    size = statbuf.st_size;

    if ((s = dcc_listen(&addr, &port, ircserver)) < 0) {
	/* error report in dcc_listen */
	return 1;
    }
    /* 'Setting -> 'Waiting (send DCC SEND to remote user) */
    printf("DCC SEND %u %d %d\n", addr, port, size);

    remote = accept(s, (struct sockaddr *) 0, (int *) 0);
    close(s);
    /* 'Waiting -> 'Sending (accepted from remote user) */
    printf("DCC SENDING\n");

    while ((len = read(file, buf, sizeof(buf))) > 0) {
	write(remote, buf, len);
	done += len;
	if (++count == 16) {
	    count = 0;
	    while (read(remote, &report, sizeof(u_long)) > 0 &&
		   ntohl(report) != done);
	    printf("DCC REPORT %s %d%% (%d/%d bytes) sent\n",
		   filename, 100 * done / size, done, size);
	}
    }
    while (read(remote, &report, sizeof(u_long)) > 0 &&
	   ntohl(report) != done);

    /* 'Sending -> end */
    close(remote);
    close(file);
    return 0;
}

int file_get(addr, port, size, filename)
u_long addr;
u_short port;
int size;
char *filename;
{
    int remote, file, len, toread, count = 0;
    u_long done = 0;
    u_long report;
    char buf[4096];

    if ((file = open(filename, O_WRONLY|O_CREAT|O_TRUNC, 0600)) < 0) {
	printf("DCC ERROR1 Cannot open file %s\n", filename);
	return 1;
    }

    if ((remote = dcc_connect(addr, port)) < 0) {
	/* error report in dcc_connect */
	return 1;
    }
    /* 'Connect -> 'Getting (connected to remote user) */
    printf("DCC GETTING\n");

    toread = sizeof(buf);
    while (size - done > 0) {
	if (toread > size - done) {
	    toread = size - done;
	}
	if ((len = read(remote, buf, toread)) < 0) {
	    printf("DCC ERROR read error %s\n", filename);
	    return 1;
	}
	write(file, buf, len);
	done += len;
	report = htonl(done);
	write(remote, &report, sizeof(report));
	if (++count == 16) {
	    count = 0;
	    printf("DCC REPORT %s %d%% (%d/%d bytes) received\n",
		   filename, 100 * done / size, done, size);
	}
    }

    /* 'Getting -> end */
    close(remote);
    close(file);
    return 0;
}

int chat_listen(port, ircserver)
u_short port;
char *ircserver;
{
    u_long addr;
    int s, remote;

    if ((s = dcc_listen(&addr, &port, ircserver)) < 0) {
	return 1;
    }
    /* 'Setting -> 'Waiting (send DCC CHAT to remote user) */
    printf("DCC CHAT %u %d\n", addr, port);

    remote = accept(s, (struct sockaddr *) 0, (int *) 0);
    close(s);
    /* 'Waiting -> 'Active (accepted from remote user) */
    printf("DCC CHATTING\n");
    return loop(remote);
}

int chat_connect(addr, port)
u_long addr;
u_short port;
{
    int remote;

    if ((remote = dcc_connect(addr, port)) < 0) {
	return 1;
    }
    /* 'Connect -> 'Active (connected to remote user) */
    printf("DCC CHATTING\n");
    return loop(remote);
}

int loop(remote)
int remote;
{
    int n, len, cnt;
    char *ptr, buf[1024];
    fd_set rfds;

    while (1) {
	FD_ZERO(&rfds);
	FD_SET(0, &rfds);
	FD_SET(remote, &rfds);
	if ((n = select(FD_SETSIZE, &rfds, NULL, NULL, NULL)) == -1) {
	    continue;
	}
	if (FD_ISSET(0, &rfds)) {
	    if ((len = read(0, buf, sizeof(buf))) <= 0) {
		close(0);
		close(remote);
		if (len == 0) {
		    return 0;
		} else {
		    return 1;
		}
	    }
	    for (ptr = buf; len > 0; ptr+= cnt, len -= cnt) {
		if ((cnt = write(remote, ptr, len)) < 0) {
		    close(0);
		    close(remote);
		    return 1;
		}
	    }
	}
	if (FD_ISSET(remote, &rfds)) {
	    if ((len = read(remote, buf, sizeof(buf))) <= 0) {
		close(0);
		close(remote);
		if (len == 0) {
		    return 0;
		} else {
		    return 1;
		}
	    }
	    for (ptr = buf; len > 0; ptr+= cnt, len -= cnt) {
		if ((cnt = write(0, ptr, len)) < 0) {
		    close(0);
		    close(remote);
		    return 1;
		}
	    }
	}
    }
}

int tcp_connect(straddr, strport)
char *straddr, *strport;
{
    int remote;
  
#ifdef INET6
    int eai;
    struct addrinfo hints, *ai;

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    if ((eai = getaddrinfo(straddr, strport, &hints, &ai)) != 0) {
        printf("ERROR :Closing Link: [*@*] (Cannot resolve %s: %s)\n",
	       straddr, gai_strerror(eai));
        return 0;
    }
    if ((remote = socket(ai->ai_family, SOCK_STREAM, 0)) < 0) {
        printf("ERROR :Closing Link: [*@*] (Cannot create socket)\n");
	return 0;
    }
    if (connect(remote, ai->ai_addr, ai->ai_addrlen) < 0) {
        printf("ERROR :Closing Link: [*@*] (Cannot connect to %s/%s)\n",
	       straddr, strport);
	return 0;
    }
#else
    int i;
    u_long addr;
    char *p;
    struct hostent *hp;
    struct sockaddr_in server;

    if (*straddr >= '0' && *straddr <= '9') {
        addr = dottoul(straddr);
    } else if ((hp = gethostbyname(straddr)) != NULL) {
	addr = ntohl(((struct in_addr *)hp->h_addr_list[0])->s_addr);
    } else {
        printf("ERROR :Closing Link: [*@*] (Cannot resolve %s)\n", straddr);
	return 0;
    }
    if ((remote = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        printf("ERROR :Closing Link: [*@*] (Cannot create socket)\n");
	return 0;
    }
    for (i = 0, p = (char *)&server; i < sizeof(server); i++, *p++ = 0);
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = htonl(addr);
    server.sin_port = htons(atoi(strport));
    if (connect(remote, (struct sockaddr *)&server, sizeof(server)) < 0) {
        printf("ERROR :Closing Link: [*@*] (Cannot connect to %s/%s)\n",
	       straddr, strport);
	return 0;
    }
#endif

    return loop(remote);
}

