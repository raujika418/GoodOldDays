#ifndef STGSOCKET_H
#define STGSOCKET_H

/* This file is NON_POSIX_SOURCE. */

/* Get definition of struct sockaddr to avoid warnings 
 */
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif

/* acceptSocket.lc */
StgInt acceptSocket(I_ sockfd, A_ peer, A_ addrlen);

/* bindSocket.lc */
StgInt bindSocket(I_ sockfd, A_ myaddr, I_ addrlen, I_ isUnixDomain);

/* connectSocket.lc */
StgInt connectSocket(I_ sockfd, A_ servaddr, I_ addrlen, I_ isUnixDomain);

/* createSocket.lc */
StgInt createSocket(I_ family, I_ type, I_ protocol);

/* getPeerName.lc */
StgInt getPeerName(I_ sockfd, A_ peer, A_ namelen);

/* getSockName.lc */
StgInt getSockName(I_ sockfd, A_ peer, A_ namelen);

/* listenSocket.lc */
StgInt listenSocket(I_ sockfd, I_ backlog);

/* readDescriptor.lc */
StgInt readDescriptor(I_ fd, A_ buf, I_ nbytes);

/* shutdownSocket.lc */
StgInt shutdownSocket(I_ sockfd, I_ how);

/* writeDescriptor.lc */
StgInt writeDescriptor(I_ fd, A_ buf, I_ nbytes);

#endif STG_SOCKET_H
