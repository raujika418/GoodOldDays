%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
% Last Modified: Fri Jul 21 15:14:43 1995
% Darren J Moffat <moffatd@dcs.gla.ac.uk>
\section[SocketPrim]{Low-level socket bindings}

The @SocketPrim@ module is for when you want full control over the
sockets, something like what you have in C (which is very messy).

\begin{code}	   
module SocketPrim (

    Socket,		
    Family(..),		
    SocketType(..),
    SockAddr(..),
    HostAddress(..),
    ShutdownCmd(..),

    socket,		-- :: Family -> SocketType -> Int -> IO Socket 
    connect,		-- :: Socket -> SockAddr -> IO ()
    bindSocket,		-- :: Socket -> SockAddr -> IO ()
    listen,		-- :: Socket -> Int -> IO ()
    accept,		-- :: Socket -> IO (Socket, SockAddr)
    getPeerName,	-- :: Socket -> IO SockAddr
    getSocketName,	-- :: Socket -> IO SockAddr

    socketPort,		-- :: Socket -> IO Int

    writeSocket,	-- :: Socket -> String -> IO Int
    readSocket,		-- :: Socket -> Int -> IO (String, Int)
    readSocketAll,	-- :: Socket -> IO String

    socketToHandle,	-- :: Socket -> IO Handle

-- Alternative read/write interface not yet implemented.
--    sendto		-- :: Socket -> String -> SockAddr -> IO Int
--    recvfrm		-- :: Socket -> Int -> SockAddr -> IO (String, Int)
--    sendmsg		-- :: Socket -> Message -> MsgFlags -> IO Int
--    recvmsg		-- :: Socket -> MsgFlags -> IO Message

    shutdown,		-- :: Socket -> ShutdownCmd -> IO ()
    sClose,		-- :: Socket -> IO ()

    inet_addr,		-- :: String -> HostAddress
    inet_ntoa,		-- :: HostAddress -> String

    sIsConnected,	-- :: Socket -> IO Bool
    sIsBound,		-- :: Socket -> IO Bool
    sIsListening,	-- :: Socket -> IO Bool 
    sIsReadable,	-- :: Socket -> IO Bool
    sIsWritable,	-- :: Socket -> IO Bool


-- Special Constants

    aNY_PORT,
    iNADDR_ANY,
--    sOL_SOCKET,
    sOMAXCONN,
    maxListenQueue,


-- The following are exported ONLY for use in the BSD module and
-- should not be used else where.

    packFamily, unpackFamily,
    packSocketType,
    packSockAddr, unpackSockAddr

) where
 
import LibPosix
import LibPosixUtil
import PreludeIOError
import PreludeGlaST
import PreludePrimIO	( newEmptyMVar, putMVar, _MVar )
import PreludeStdIO
import PS
import Prel		( isSpace )
\end{code}


%************************************************************************
%*									*
\subsection[Socket-SocketTypes]{Socket Types}
%*									*
%************************************************************************


There are a few possible ways to do this.  The first is convert the
structs used in the C library into an equivalent Haskell type.	An
other possible implementation is to keep all the internals in the C
code and use an Int\# and a status flag. The second method is used here
since a lot of the C structures are not required to be manipulated.
Originally the status was non mutable so we had to return a new socket
each time we changed the status.  This version now uses mutable
variables to avoid the need to do this.	 The result is a cleaner
interface and better security since the application programmer now
can't circumvent the status information to perform invalid operations
on sockets.	      


\begin{code}  
data SocketStatus
  -- Returned Status	Function called
  = NotConnected	-- socket
  | Bound		-- bindSocket
  | Listening		-- listen
  | Connected		-- connect/accept
  | Error String	-- Any
  deriving (Eq, Text)

data Socket
  = MkSocket 
     Int				  -- File Descriptor Part
     Family				  
     SocketType				  
     Int				  -- Protocol Number
     (MutableVar _RealWorld SocketStatus) -- Status Flag
\end{code}

The scheme used for addressing sockets is somewhat quirky. The
calls in the BSD socket API that need to know the socket address all
operate in terms of \tr{struct sockaddr}, a `virtual' type of socket address. 
The Internet family of sockets are addressed as \tr{struct sockaddr\_in},
so when calling functions that operate on \tr{struct sockaddr}, we have
to type cast the Internet socket address into a \tr{struct sockaddr}. By luck(!),
the two structures are of the same size. Same casting is required of other
families of sockets such as Xerox NS. Similarly for Unix domain sockets.

To represent these socket addresses in Haskell-land, we do what BSD didn't do,
and use a union/algebraic type for the different families. Currently only
Unix domain sockets and the Internet family is supported.

\begin{code}
type HostAddress = _Word

data SockAddr		-- C Names				
  = SockAddrUnix	-- struct sockaddr_un
	String		-- sun_path
  | SockAddrInet	-- struct sockaddr_in
	Int		-- sin_port
	HostAddress	-- sin_addr
  deriving Eq
\end{code}


%************************************************************************
%*									*
\subsection[Socket-Connections]{Connection Functions}
%*									*
%************************************************************************

In the following connection and binding primitives.  The names of the
equivalent C functions have been preserved where possible. It should
be noted that some of these names used in the C library, \tr{bind} in
particular, have a different meaning to many Haskell programmers and
have thus been renamed by appending the prefix Socket.

Create an unconnected socket of the given family, type and protocol.
The most common invocation of $socket$ is the following:
\begin{verbatim}
   ...
   socket AF_INET Stream 6	>>= \ my_socket ->
   ...
\end{verbatim}

\begin{code}	   
socket :: Family 	-- Family Name (usually AF_INET)
       -> SocketType 	-- Socket Type (usually Stream)
       -> Int		-- Protocol Number (getProtocolByName to find value)
       -> IO Socket	-- Unconnected Socket
socket family stype protocol = 
    _ccall_ createSocket (packFamily family) 
		         (packSocketType stype) 
		         protocol		`thenPrimIO` \ status -> 
    case status of
      -1 -> _constructError "socket"		`thenPrimIO` \ ioError ->
            failWith ioError
      n ->
	newVar NotConnected			`thenPrimIO` \ socket_status ->
	return (MkSocket n family stype protocol socket_status)

\end{code}
      
Given a port number this {\em binds} the socket to that port. This
means that the programmer is only interested in data being sent to
that port number. The $Family$ passed to $bindSocket$ must
be the same as that passed to $socket$.	 If the special port
number $aNY\_PORT$ is passed then the system assigns the next
available use port.

Port numbers for standard unix services can be found by calling
$getServiceEntry$.  These are traditionally port numbers below
1000; although there are afew, namely NFS and IRC, which used higher
numbered ports.

The port number allocated to a socket bound by using $aNY\_PORT$ can be
found by calling $port$

\begin{code}
bindSocket :: Socket	-- Unconnected Socket
	   -> SockAddr	-- Address to Bind to
	   -> IO ()
bindSocket (MkSocket s family stype protocol socketStatus) addr =
 let isDomainSocket = if family == AF_UNIX then 1 else (0::Int) in
 readVar socketStatus				`thenPrimIO` \ currentStatus ->
 if currentStatus /= NotConnected then
   fail ("bindSocket: can't peform bind on socket in status " ++
	 show currentStatus)
 else
   packSockAddr addr				`thenPrimIO` \ addr' ->
   let (_,sz) = boundsOfByteArray addr' in
   _ccall_ bindSocket s addr' sz isDomainSocket	`thenPrimIO` \ status ->
   case status of
     -1 -> _constructError "bindSocket"	        `thenPrimIO`
	   failWith
     0  -> 
  	   writeVar socketStatus (Bound)	`seqPrimIO`
	   return ()
\end{code}
	

Make a connection to an already opened socket on a given machine and port.
assumes that we have already called createSocket, othewise it will fail.
			
This is the dual to $bindSocket$.  The {\em server} process will
usually bind to a port number, the {\em client} will then connect to 
the same port number.  Port numbers of user applications are normally
agreed in advance, otherwise we must rely on some meta protocol for telling
the other side what port number we have been allocated.	       

\begin{code}
connect :: Socket	-- Unconnected Socket
	-> SockAddr 	-- Socket address stuff
	-> IO ()
connect (MkSocket s family stype protocol socketStatus) addr =
 let isDomainSocket = if family == AF_UNIX then 1 else (0::Int) in
 readVar socketStatus				  `thenPrimIO` \ currentStatus ->
 if currentStatus /= NotConnected then
   fail ("connect: can't peform connect on socket in status " ++
         show currentStatus)
 else
   packSockAddr addr				   `thenPrimIO` \ addr' ->
   let (_,sz) = boundsOfByteArray addr' in
   _ccall_ connectSocket s addr' sz isDomainSocket `thenPrimIO` \ status ->
   case status of
     -1 -> _constructError "connect"		   `thenPrimIO`
	   failWith
     0 ->
	   writeVar socketStatus Connected	   `seqPrimIO`
	   return ()
\end{code}
       
The programmer must call $listen$ to tell the system software
that they are now interested in receiving data on this port.  This
must be called on the bound socket before any calls to read or write
data are made. 

The programmer also gives a number which indicates the length of the
incoming queue of unread messages for this socket. On most systems the
maximum queue length is around 5.  To remove a message from the queue
for processing a call to $accept$ should be made.	

\begin{code}
listen :: Socket  -- Connected & Bound Socket
       -> Int 	  -- Queue Length
       -> IO ()
listen (MkSocket s family stype protocol socketStatus) backlog =
 readVar socketStatus			  `thenPrimIO` \ currentStatus ->
 if currentStatus /= Bound then
    fail ("listen: can't peform listen on socket in status " ++
          show currentStatus)
 else
    _ccall_ listenSocket s backlog 	  `thenPrimIO` \ status ->
    case status of
      -1 ->
	  _constructError "listen"        `thenPrimIO`
	  failWith
      0  ->
	  writeVar socketStatus Listening `seqPrimIO`
	  return ()
\end{code}

A call to $accept$ only returns when data is available on the given
socket, unless the socket has been set to non-blocking.	 It will
return a new socket which should be used to read the incoming data and
should then be closed. Using the socket returned by $accept$ allows
incoming requests to be queued on the original socket.

\begin{code}
accept :: Socket			-- Queue Socket
       -> IO (Socket,			-- Readable Socket
	      SockAddr)			-- Peer details
accept sock@(MkSocket s family stype protocol status) =
 readVar status			        `thenPrimIO` \ currentStatus ->
 sIsAcceptable sock		        >>= \ okay ->
 if not okay then    
   fail ("accept: can't peform accept on socket in status " ++
	 show currentStatus)
 else
   allocSockAddr family			`thenPrimIO` \ (ptr, sz) ->
   newIntArray (0,1)			`thenPrimIO` \ int_star ->
   writeIntArray int_star 0 sz		`seqPrimIO`
   _ccall_ acceptSocket s ptr int_star  `thenPrimIO` \ sock ->
   case sock of
     -1 ->
	_constructError "accept"        `thenPrimIO`
	failWith
     _  ->
        readIntArray int_star 0		`thenPrimIO` \ sz ->
	unpackSockAddr ptr sz		`thenPrimIO` \ addr ->
	newVar Connected		`thenPrimIO` \ status ->
	return ((MkSocket sock family stype protocol status), addr)
\end{code}

%************************************************************************
%*									*
\subsection[Socket-DataPass]{Data Passing Primitives}
%*									*
%************************************************************************

To allow Haskell to talk to C programs we need to be able to
communicate in terms of byte streams. @writeSocket@ and
@readSocket@ should only be used for this purpose and not for
communication between Haskell programs.	 Haskell programs should use
the 1.3 IO hPutStr and associated machinery for communicating with
each other.


\begin{code}
writeSocket :: Socket	-- Connected Socket
	    -> String	-- Data to send
	    -> IO Int	-- Number of Bytes sent
writeSocket (MkSocket s family stype protocol status) xs =
 readVar status				`thenST` \ currentStatus ->
 if not ((currentStatus /= Connected) || (currentStatus /= Listening)) then
    fail ("writeSocket: can't peform write on socket in status " ++
          show currentStatus)
 else
    _ccall_ writeDescriptor s xs (length xs)    `thenPrimIO` \ nbytes ->
    case nbytes of
      -1 ->
	   _constructError "writeSocket"        `thenPrimIO`
	   failWith
      _  ->
	   return nbytes

readSocket :: Socket		-- Connected Socket
	   -> Int		-- Number of Bytes to Read
	   -> IO (String, Int)	-- (Data Read, Number of Bytes)
readSocket (MkSocket s family stype protocol status) nbytes =
 readVar status				`thenST` \ currentStatus ->
 if not ((currentStatus /= Connected) || (currentStatus /= Listening)) then
    fail ("readSocket: can't perform read on socket in status " ++
	  show currentStatus)
 else
    newCharArray (0, nbytes)		`thenPrimIO` \ ptr ->
    _ccall_ readDescriptor s ptr nbytes `thenPrimIO` \ nbytes ->
    case nbytes of
      -1 ->
         _constructError "readSocket"      `thenPrimIO`
	 failWith
      n  ->
	 unsafeFreezeByteArray ptr	`thenPrimIO` \barr ->
         return (_unpackPS (_byteArrayToPS barr), n)


readSocketAll :: Socket -> IO String
readSocketAll s =
    let 
      loop xs =
       handle
        (readSocket s 4096			>>= \ (str, nbytes) ->
	 if nbytes /= 0 then
	    loop (str ++ xs)
	 else
	    return xs)
	(\ _ -> return xs)
    in
    	loop ""
\end{code}

The port number the given socket is currently connected to can be
determined by calling $port$, is generally only useful when bind
was given $aNY\_PORT$.

\begin{code}
socketPort :: Socket		-- Connected & Bound Socket
	   -> IO Int		-- Port Number of Socket
socketPort sock@(MkSocket s AF_INET stype protocol status) =
    getSocketName sock		    >>= \ (SockAddrInet port _) ->
    return port
socketPort (MkSocket s family stype protocol status) =
    fail ("socketPort: not supported for Family " ++ show family)
\end{code}

Calling $getPeerName$ returns the address details of the machine,
other than the local one, which is connected to the socket. This is
used in programs such as FTP to determine where to send the returning
data.  The corresponding call to get the details of the local machine
is $getSocketName$.

\begin{code}
getPeerName   :: Socket -> IO SockAddr
getPeerName (MkSocket s family stype protocol status) =
 allocSockAddr family			    `thenPrimIO` \ (ptr,sz) ->
 newIntArray (0,1)			    `thenPrimIO` \ int_star ->
 writeIntArray int_star 0 sz		    `seqPrimIO`
 _ccall_ getPeerName s ptr int_star         `thenPrimIO` \ status ->
 case status of
   -1 ->
      _constructError "getPeerName"	    `thenPrimIO`
      failWith
   _  ->
      readIntArray int_star 0		    `thenPrimIO` \ sz ->
      unpackSockAddr ptr sz		    `thenPrimIO` \ addr ->
      return addr
    
getSocketName :: Socket -> IO SockAddr
getSocketName (MkSocket s family stype protocol status) =
 allocSockAddr family			    `thenPrimIO` \ (ptr,sz) ->
 newIntArray (0,1)			    `thenPrimIO` \ int_star ->
 writeIntArray int_star 0 sz		    `seqPrimIO`
 _ccall_ getSockName s ptr int_star	    `thenPrimIO` \ status ->
 case status of
   -1 ->
      _constructError "getSocketName"	    `thenPrimIO`
      failWith
   _  ->
      readIntArray int_star 0		    `thenPrimIO` \ sz ->
      unpackSockAddr ptr sz		    `thenPrimIO` \ addr ->
      return addr


\end{code}


%************************************************************************
%*									*
\subsection[Socket-Properties]{Socket Properties}
%*									*
%************************************************************************

\begin{code}
{-
data SocketOption =
      Debug
    | AcceptConnection
    | ReuseAddr
    | KeepAlive
    | DontRoute
    | Broadcast
    | UseLoopBack
    | Linger
    | OOBInline
    | SendBuffer
    | RecvBuffer
    | SendLowWater
    | RecvLowWater
    | SendTimeOut
    | RecvTimeOut
    | Error
    | Type

sOL_SOCKET = ``SOL_SOCKET''

setSocketOptions :: Socket ->
		    Int ->		-- Level
		    SocketOption ->	-- Option Name
		    String ->		-- Option Value
		    IO ()

getSocketOptons :: Socket ->
		   Int ->		-- Level
		   SocketOption ->	-- Option Name
		   IO String		-- Option Value
-}
\end{code}

A calling sequence table for the main functions is shown in the table below.

\begin{figure}[h]
\begin{center}
\begin{tabular}{|l|c|c|c|c|c|c|c|}
\hline
{\bf A Call to} & socket & connect & bindSocket & listen & accept & read & write \\
\hline
{\bf Precedes} & & & & & & & \\
\hline 
socket &	&	  &	       &	&	 &	& \\
\hline
connect & +	&	  &	       &	&	 &	& \\
\hline
bindSocket & +	&	  &	       &	&	 &	& \\
\hline
listen &	&	  & +	       &	&	 &	& \\
\hline
accept &	&	  &	       &  +	&	 &	& \\
\hline
read   &	&   +	  &	       &  +	&  +	 &  +	& + \\
\hline
write  &	&   +	  &	       &  +	&  +	 &  +	& + \\
\hline
\end{tabular}
\caption{Sequence Table for Major functions of Socket}
\label{tab:api-seq}
\end{center}
\end{figure}

%************************************************************************
%*									*
\subsection[Socket-OSDefs]{OS Dependent Definitions}
%*									*
%************************************************************************

    
The following Family and Socket Type declarations were manually derived
from /usr/include/sys/socket.h on the appropriate machines.

Maybe a configure script that could parse the socket.h file to produce
the following declaration is required to make it ``portable'' rather than
using the dreaded \#ifdefs.

Presently only the following machine/os combinations are supported:

\begin{itemize}
\item Intelx86/Linux
\item SPARC/SunOS
\item SPARC/Solaris
\item Alpha/OSF
\end{itemize}

\begin{code}
unpackFamily	:: Int -> Family
packFamily	:: Family -> Int

packSocketType	:: SocketType -> Int
#ifdef sun
 
data Family = 
	  AF_UNSPEC	-- unspecified
	| AF_UNIX	-- local to host (pipes, portals
	| AF_INET	-- internetwork: UDP, TCP, etc
	| AF_IMPLINK	-- arpanet imp addresses
	| AF_PUP	-- pup protocols: e.g. BSP
	| AF_CHAOS	-- mit CHAOS protocols
	| AF_NS		-- XEROX NS protocols 
	| AF_NBS	-- nbs protocols
	| AF_ECMA	-- european computer manufacturers
	| AF_DATAKIT	-- datakit protocols
	| AF_CCITT	-- CCITT protocols, X.25 etc
	| AF_SNA	-- IBM SNA
	| AF_DECnet	-- DECnet
	| AF_DLI	-- Direct data link interface
	| AF_LAT	-- LAT
	| AF_HYLINK	-- NSC Hyperchannel
	| AF_APPLETALK	-- Apple Talk
	| AF_NIT	-- Network Interface Tap
	| AF_802	-- IEEE 80.2, also ISO 8802
	| AF_OSI	-- umberella of all families used by OSI
	| AF_X25	-- CCITT X.25
	| AF_OSINET	-- AFI
	| AF_GOSSIP	-- US Government OSI
	| AF_IPX	-- Novell Internet Protocol
	deriving (Eq, Ord, Ix, Text)
			
packFamily = index (AF_UNSPEC, AF_IPX)
unpackFamily family = (range (AF_UNSPEC, AF_IPX))!!family

#endif

#ifdef __alpha__
       
data Family =
	  AF_UNSPEC	-- unspecified 
	| AF_UNIX	-- local to host (pipes, portals) 
	| AF_INET	-- internetwork: UDP, TCP, etc. 
	| AF_IMPLINK	-- arpanet imp addresses 
	| AF_PUP	-- pup protocols: e.g. BSP 
	| AF_CHAOS	-- mit CHAOS protocols 
	| AF_NS		-- XEROX NS protocols 
	| AF_ISO	-- ISO protocols 
	| AF_ECMA	-- european computer manufacturers 
	| AF_DATAKIT	-- datakit protocols 
	| AF_CCITT	-- CCITT protocols, X.25 etc 
	| AF_SNA	-- IBM SNA 
	| AF_DECnet	-- DECnet 
	| AF_DLI	-- DEC Direct data link interface 
	| AF_LAT	-- LAT 
	| AF_HYLINK	-- NSC Hyperchannel 
	| AF_APPLETALK	-- Apple Talk 
	| AF_ROUTE	-- Internal Routing Protocol 
	| AF_LINK	-- Link layer interface 
	| Pseudo_AF_XTP	-- eXpress Transfer Protocol (no AF) 
	| AF_NETMAN	-- DNA Network Management 
	| AF_X25	-- X25 protocol 
	| AF_CTF	-- Common Trace Facility 
	| AF_WAN	-- Wide Area Network protocols 
	deriving (Eq, Ord, Ix, Text)
  
packFamily = index (AF_UNSPEC, AF_WAN)
unpackFamily family = (range (AF_UNSPEC, AF_WAN))!!family
#endif 

       
#ifdef linux
data Family = 
	  AF_UNSPEC
	| AF_UNIX
	| AF_INET
	| AF_AX25
	| AF_IPX
	deriving (Eq, Ord, Ix, Text)	

packFamily = index (AF_UNSPEC, AF_IPX)
unpackFamily family = (range (AF_UNSPEC, AF_IPX))!!family

#endif

-- Alpha running OSF or a SPARC with SunOS, rather than Solaris.

#if __alpha__ || (sun && !__svr4__)
data SocketType = 
	  Stream 
	| Datagram
	| Raw 
	| RDM 
	| SeqPacket
	deriving (Eq, Ord, Ix, Text)
	
packSocketType stype = 1 + (index (Stream, SeqPacket) stype)	
#endif

-- This is a Sun running Solaris rather than SunOS    

#if sun && __svr4__
data SocketType =
	  Datagram
	| Stream
	| NC_TPI_COTS_ORD
	| Raw
	| RDM
	| SeqPacket
	deriving (Eq, Ord, Ix, Text)	

packSocketType stype = 1 + (index (Datagram, SeqPacket) stype)
#endif	
    

#if linux
data SocketType = 
	  Stream 
	| Datagram
	| Raw 
	| RDM 
	| SeqPacket
	| Packet
	deriving (Eq, Ord, Ix, Text)

packSocketType stype = 1 + (index (Stream, Packet) stype)	
#endif
\end{code}

%************************************************************************
%*									*
\subsection[Socket-Util]{Utility Functions}
%*									*
%************************************************************************

\begin{code}
aNY_PORT = 0::Int
iNADDR_ANY = ``INADDR_ANY''::_Word
sOMAXCONN = ``SOMAXCONN''::Int
maxListenQueue = sOMAXCONN

-------------------------------------------------------------------------------
data ShutdownCmd 
 = ShutdownReceive
 | ShutdownSend
 | ShutdownBoth

sdownCmdToInt :: ShutdownCmd -> Int
sdownCmdToInt ShutdownReceive = 0
sdownCmdToInt ShutdownSend    = 1
sdownCmdToInt ShutdownBoth    = 2

shutdown :: Socket -> ShutdownCmd -> IO ()
shutdown (MkSocket s _ _ _ _) stype = 
  let t = sdownCmdToInt stype in
  _ccall_ shutdownSocket s t `thenPrimIO` \status ->
  case status of
    -1 ->
       _constructError "shutdown" `thenPrimIO`
       failWith
    _ ->
      return ()

-------------------------------------------------------------------------------

sClose	 :: Socket -> IO ()
sClose (MkSocket s family stype protocol status) = 
    primIOToIO (_ccall_ close s)

-------------------------------------------------------------------------------

inet_addr :: String -> HostAddress
inet_addr ipstr = unsafePerformPrimIO (_ccall_ inet_addr ipstr)

-------------------------------------------------------------------------------

inet_ntoa :: HostAddress -> String
inet_ntoa haddr = unsafePerformPrimIO (
    _casm_ ``struct in_addr addr;
	     addr.s_addr = htonl(%0);
	     %r = inet_ntoa (addr);'' haddr    `thenPrimIO` \ str ->
    returnPrimIO (_unpackPS (_packCString str)))

-------------------------------------------------------------------------------

sIsConnected :: Socket -> IO Bool
sIsConnected (MkSocket s family stype protocol status) =
    readVar status			`thenST` \ value ->
    return (value == Connected)	

-------------------------------------------------------------------------------

sIsBound :: Socket -> IO Bool
sIsBound (MkSocket s family stype protocol status) =
    readVar status			`thenST` \ value ->
    return (value == Bound)	

-------------------------------------------------------------------------------

sIsListening :: Socket -> IO Bool
sIsListening (MkSocket s family stype protocol status) =
    readVar status			`thenST` \ value ->
    return (value == Listening)	

-------------------------------------------------------------------------------

sIsReadable  :: Socket -> IO Bool
sIsReadable (MkSocket s family stype protocol status) =
    readVar status			`thenST` \ value ->
    return (value == Listening || value == Connected)

-------------------------------------------------------------------------------

sIsWritable  :: Socket -> IO Bool
sIsWritable = sIsReadable

-------------------------------------------------------------------------------

sIsAcceptable :: Socket -> IO Bool
sIsAcceptable (MkSocket s AF_UNIX Stream protocol status) =
    readVar status			`thenST` \ value ->    
    return (value == Connected || value == Bound || value == Listening)
sIsAcceptable (MkSocket s AF_UNIX _ protocol status) = 
    return False
sIsAcceptable (MkSocket s _ stype protocol status) =
    readVar status			`thenST` \ value ->
    return (value == Connected || value == Listening)
    
-------------------------------------------------------------------------------

{-
sSetBlocking :: Socket -> Bool -> IO ()
sIsBlocking  :: Socket -> IO Bool
-}

-------------------------------------------------------------------------------

allocSockAddr :: Family -> PrimIO (_MutableByteArray _RealWorld Int, Int)
allocSockAddr AF_UNIX = 
    newCharArray (0,``sizeof(struct sockaddr_un)'')	`thenPrimIO` \ ptr ->
    let 
	(_,sz) = boundsOfByteArray ptr
    in 
    returnPrimIO (ptr, sz)
allocSockAddr AF_INET = 
    newCharArray (0,``sizeof(struct sockaddr_in)'')	`thenPrimIO` \ ptr ->
    let 
	(_,sz) = boundsOfByteArray ptr
    in
    returnPrimIO (ptr, sz)

-------------------------------------------------------------------------------

unpackSockAddr :: _MutableByteArray _RealWorld Int -> Int -> PrimIO SockAddr
unpackSockAddr arr len =
    _casm_ ``%r = ((struct sockaddr*)%0)->sa_family;'' arr `thenPrimIO` \ fam ->
    case unpackFamily fam of
	AF_UNIX -> unpackSockAddrUnix arr (len - ``sizeof(short)'')
	AF_INET -> unpackSockAddrInet arr

-------------------------------------------------------------------------------

{-
  sun_path is *not* NULL terminated, hence we *do* the need to know the
  length of it.
-}
unpackSockAddrUnix :: (_MutableByteArray _RealWorld Int) -> Int -> PrimIO SockAddr
unpackSockAddrUnix ptr len =
    _casm_ ``%r = ((struct sockaddr_un*)%0)->sun_path;'' ptr
						`thenPrimIO` \ char_star ->
    _packCBytesST len char_star			`thenPrimIO` \ path ->
    returnPrimIO (SockAddrUnix (_unpackPS path))

-------------------------------------------------------------------------------

unpackSockAddrInet :: (_MutableByteArray _RealWorld Int) -> PrimIO SockAddr
unpackSockAddrInet ptr =
    _casm_ ``%r = ntohs(((struct sockaddr_in*)%0)->sin_port);'' ptr
						`thenPrimIO` \ port ->
    _casm_ ``%r = ntohl(((struct sockaddr_in*)%0)->sin_addr.s_addr);'' ptr
						`thenPrimIO` \ address ->
    returnPrimIO (SockAddrInet port address)

-------------------------------------------------------------------------------


packSockAddr :: SockAddr -> PrimIO (_MutableByteArray _RealWorld Int)
packSockAddr (SockAddrUnix path) =
    allocSockAddr AF_UNIX				`thenPrimIO` \ (ptr,_) ->
    _casm_ ``(((struct sockaddr_un *)%0)->sun_family) = AF_UNIX;''
		ptr					`thenPrimIO` \ () ->
    _casm_ ``strcpy ((((struct sockaddr_un *)%0)->sun_path),%1);''
		ptr path				`thenPrimIO` \ () ->	
    returnPrimIO ptr

packSockAddr (SockAddrInet port address) =
    allocSockAddr AF_INET				`thenPrimIO` \ (ptr,_) ->
    _casm_ ``(((struct sockaddr_in *)%0)->sin_family) = AF_INET;''
		ptr					`thenPrimIO` \ () ->
    _casm_ ``(((struct sockaddr_in *)%0)->sin_port) = htons((int)%1);''
		ptr port				`thenPrimIO` \ () ->
    _casm_ ``(((struct sockaddr_in *)%0)->sin_addr.s_addr) = htonl(%1);''
		ptr address				`thenPrimIO` \ () ->
    returnPrimIO ptr

-------------------------------------------------------------------------------
\end{code}

@socketHandle@ turns a @Socket@ into a 1.3 @Handle@. By default, the new
handle will not be buffered, use @hSetBuffering@ if you want to change
it subsequently.

\begin{code}
socketToHandle :: Socket -> IOMode -> IO Handle
socketToHandle (MkSocket s family stype protocol status) m =
 _casm_ ``%r = fdopen (%0, (char *)%1);'' s m'  `thenPrimIO` \ ptr ->
 _newHandle (htype ptr Nothing False)           >>= \ hndl ->
 hSetBuffering hndl NoBuffering		        >>
 return hndl
 where
  m' = 
   case m of 
     ReadMode      -> "r"
     WriteMode     -> "w"
     AppendMode    -> "a"
     ReadWriteMode -> "r+"
  htype = 
   case m of 
     ReadMode      -> _ReadHandle
     WriteMode     -> _WriteHandle
     AppendMode    -> _AppendHandle
     ReadWriteMode -> _ReadWriteHandle

-------------------------------------------------------------------------------
\end{code}
