%
% (c) The GRASP/AQUA Project, Glasgow University, 1994
%
\section[PrelWriteTextIO]{Haskell 1.3 Text Output}

This module defines the standard set of output operations for writing
characters and strings to text files, using 
$handles$

\begin{code}
module PreludeWriteTextIO (
    hPutChar,
    putChar,
    hPutStr,
    putStr,
    hPutText,
    putText,
    print13,
    writeFile13,
    appendFile13
  ) where

import Cls
import Core
import IChar
import IInt
import IList
import List		( splitAt, (++) )
import Prel		( ord, (.), otherwise, (&&), (||) )
import Text
import TyArray		-- instance _CCallable (_ByteArray a)
import TyComplex

import PreludeIOError
import PreludeMonadicIO
import PreludePrimIO
import PreludeGlaST
import PreludeStdIO
import PS hiding ( _hPutPS )
import Prel (isSpace)

hPutChar :: Handle -> Char -> IO ()
hPutChar handle c =
    _readHandle handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  _writeHandle handle htype		    >>
          failWith ioError
      _ClosedHandle ->
	  _writeHandle handle htype		    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  _writeHandle handle htype		    >>
	  failWith (IllegalOperation "handle is closed")
      _ReadHandle _ _ _ ->
	  _writeHandle handle htype		    >>
	  failWith (IllegalOperation "handle is not open for writing")
      other -> 
	  _ccall_ filePutc (_filePtr other) (ord c) `thenPrimIO` \ rc ->
	  _writeHandle handle (_markHandle htype)   >>
          if rc == 0 then
              return ()
          else
              _constructError "hPutChar"	    `thenPrimIO` \ ioError ->
	      failWith ioError

putChar :: Char -> IO () 
putChar = hPutChar stdout13

\end{code}

Computation $hPutChar hdl c$ writes the character {\em c} to the file
or channel managed by {\em hdl}.  Characters may be buffered if
buffering is enabled for {\em hdl}.

\begin{code}
hPutStr :: Handle -> String -> IO ()
hPutStr handle str = 
    _readHandle handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  _writeHandle handle htype		    >>
          failWith ioError
      _ClosedHandle ->
	  _writeHandle handle htype		    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  _writeHandle handle htype		    >>
	  failWith (IllegalOperation "handle is closed")
      _ReadHandle _ _ _ ->
	  _writeHandle handle htype		    >>
	  failWith (IllegalOperation "handle is not open for writing")
      other -> 
          _getBufferMode other			    `thenPrimIO` \ other ->
          (case _bufferMode other of
            Just LineBuffering ->
		writeLines (_filePtr other) str
            Just (BlockBuffering (Just size)) ->
	        writeBlocks (_filePtr other) size str
            Just (BlockBuffering Nothing) ->
	        writeBlocks (_filePtr other) ``BUFSIZ'' str
            _ -> -- Nothing is treated pessimistically as NoBuffering
	        writeChars (_filePtr other) str
	  )    					    `thenPrimIO` \ success ->
	    _writeHandle handle (_markHandle other) `seqPrimIO`
          if success then
              return ()
          else
              _constructError "hPutStr"   	    `thenPrimIO` \ ioError ->
	      failWith ioError

  where
    writeLines :: _Addr -> String -> PrimIO Bool
    writeLines = writeChunks ``BUFSIZ'' True 

    writeBlocks :: _Addr -> Int -> String -> PrimIO Bool
    writeBlocks fp size s = writeChunks size False fp s
 
     {-
       The breaking up of output into lines along \n boundaries
       works fine as long as there are newlines to split by.
       Avoid the splitting up into lines alltogether (doesn't work
       for overly long lines like the stuff that showsPrec instances
       normally return). Instead, we split them up into fixed size
       chunks before blasting them off to the Real World.

       Hacked to avoid multiple passes over the strings - unsightly, but
       a whole lot quicker. -- SOF 3/96
     -}

    writeChunks :: Int -> Bool -> _Addr -> String -> PrimIO Bool
    writeChunks (I# bufLen) chopOnNewLine fp s =
     newCharArray (0,I# bufLen) `thenPrimIO` \ arr@(_MutableByteArray _ arr#) ->
     let
      shoveString :: Int# -> [Char] -> PrimIO Bool
      shoveString n ls = 
       case ls of
         [] ->   
	   if n `eqInt#` 0# then
	      returnPrimIO True
	   else
             _ccall_ writeFile arr fp (I# n) `thenPrimIO` \rc ->
             returnPrimIO (rc==0)
         ((C# x):xs) ->
	   (\ (S# s#) ->
              case writeCharArray# arr# n x s# of
	        s1# -> 
		   {- Flushing lines - should we bother? -}
		  (if n `eqInt#` bufLen {- || (chopOnNewLine && (x `eqChar#` '\n'#)) -} then
                     _ccall_ writeFile arr fp (I# (n `plusInt#` 1#)) `thenPrimIO` \rc ->
	             if rc == 0 then
	                shoveString 0# xs
	              else
	                returnPrimIO False
                   else
                      shoveString (n `plusInt#` 1#) xs) (S# s1#))
     in
     shoveString 0# s

    writeChars :: _Addr -> String -> PrimIO Bool
    writeChars fp "" = returnPrimIO True
    writeChars fp (c:cs) =
	_ccall_ filePutc fp (ord c)		    `thenPrimIO` \ rc ->
        if rc == 0 then
	    writeChars fp cs
	else
	    returnPrimIO False

putStr :: String -> IO () 
putStr = hPutStr stdout13

hPutText :: Text a => Handle -> a -> IO ()
hPutText hdl = hPutStr hdl . show

putText :: Text a => a -> IO () 
putText = hPutText stdout13

print13 :: Text a => a -> IO ()
print13 x = putText x >> putChar '\n'

\end{code}

Computation $hPutStr hdl s$ writes the string {\em s} to the file or
channel managed by {\em hdl}.

Computation $putStr s$ writes the string {\em s} to $stdout$.

Computation $hPutText hdl t$ writes the string representation of {\em
t} given by the $shows$ function to the file or channel managed by
{\em hdl}.

\begin{code}

writeFile13 :: FilePath -> String -> IO ()
writeFile13 name str =
 openFile name WriteMode >>= \hdl -> hPutStr hdl str >> hClose hdl

appendFile13 :: FilePath -> String -> IO ()
appendFile13 name str =
 openFile name AppendMode >>= \hdl -> hPutStr hdl str >> hClose hdl

\end{code}

$writeFile file s$ replaces the contents of {\em file} by the string
{\em s}. $appendFile file s$ appends string {\em s} to {\em file}.
