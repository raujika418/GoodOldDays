%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

\section[I386RegInfo]{Info about ix86 registers}

\begin{code}
#include "HsVersions.h"

module I386RegInfo (
	stgRegMap, baseRegOffset, callerSaves, freeReg, reservedRegs
	)  where

import AbsCSyn	    	( MagicId(..), PrimKind )
import AsmRegAlloc  	( Reg(..), Unique )

import Outputable
import Util

\end{code}

If you value your sanity, do not venture below this line.

\begin{code}

-- platform.h is generated and tells us what the target architecture is
#include "../../includes/platform.h"
#define STOLEN_X86_REGS 5
#include "../../includes/MachRegs.h"
#if i386_TARGET_ARCH
#if linuxaout_TARGET_OS
#include "../../includes/i386-unknown-linuxaout.h"
#elif freebsd_TARGET_OS
#include "../../includes/i386-unknown-freebsd2.1.0.h"
#endif
#endif

-- Redefine the literals used for I386 register names in the header
-- files.  Gag me with a spoon, eh?

#define eax 0
#define ebx 1
#define ecx 2
#define edx 3
#define esi 4
#define edi 5
#define ebp 6
#define esp 7
#define st0 8
#define st1 9
#define st2 10
#define st3 11
#define st4 12
#define st5 13
#define st6 14
#define st7 15

baseRegOffset :: MagicId -> Int
baseRegOffset StkOReg			= OFFSET_StkO
baseRegOffset (VanillaReg _ ILIT(1))	= OFFSET_R1
baseRegOffset (VanillaReg _ ILIT(2))	= OFFSET_R2
baseRegOffset (VanillaReg _ ILIT(3))	= OFFSET_R3
baseRegOffset (VanillaReg _ ILIT(4))	= OFFSET_R4
baseRegOffset (VanillaReg _ ILIT(5))	= OFFSET_R5
baseRegOffset (VanillaReg _ ILIT(6))	= OFFSET_R6
baseRegOffset (VanillaReg _ ILIT(7))	= OFFSET_R7
baseRegOffset (VanillaReg _ ILIT(8))	= OFFSET_R8
baseRegOffset (FloatReg ILIT(1))	= OFFSET_Flt1
baseRegOffset (FloatReg ILIT(2))	= OFFSET_Flt2
baseRegOffset (FloatReg ILIT(3))	= OFFSET_Flt3
baseRegOffset (FloatReg ILIT(4))	= OFFSET_Flt4
baseRegOffset (DoubleReg ILIT(1))	= OFFSET_Dbl1
baseRegOffset (DoubleReg ILIT(2))	= OFFSET_Dbl2
baseRegOffset TagReg			= OFFSET_Tag
baseRegOffset RetReg			= OFFSET_Ret
baseRegOffset SpA			= OFFSET_SpA
baseRegOffset SuA			= OFFSET_SuA
baseRegOffset SpB			= OFFSET_SpB
baseRegOffset SuB			= OFFSET_SuB
baseRegOffset Hp			= OFFSET_Hp
baseRegOffset HpLim			= OFFSET_HpLim
baseRegOffset LivenessReg		= OFFSET_Liveness
--baseRegOffset ActivityReg		= OFFSET_Activity
#ifdef DEBUG
baseRegOffset BaseReg			= panic "baseRegOffset:BaseReg"
baseRegOffset StdUpdRetVecReg		= panic "baseRegOffset:StgUpdRetVecReg"
baseRegOffset StkStubReg		= panic "baseRegOffset:StkStubReg"
baseRegOffset CurCostCentre		= panic "baseRegOffset:CurCostCentre"
baseRegOffset VoidReg			= panic "baseRegOffset:VoidReg"
#endif

callerSaves :: MagicId -> Bool
#ifdef CALLER_SAVES_Base
callerSaves BaseReg    	    	= True
#endif
#ifdef CALLER_SAVES_StkO
callerSaves StkOReg         	= True
#endif
#ifdef CALLER_SAVES_R1
callerSaves (VanillaReg _ ILIT(1))	= True
#endif
#ifdef CALLER_SAVES_R2
callerSaves (VanillaReg _ ILIT(2))    = True
#endif
#ifdef CALLER_SAVES_R3
callerSaves (VanillaReg _ ILIT(3))    = True
#endif
#ifdef CALLER_SAVES_R4
callerSaves (VanillaReg _ ILIT(4))    = True
#endif
#ifdef CALLER_SAVES_R5
callerSaves (VanillaReg _ ILIT(5))    = True
#endif
#ifdef CALLER_SAVES_R6
callerSaves (VanillaReg _ ILIT(6))    = True
#endif
#ifdef CALLER_SAVES_R7
callerSaves (VanillaReg _ ILIT(7))	= True
#endif
#ifdef CALLER_SAVES_R8
callerSaves (VanillaReg _ ILIT(8))    = True
#endif
#ifdef CALLER_SAVES_FltReg1
callerSaves (FloatReg ILIT(1))   	= True
#endif
#ifdef CALLER_SAVES_FltReg2
callerSaves (FloatReg ILIT(2))   	= True
#endif
#ifdef CALLER_SAVES_FltReg3
callerSaves (FloatReg ILIT(3))   	= True
#endif
#ifdef CALLER_SAVES_FltReg4
callerSaves (FloatReg ILIT(4))    	= True
#endif
#ifdef CALLER_SAVES_DblReg1
callerSaves (DoubleReg ILIT(1))    	= True
#endif
#ifdef CALLER_SAVES_DblReg2
callerSaves (DoubleReg ILIT(2))    	= True
#endif
#ifdef CALLER_SAVES_Tag
callerSaves TagReg      	= True
#endif
#ifdef CALLER_SAVES_Ret
callerSaves RetReg      	= True
#endif
#ifdef CALLER_SAVES_SpA
callerSaves SpA	    	    	= True
#endif
#ifdef CALLER_SAVES_SuA
callerSaves SuA	    	    	= True
#endif
#ifdef CALLER_SAVES_SpB
callerSaves SpB	    	    	= True
#endif
#ifdef CALLER_SAVES_SuB
callerSaves SuB	    	    	= True
#endif
#ifdef CALLER_SAVES_Hp 
callerSaves Hp	    	    	= True
#endif
#ifdef CALLER_SAVES_HpLim
callerSaves HpLim   	    	= True
#endif
#ifdef CALLER_SAVES_Liveness
callerSaves LivenessReg	        = True
#endif
#ifdef CALLER_SAVES_Activity
--callerSaves ActivityReg	        = True
#endif
#ifdef CALLER_SAVES_StdUpdRetVec
callerSaves StdUpdRetVecReg    	= True
#endif
#ifdef CALLER_SAVES_StkStub
callerSaves StkStubReg 	    	= True
#endif
callerSaves _	    	    	= False

stgRegMap :: MagicId -> Maybe Reg

#ifdef REG_Base
stgRegMap BaseReg	   = Just (FixedReg ILIT(REG_Base))
#endif
#ifdef REG_StkO
stgRegMap StkOReg   	   = Just (FixedReg ILIT(REG_StkOReg))
#endif
#ifdef REG_R1
stgRegMap (VanillaReg _ ILIT(1)) = Just (FixedReg ILIT(REG_R1))
#endif
#ifdef REG_R2
stgRegMap (VanillaReg _ ILIT(2)) = Just (FixedReg ILIT(REG_R2))
#endif
#ifdef REG_R3
stgRegMap (VanillaReg _ ILIT(3)) = Just (FixedReg ILIT(REG_R3))
#endif
#ifdef REG_R4
stgRegMap (VanillaReg _ ILIT(4)) = Just (FixedReg ILIT(REG_R4))
#endif
#ifdef REG_R5
stgRegMap (VanillaReg _ ILIT(5)) = Just (FixedReg ILIT(REG_R5))
#endif
#ifdef REG_R6
stgRegMap (VanillaReg _ ILIT(6)) = Just (FixedReg ILIT(REG_R6))
#endif
#ifdef REG_R7
stgRegMap (VanillaReg _ ILIT(7)) = Just (FixedReg ILIT(REG_R7))
#endif
#ifdef REG_R8
stgRegMap (VanillaReg _ ILIT(8)) = Just (FixedReg ILIT(REG_R8))
#endif
#ifdef REG_Flt1
stgRegMap (FloatReg ILIT(1)) 	   = Just (FixedReg ILIT(REG_Flt1))
#endif
#ifdef REG_Flt2
stgRegMap (FloatReg ILIT(2)) 	   = Just (FixedReg ILIT(REG_Flt2))
#endif
#ifdef REG_Flt3
stgRegMap (FloatReg ILIT(3)) 	   = Just (FixedReg ILIT(REG_Flt3))
#endif
#ifdef REG_Flt4
stgRegMap (FloatReg ILIT(4)) 	   = Just (FixedReg ILIT(REG_Flt4))
#endif
#ifdef REG_Dbl1
stgRegMap (DoubleReg ILIT(1))	   = Just (FixedReg ILIT(REG_Dbl1))
#endif
#ifdef REG_Dbl2
stgRegMap (DoubleReg ILIT(2))	   = Just (FixedReg ILIT(REG_Dbl2))
#endif
#ifdef REG_Tag
stgRegMap TagReg    	   = Just (FixedReg ILIT(REG_TagReg))
#endif
#ifdef REG_Ret
stgRegMap RetReg    	   = Just (FixedReg ILIT(REG_Ret))
#endif
#ifdef REG_SpA
stgRegMap SpA	    	   = Just (FixedReg ILIT(REG_SpA))
#endif
#ifdef REG_SuA
stgRegMap SuA	    	   = Just (FixedReg ILIT(REG_SuA))
#endif
#ifdef REG_SpB
stgRegMap SpB	    	   = Just (FixedReg ILIT(REG_SpB))
#endif
#ifdef REG_SuB
stgRegMap SuB	    	   = Just (FixedReg ILIT(REG_SuB))
#endif
#ifdef REG_Hp 
stgRegMap Hp	    	   = Just (FixedReg ILIT(REG_Hp))
#endif
#ifdef REG_HpLim
stgRegMap HpLim	    	   = Just (FixedReg ILIT(REG_HpLim))
#endif
#ifdef REG_Liveness
stgRegMap LivenessReg	   = Just (FixedReg ILIT(REG_Liveness))
#endif
#ifdef REG_Activity
--stgRegMap ActivityReg	   = Just (FixedReg ILIT(REG_Activity))
#endif
#ifdef REG_StdUpdRetVec
stgRegMap StdUpdRetVecReg  = Just (FixedReg ILIT(REG_StdUpdRetVec))
#endif
#ifdef REG_StkStub
stgRegMap StkStubReg	   = Just (FixedReg ILIT(REG_StkStub))
#endif

stgRegMap _		   = Nothing

\end{code}

Here is the list of registers we can use in register allocation.

\begin{code}
freeReg :: FAST_INT -> FAST_BOOL

freeReg ILIT(esp) = _FALSE_  --	%esp is our stack pointer.

#ifdef REG_Base
freeReg ILIT(REG_Base) = _FALSE_
#endif
#ifdef REG_StkO
freeReg ILIT(REG_StkO) = _FALSE_
#endif
#ifdef REG_R1
freeReg ILIT(REG_R1) = _FALSE_
#endif
#ifdef REG_R2
freeReg ILIT(REG_R2) = _FALSE_
#endif
#ifdef REG_R3
freeReg ILIT(REG_R3) = _FALSE_
#endif
#ifdef REG_R4
freeReg ILIT(REG_R4) = _FALSE_
#endif
#ifdef REG_R5
freeReg ILIT(REG_R5) = _FALSE_
#endif
#ifdef REG_R6
freeReg ILIT(REG_R6) = _FALSE_
#endif
#ifdef REG_R7
freeReg ILIT(REG_R7) = _FALSE_
#endif
#ifdef REG_R8
freeReg ILIT(REG_R8) = _FALSE_
#endif
#ifdef REG_Flt1
freeReg ILIT(REG_Flt1) = _FALSE_
#endif
#ifdef REG_Flt2
freeReg ILIT(REG_Flt2) = _FALSE_
#endif
#ifdef REG_Flt3
freeReg ILIT(REG_Flt3) = _FALSE_
#endif
#ifdef REG_Flt4
freeReg ILIT(REG_Flt4) = _FALSE_
#endif
#ifdef REG_Dbl1
freeReg ILIT(REG_Dbl1) = _FALSE_
#endif
#ifdef REG_Dbl2
freeReg ILIT(REG_Dbl2) = _FALSE_
#endif
#ifdef REG_Tag
freeReg ILIT(REG_Tag) = _FALSE_
#endif
#ifdef REG_Ret
freeReg ILIT(REG_Ret) = _FALSE_
#endif
#ifdef REG_SpA
freeReg ILIT(REG_SpA) = _FALSE_
#endif
#ifdef REG_SuA
freeReg ILIT(REG_SuA) = _FALSE_
#endif
#ifdef REG_SpB
freeReg ILIT(REG_SpB) = _FALSE_
#endif
#ifdef REG_SuB
freeReg ILIT(REG_SuB) = _FALSE_
#endif
#ifdef REG_Hp
freeReg ILIT(REG_Hp) = _FALSE_
#endif
#ifdef REG_HpLim
freeReg ILIT(REG_HpLim) = _FALSE_
#endif
#ifdef REG_Liveness
freeReg ILIT(REG_Liveness) = _FALSE_
#endif
#ifdef REG_Activity
--freeReg ILIT(REG_Activity) = _FALSE_
#endif
#ifdef REG_StdUpdRetVec
freeReg ILIT(REG_StdUpdRetVec) = _FALSE_
#endif
#ifdef REG_StkStub
freeReg ILIT(REG_StkStub) = _FALSE_
#endif
freeReg n
#ifdef REG_Dbl1
  | n _EQ_ (ILIT(REG_Dbl1) _ADD_ ILIT(1)) = _FALSE_
#endif
#ifdef REG_Dbl2
  | n _EQ_ (ILIT(REG_Dbl2) _ADD_ ILIT(1)) = _FALSE_
#endif

  | otherwise = _TRUE_

reservedRegs :: [Int]
reservedRegs = []
--reservedRegs = [NCG_Reserved_I1, NCG_Reserved_I2,
--    	    	NCG_Reserved_F1, NCG_Reserved_F2,
--    	    	NCG_Reserved_D1, NCG_Reserved_D2]

\end{code}
