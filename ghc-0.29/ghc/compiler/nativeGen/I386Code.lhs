%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

\section[I386Code]{The Native (I386) Machine Code}

\begin{code}
#include "HsVersions.h"

module I386Code (
	Addr(..), 
        Cond(..), Imm(..), Operand(..), Size(..),
        Base(..), Index(..), Displacement(..),
	I386Code(..),I386Instr(..),I386Regs,
	strImmLit, --UNUSED: strImmLab,
        spRel,

	is13Bits, offset,

    	kindToSize,

	freeRegs,

    	st0, st1, eax, ebx, ecx, edx, esi, edi, ebp, esp,

	-- and, for self-sufficiency ...
	CLabel, CodeSegment, OrdList, PrimKind, UniqSet(..),
	UniqFM, FiniteMap, Unique, MagicId, BitSet
    ) where

IMPORT_Trace

import I386RegInfo

import AbsCSyn	    	( MagicId(..) )
import AsmRegAlloc  	( MachineCode(..), MachineRegisters(..), FutureLive(..),
			  Reg(..), RegUsage(..), RegLiveness(..) )
import BitSet	 
import CgCompInfo   	( mAX_Double_REG, mAX_Float_REG, mAX_Vanilla_REG )
import CLabelInfo   	( CLabel, pprCLabel )
import FiniteMap    
import Maybes	    	( Maybe(..), maybeToBool )
import OrdList	    	( OrdList, mkUnitList, flattenOrdList )
import Outputable    
import PrimKind	    	( PrimKind(..) )
import Unpretty
import UniqSet
import Stix
import Util
\end{code}

%************************************************************************
%*									*
\subsection[I386Reg]{The Native (I386) Machine Register Table}
%*									*
%************************************************************************

- All registers except 7 (esp) are available for use.
- Only ebx, esi, edi and esp are available across a C call (they are callee-saves).
- Registers 0-7 have 16-bit counterparts (ax, bx etc.)
- Registers 0-3 have 8 bit counterparts (ah, bh etc.)
- Registers 8-15 hold extended floating point values.

\begin{code}

gReg,fReg :: Int -> Int
gReg x = x
fReg x = (8 + x)

st0, st1, st2, st3, st4, st5, st6, st7, eax, ebx, ecx, edx, esp :: Reg
eax = case (gReg 0) of { IBOX(g0) -> FixedReg g0 }
ebx = case (gReg 1) of { IBOX(g1) -> FixedReg g1 }
ecx = case (gReg 2) of { IBOX(g2) -> FixedReg g2 }
edx = case (gReg 3) of { IBOX(g3) -> FixedReg g3 }
esi = case (gReg 4) of { IBOX(g4) -> FixedReg g4 }
edi = case (gReg 5) of { IBOX(g5) -> FixedReg g5 }
ebp = case (gReg 6) of { IBOX(g6) -> FixedReg g6 }
esp = case (gReg 7) of { IBOX(g7) -> FixedReg g7 }
st0 = realReg  (fReg 0)
st1 = realReg  (fReg 1)
st2 = realReg  (fReg 2)
st3 = realReg  (fReg 3)
st4 = realReg  (fReg 4)
st5 = realReg  (fReg 5)
st6 = realReg  (fReg 6)
st7 = realReg  (fReg 7)

realReg n@IBOX(i) = if _IS_TRUE_(freeReg i) then MappedReg i else FixedReg i

\end{code}

%************************************************************************
%*									*
\subsection[TheI386Code]{The datatype for i386 assembly language}
%*									*
%************************************************************************

Here is a definition of the I386 assembly language.

\begin{code}

data Imm = ImmInt Int
    	 | ImmInteger Integer	      -- Sigh.
	 | ImmCLbl CLabel	      -- AbstractC Label (with baggage)
	 | ImmLab  Unpretty	      -- Simple string label (underscored)
	 | ImmLit Unpretty	      -- Simple string
	 deriving ()

--UNUSED:strImmLab s = ImmLab (uppStr s)
strImmLit s = ImmLit (uppStr s)

data Cond = ALWAYS
	  | GEU
	  | LU
	  | EQ
	  | GT
	  | GE
	  | GU
	  | LT
	  | LE
	  | LEU
	  | NE
	  | NEG
	  | POS
	  deriving ()


data Size = B
	  | HB
	  | S -- unused ?
	  | L
	  | F
	  | D
	  deriving ()

data Operand = OpReg  Reg	-- register
             | OpImm  Imm	-- immediate value
             | OpAddr Addr	-- memory reference
	     deriving ()

data Addr = Addr Base Index Displacement
          | ImmAddr Imm Int
          -- deriving Eq

type Base         = Maybe Reg
type Index        = Maybe (Reg, Int)	-- Int is 2, 4 or 8
type Displacement = Imm

data I386Instr =

-- Moves.

		MOV	      Size Operand Operand 
	      | MOVZX	      Size Operand Operand -- size is the size of operand 2
	      | MOVSX	      Size Operand Operand -- size is the size of operand 2

-- Load effective address (also a very useful three-operand add instruction :-)

              | LEA           Size Operand Operand

-- Int Arithmetic.

	      | ADD	      Size Operand Operand 
	      | SUB	      Size Operand Operand 

-- Multiplication (signed and unsigned), Division (signed and unsigned),
-- result in %eax, %edx.

	      | IMUL	      Size Operand Operand
	      | IDIV	      Size Operand

-- Simple bit-twiddling.

	      | AND	      Size Operand Operand 
	      | OR	      Size Operand Operand 
	      | XOR	      Size Operand Operand 
	      | NOT	      Size Operand 
	      | NEGI	      Size Operand -- NEG instruction (name clash with Cond)
	      | SHL	      Size Operand Operand -- 1st operand must be an Imm
	      | SAR	      Size Operand Operand -- 1st operand must be an Imm
	      | SHR	      Size Operand Operand -- 1st operand must be an Imm
	      | NOP	      

-- Note that we cheat by treating F{ABS,MOV,NEG} of doubles as single instructions
-- right up until we spit them out.

	      | SAHF	      -- stores ah into flags
    	      | FABS          
	      | FADD	      Size Operand -- src
	      | FADDP	      
	      | FIADD	      Size Addr -- src
    	      | FCHS          
    	      | FCOM	      Size Operand -- src
    	      | FCOS          
	      | FDIV	      Size Operand -- src
	      | FDIVP	      
	      | FIDIV	      Size Addr -- src
	      | FDIVR	      Size Operand -- src
	      | FDIVRP	      
	      | FIDIVR	      Size Addr -- src
    	      | FICOM	      Size Addr -- src
    	      | FILD	      Size Addr Reg -- src, dst
    	      | FIST	      Size Addr -- dst
    	      | FLD	      Size Operand -- src
    	      | FLD1          
    	      | FLDZ          
    	      | FMUL	      Size Operand -- src
    	      | FMULP	      
    	      | FIMUL	      Size Addr -- src
    	      | FRNDINT       
    	      | FSIN          
    	      | FSQRT         
    	      | FST	      Size Operand -- dst
    	      | FSTP	      Size Operand -- dst
	      | FSUB	      Size Operand -- src
	      | FSUBP	      
	      | FISUB	      Size Addr -- src
	      | FSUBR	      Size Operand -- src
	      | FSUBRP	      
	      | FISUBR	      Size Addr -- src
	      | FTST          
    	      | FCOMP	      Size Operand -- src
    	      | FUCOMPP	      
	      | FXCH
	      | FNSTSW
	      | FNOP

-- Comparison
        
              | TEST          Size Operand Operand
              | CMP           Size Operand Operand
              | SETCC         Cond Operand

-- Stack Operations.

              | PUSH          Size Operand
              | POP           Size Operand

-- Jumping around.

	      | JMP	      Operand -- target
	      | JXX	      Cond CLabel -- target
	      | CALL	      Imm 

-- Other things.

              | CLTD -- sign extend %eax into %edx:%eax

-- Pseudo-ops.

	      | LABEL CLabel
	      | COMMENT FAST_STRING
	      | SEGMENT CodeSegment
	      | ASCII Bool String   -- needs backslash conversion?
	      | DATA Size [Imm]

type I386Code	= OrdList I386Instr

\end{code}

%************************************************************************
%*									*
\subsection[Schedule]{Register allocation information}
%*									*
%************************************************************************

\begin{code}

data I386Regs = SRegs BitSet BitSet

instance MachineRegisters I386Regs where
    mkMRegs xs = SRegs (mkBS ints) (mkBS floats')
      where
    	(ints, floats) = partition (< 8) xs
    	floats' = map (subtract 8) floats

    possibleMRegs FloatKind (SRegs _ floats) = [ x + 8 | x <- listBS floats]
    possibleMRegs DoubleKind (SRegs _ floats) = [ x + 8 | x <- listBS floats]
    possibleMRegs _ (SRegs ints _) = listBS ints

    useMReg (SRegs ints floats) n =
    	if n _LT_ ILIT(8) then SRegs (ints `minusBS` singletonBS IBOX(n)) floats
    	else SRegs ints (floats `minusBS` singletonBS (IBOX(n _SUB_ ILIT(8))))

    useMRegs (SRegs ints floats) xs =
    	SRegs (ints `minusBS` ints')
    	      (floats `minusBS` floats')
      where
        SRegs ints' floats' = mkMRegs xs

    freeMReg (SRegs ints floats) n =
    	if n _LT_ ILIT(8) then SRegs (ints `unionBS` singletonBS IBOX(n)) floats
    	else SRegs ints (floats `unionBS` singletonBS (IBOX(n _SUB_ ILIT(8))))

    freeMRegs (SRegs ints floats) xs =
        SRegs (ints `unionBS` ints')
    	      (floats `unionBS` floats')
      where
        SRegs ints' floats' = mkMRegs xs

instance MachineCode I386Instr where
    -- Alas, we don't do anything clever with our OrdLists
--OLD:
--  flatten = flattenOrdList

    regUsage = i386RegUsage
    regLiveness = i386RegLiveness
    patchRegs = i386PatchRegs

    -- We spill just below the stack pointer, leaving two words per spill location.
    spillReg dyn (MemoryReg i pk) 
      = trace "spillsave"
        (mkUnitList (MOV (kindToSize pk) (OpReg dyn) (OpAddr (spRel (-2 * i)))))
    loadReg (MemoryReg i pk) dyn 
      = trace "spillload"
        (mkUnitList (MOV (kindToSize pk) (OpAddr (spRel (-2 * i))) (OpReg dyn)))

--spRel gives us a stack relative addressing mode for volatile temporaries
--and for excess call arguments.

spRel  
    :: Int      -- desired stack offset in words, positive or negative
    -> Addr
spRel n = Addr (Just esp) Nothing (ImmInt (n * 4))

kindToSize :: PrimKind -> Size
kindToSize PtrKind	    = L
kindToSize CodePtrKind	    = L
kindToSize DataPtrKind	    = L
kindToSize RetKind	    = L
kindToSize InfoPtrKind	    = L
kindToSize CostCentreKind   = L
kindToSize CharKind	    = L
kindToSize IntKind	    = L
kindToSize WordKind	    = L
kindToSize AddrKind	    = L
kindToSize FloatKind	    = F
kindToSize DoubleKind	    = D
kindToSize ArrayKind	    = L
kindToSize ByteArrayKind    = L
kindToSize StablePtrKind    = L
kindToSize ForeignObjKind   = L

\end{code}

@i386RegUsage@ returns the sets of src and destination registers used by
a particular instruction.  Machine registers that are pre-allocated
to stgRegs are filtered out, because they are uninteresting from a
register allocation standpoint.  (We wouldn't want them to end up on
the free list!)

\begin{code}

i386RegUsage :: I386Instr -> RegUsage
i386RegUsage instr = case instr of
    MOV  sz src dst	-> usage2 src dst
    MOVZX sz src dst	-> usage2 src dst
    MOVSX sz src dst	-> usage2 src dst
    LEA  sz src dst	-> usage2 src dst
    ADD  sz src dst	-> usage2 src dst
    SUB  sz src dst	-> usage2 src dst
    IMUL sz src dst	-> usage2 src dst
    IDIV sz src		-> usage (eax:edx:opToReg src) [eax,edx]
    AND  sz src dst	-> usage2 src dst
    OR   sz src dst	-> usage2 src dst
    XOR  sz src dst	-> usage2 src dst
    NOT  sz op		-> usage1 op
    NEGI sz op		-> usage1 op
    SHL  sz imm dst	-> usage1 dst -- imm has to be an Imm
    SAR  sz imm dst	-> usage1 dst -- imm has to be an Imm
    SHR  sz imm dst	-> usage1 dst -- imm has to be an Imm
    PUSH sz op		-> usage (opToReg op) []
    POP  sz op		-> usage [] (opToReg op)
    TEST sz src dst	-> usage (opToReg src ++ opToReg dst) []
    CMP  sz src dst	-> usage (opToReg src ++ opToReg dst) []
    SETCC cond op	-> usage [] (opToReg op)
    JXX cond label	-> usage [] []
    JMP op		-> usage (opToReg op) freeRegs
    CALL imm		-> usage [] callClobberedRegs
    CLTD		-> usage [eax] [edx]
    NOP			-> usage [] []
    SAHF 		-> usage [eax] []
    FABS 		-> usage [st0] [st0]
    FADD sz src		-> usage (st0:opToReg src) [st0] -- allFPRegs
    FADDP 		-> usage [st0,st1] [st0] -- allFPRegs
    FIADD sz asrc	-> usage (addrToRegs asrc) [st0]
    FCHS 		-> usage [st0] [st0]
    FCOM sz src		-> usage (st0:opToReg src) []
    FCOS 		-> usage [st0] [st0]
    FDIV sz src 	-> usage (st0:opToReg src) [st0]
    FDIVP  		-> usage [st0,st1] [st0]
    FDIVRP 		-> usage [st0,st1] [st0]
    FIDIV sz asrc	-> usage (addrToRegs asrc) [st0]
    FDIVR sz src 	-> usage (st0:opToReg src) [st0]
    FIDIVR sz asrc	-> usage (addrToRegs asrc) [st0]
    FICOM sz asrc	-> usage (addrToRegs asrc) []
    FILD sz asrc dst	-> usage (addrToRegs asrc) [dst] -- allFPRegs
    FIST sz adst	-> usage (st0:addrToRegs adst) []
    FLD	 sz src 	-> usage (opToReg src) [st0] -- allFPRegs
    FLD1 		-> usage [] [st0] -- allFPRegs
    FLDZ 		-> usage [] [st0] -- allFPRegs
    FMUL sz src 	-> usage (st0:opToReg src) [st0]
    FMULP 	 	-> usage [st0,st1] [st0]
    FIMUL sz asrc	-> usage (addrToRegs asrc) [st0]
    FRNDINT 		-> usage [st0] [st0]
    FSIN 		-> usage [st0] [st0]
    FSQRT 		-> usage [st0] [st0]
    FST sz (OpReg r)	-> usage [st0] [r]
    FST sz dst		-> usage (st0:opToReg dst) []
    FSTP sz (OpReg r)	-> usage [st0] [r] -- allFPRegs
    FSTP sz dst		-> usage (st0:opToReg dst) [] -- allFPRegs
    FSUB sz src		-> usage (st0:opToReg src) [st0] -- allFPRegs
    FSUBR sz src	-> usage (st0:opToReg src) [st0] -- allFPRegs
    FISUB sz asrc	-> usage (addrToRegs asrc) [st0]
    FSUBP 		-> usage [st0,st1] [st0] -- allFPRegs
    FSUBRP 		-> usage [st0,st1] [st0] -- allFPRegs
    FISUBR sz asrc	-> usage (addrToRegs asrc) [st0]
    FTST 		-> usage [st0] []
    FCOMP sz op		-> usage (st0:opToReg op) [st0] -- allFPRegs
    FUCOMPP 		-> usage [st0, st1] [] --  allFPRegs
    FXCH		-> usage [st0, st1] [st0, st1]
    FNSTSW		-> usage [] [eax]
    _			-> noUsage

 where

    usage2 :: Operand -> Operand -> RegUsage
    usage2 op (OpReg reg) = usage (opToReg op) [reg]
    usage2 op (OpAddr ea) = usage (opToReg op ++ addrToRegs ea) []
    usage2 op (OpImm imm) = usage (opToReg op) []
    usage1 :: Operand -> RegUsage
    usage1 (OpReg reg)    = usage [reg] [reg]
    usage1 (OpAddr ea)    = usage (addrToRegs ea) []
    allFPRegs = [st0,st1,st2,st3,st4,st5,st6,st7]
    --callClobberedRegs = [ eax, ecx, edx ] -- according to gcc, anyway.
    callClobberedRegs = [eax] 

-- General purpose register collecting functions.

    opToReg (OpReg reg)   = [reg]
    opToReg (OpImm imm)   = []
    opToReg (OpAddr  ea)  = addrToRegs ea

    addrToRegs (Addr base index _) = baseToReg base ++ indexToReg index
      where  baseToReg Nothing       = []
             baseToReg (Just r)      = [r]
             indexToReg Nothing      = []
             indexToReg (Just (r,_)) = [r]
    addrToRegs (ImmAddr _ _) = []

    usage src dst = RU (mkUniqSet (filter interesting src))
    	    	       (mkUniqSet (filter interesting dst))

    interesting (FixedReg _) = False
    interesting _ = True

freeRegs :: [Reg]
freeRegs = freeMappedRegs (\ x -> x) [0..15]

freeMappedRegs :: (Int -> Int) -> [Int] -> [Reg]

freeMappedRegs modify nums
  = foldr free [] nums
  where
    free n acc
      = let
	    modified_i = case (modify n) of { IBOX(x) -> x }
	in
	if _IS_TRUE_(freeReg modified_i) then (MappedReg modified_i) : acc else acc

freeSet :: UniqSet Reg
freeSet = mkUniqSet freeRegs

noUsage :: RegUsage
noUsage = RU emptyUniqSet emptyUniqSet

endUsage :: RegUsage
endUsage = RU emptyUniqSet freeSet

\end{code}

@i386RegLiveness@ takes future liveness information and modifies it according to
the semantics of branches and labels.  (An out-of-line branch clobbers the liveness
passed back by the following instruction; a forward local branch passes back the
liveness from the target label; a conditional branch merges the liveness from the
target and the liveness from its successor; a label stashes away the current liveness
in the future liveness environment).

\begin{code}
i386RegLiveness :: I386Instr -> RegLiveness -> RegLiveness
i386RegLiveness instr info@(RL live future@(FL all env)) = case instr of

    JXX _ lbl	-> RL (lookup lbl `unionUniqSets` live) future
    JMP _	-> RL emptyUniqSet future
    CALL _      -> RL live future
    LABEL lbl   -> RL live (FL (all `unionUniqSets` live) (addToFM env lbl live))
    _		    -> info

  where
    lookup lbl = case lookupFM env lbl of
	Just regs -> regs
	Nothing -> trace ("Missing " ++ (uppShow 80 (pprCLabel (PprForAsm (\_->False) False id) lbl)) ++
                          " in future?") emptyUniqSet

\end{code}

@i386PatchRegs@ takes an instruction (possibly with MemoryReg/UnmappedReg registers) and
changes all register references according to the supplied environment.

\begin{code}

i386PatchRegs :: I386Instr -> (Reg -> Reg) -> I386Instr
i386PatchRegs instr env = case instr of
    MOV  sz src dst	-> patch2 (MOV  sz) src dst
    MOVZX sz src dst	-> patch2 (MOVZX sz) src dst
    MOVSX sz src dst	-> patch2 (MOVSX sz) src dst
    LEA  sz src dst	-> patch2 (LEA  sz) src dst
    ADD  sz src dst	-> patch2 (ADD  sz) src dst
    SUB  sz src dst	-> patch2 (SUB  sz) src dst
    IMUL sz src dst 	-> patch2 (IMUL sz) src dst
    IDIV sz src  	-> patch1 (IDIV sz) src 
    AND  sz src dst	-> patch2 (AND  sz) src dst
    OR   sz src dst	-> patch2 (OR   sz) src dst
    XOR  sz src dst	-> patch2 (XOR  sz) src dst
    NOT  sz op 		-> patch1 (NOT  sz) op
    NEGI sz op		-> patch1 (NEGI sz) op
    SHL  sz imm dst 	-> patch1 (SHL  sz imm) dst
    SAR  sz imm dst 	-> patch1 (SAR  sz imm) dst
    SHR  sz imm dst 	-> patch1 (SHR  sz imm) dst
    TEST sz src dst	-> patch2 (TEST sz) src dst
    CMP  sz src dst	-> patch2 (CMP  sz) src dst
    PUSH sz op		-> patch1 (PUSH sz) op
    POP  sz op		-> patch1 (POP  sz) op
    SETCC cond op	-> patch1 (SETCC cond) op
    JMP op		-> patch1 JMP op
    FADD sz src		-> FADD sz (patchOp src)
    FIADD sz asrc	-> FIADD sz (lookupAddr asrc)
    FCOM sz src		-> patch1 (FCOM sz) src
    FDIV sz src 	-> FDIV sz (patchOp src)
    --FDIVP sz src 	-> FDIVP sz (patchOp src)
    FIDIV sz asrc	-> FIDIV sz (lookupAddr asrc)
    FDIVR sz src 	-> FDIVR sz (patchOp src)
    --FDIVRP sz src 	-> FDIVRP sz (patchOp src)
    FIDIVR sz asrc	-> FIDIVR sz (lookupAddr asrc)
    FICOM sz asrc	-> FICOM sz (lookupAddr asrc)
    FILD sz asrc dst	-> FILD sz (lookupAddr asrc) (env dst)
    FIST sz adst	-> FIST sz (lookupAddr adst)
    FLD	sz src 		-> patch1 (FLD sz) (patchOp src)
    FMUL sz src 	-> FMUL sz (patchOp src)
    --FMULP sz src 	-> FMULP sz (patchOp src)
    FIMUL sz asrc	-> FIMUL sz (lookupAddr asrc)
    FST sz dst		-> FST sz (patchOp dst)
    FSTP sz dst		-> FSTP sz (patchOp dst)
    FSUB sz src		-> FSUB sz (patchOp src)
    --FSUBP sz src	-> FSUBP sz (patchOp src)
    FISUB sz asrc	-> FISUB sz (lookupAddr asrc)
    FSUBR sz src 	-> FSUBR sz (patchOp src)
    --FSUBRP sz src 	-> FSUBRP sz (patchOp src)
    FISUBR sz asrc	-> FISUBR sz (lookupAddr asrc)
    FCOMP sz src	-> FCOMP sz (patchOp src)
    _			-> instr
	
  where
		patch1 insn op = insn (patchOp op)
		patch2 insn src dst = insn (patchOp src) (patchOp dst)

		patchOp (OpReg  reg) = OpReg (env reg)
		patchOp (OpImm  imm) = OpImm imm
		patchOp (OpAddr ea)  = OpAddr (lookupAddr ea)

		lookupAddr (Addr base index disp) 
			= Addr (lookupBase base) (lookupIndex index) disp
			where lookupBase Nothing        = Nothing
 	      		      lookupBase (Just r)       = Just (env r)
	      		      lookupIndex Nothing       = Nothing
	      		      lookupIndex (Just (r,i))  = Just (env r, i)
		lookupAddr (ImmAddr imm off) 
			= ImmAddr imm off

\end{code}

Sometimes, we want to be able to modify addresses at compile time.
(Okay, just for chrCode of a fetch.)

\begin{code}

#ifdef __GLASGOW_HASKELL__

{-# SPECIALIZE
    is13Bits :: Int -> Bool
  #-}
{-# SPECIALIZE
    is13Bits :: Integer -> Bool
  #-}

#endif

is13Bits :: Integral a => a -> Bool
is13Bits x = x >= -4096 && x < 4096

offset :: Addr -> Int -> Maybe Addr
offset (Addr reg index (ImmInt n)) off
  = Just (Addr reg index (ImmInt n2))
  where n2 = n + off

offset (Addr reg index (ImmInteger n)) off
  = Just (Addr reg index (ImmInt (fromInteger n2)))
  where n2 = n + toInteger off

offset (ImmAddr imm off1) off2
  = Just (ImmAddr imm off3)
  where off3 = off1 + off2

offset _ _ = Nothing

\end{code}

