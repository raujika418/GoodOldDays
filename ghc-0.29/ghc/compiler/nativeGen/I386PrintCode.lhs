%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

\section[I386PrintCode]{The (un-)Pretty Printer for ix86 Assembly Language}

\begin{code}
#include "HsVersions.h"

module I386PrintCode (printLabeledCodes) where

import I386Code
import Stix
import AsmRegAlloc  	( MachineCode(..), MachineRegisters(..),
			  Reg(..), RegUsage, RegLiveness, FutureLive )

import CLabelInfo   	( pprCLabel, charToC, externallyVisibleCLabel )
import Maybes	    	( Maybe(..), maybeToBool )

import Util
import Outputable
import Unpretty

\end{code}

%************************************************************************
%*									*
\subsection[TheI386Pretty]{Pretty-printing the I386 Assembly Language}
%*									*
%************************************************************************

\begin{code}

printLabeledCodes :: PprStyle -> [I386Instr] -> Unpretty
printLabeledCodes sty codes = uppAboves (map (pprI386Instr sty) codes)

\end{code}

Printing the pieces...

\begin{code}

pprReg :: Size -> Reg -> Unpretty

pprReg s (FixedReg i)  = pprI386Reg s i
pprReg s (MappedReg i) = pprI386Reg s i
pprReg s other         = uppStr (show other) -- should only happen when debugging

pprI386Reg :: Size -> FAST_INT -> Unpretty
pprI386Reg B i = uppPStr
    (case i of {
        ILIT( 0) -> SLIT("%al");  ILIT( 1) -> SLIT("%bl");
	ILIT( 2) -> SLIT("%cl");  ILIT( 3) -> SLIT("%dl");
	_ -> SLIT("very naughty I386 byte register")
    })

pprI386Reg HB i = uppPStr
    (case i of {
        ILIT( 0) -> SLIT("%ah");  ILIT( 1) -> SLIT("%bh");
	ILIT( 2) -> SLIT("%ch");  ILIT( 3) -> SLIT("%dh");
	_ -> SLIT("very naughty I386 high byte register")
    })

pprI386Reg S i = uppPStr
    (case i of {
        ILIT( 0) -> SLIT("%ax");  ILIT( 1) -> SLIT("%bx");
	ILIT( 2) -> SLIT("%cx");  ILIT( 3) -> SLIT("%dx");
        ILIT( 4) -> SLIT("%si");  ILIT( 5) -> SLIT("%di");
	ILIT( 6) -> SLIT("%bp");  ILIT( 7) -> SLIT("%sp");
	_ -> SLIT("very naughty I386 word register")
    })

pprI386Reg L i = uppPStr
    (case i of {
        ILIT( 0) -> SLIT("%eax");  ILIT( 1) -> SLIT("%ebx");
	ILIT( 2) -> SLIT("%ecx");  ILIT( 3) -> SLIT("%edx");
        ILIT( 4) -> SLIT("%esi");  ILIT( 5) -> SLIT("%edi");
	ILIT( 6) -> SLIT("%ebp");  ILIT( 7) -> SLIT("%esp");
	_ -> SLIT("very naughty I386 double word register")
    })

pprI386Reg F i = uppPStr
    (case i of {
--ToDo: rm these
        ILIT( 8) -> SLIT("%st(0)");  ILIT( 9) -> SLIT("%st(1)");
	ILIT(10) -> SLIT("%st(2)");  ILIT(11) -> SLIT("%st(3)");
        ILIT(12) -> SLIT("%st(4)");  ILIT(13) -> SLIT("%st(5)");
	ILIT(14) -> SLIT("%st(6)");  ILIT(15) -> SLIT("%st(7)");
	_ -> SLIT("very naughty I386 float register")
    })

pprI386Reg D i = uppPStr
    (case i of {
--ToDo: rm these
        ILIT( 8) -> SLIT("%st(0)");  ILIT( 9) -> SLIT("%st(1)");
	ILIT(10) -> SLIT("%st(2)");  ILIT(11) -> SLIT("%st(3)");
        ILIT(12) -> SLIT("%st(4)");  ILIT(13) -> SLIT("%st(5)");
	ILIT(14) -> SLIT("%st(6)");  ILIT(15) -> SLIT("%st(7)");
	_ -> SLIT("very naughty I386 float register")
    })

pprCond :: Cond -> Unpretty -- ToDo
pprCond x = uppPStr
    (case x of {
	GEU	-> SLIT("ae");	LU    -> SLIT("b");
	EQ	-> SLIT("e");	GT    -> SLIT("g");
	GE	-> SLIT("ge");	GU    -> SLIT("a");
	LT	-> SLIT("l");	LE    -> SLIT("le");
	LEU	-> SLIT("be");	NE    -> SLIT("ne");
	NEG	-> SLIT("s");	POS   -> SLIT("ns");
	ALWAYS	-> SLIT("mp");	-- hack
        _       -> error "Spix: iI386Code: unknown conditional!"
    })

pprDollImm :: PprStyle -> Imm -> Unpretty

pprDollImm sty i     = uppBesides [ uppPStr SLIT("$"), pprImm sty i]

pprImm :: PprStyle -> Imm -> Unpretty

pprImm sty (ImmInt i)     = uppInt i
pprImm sty (ImmInteger i) = uppInteger i
pprImm sty (ImmCLbl l)    = pprCLabel sty l
pprImm sty (ImmLab l)     = l

--pprImm (PprForAsm _ False _) (ImmLab s) = s
--pprImm _                     (ImmLab s) = uppBeside (uppChar '_') s

pprImm sty (ImmLit s) = s

pprAddr :: PprStyle -> Addr -> Unpretty
pprAddr sty (ImmAddr imm off)
  =  uppBesides [pprImm sty imm,
                 if off > 0 then uppChar '+' else uppPStr SLIT(""),
                 if off == 0 then uppPStr SLIT("") else uppInt off
                ]
pprAddr sty (Addr Nothing Nothing displacement)
  =  uppBesides [pprDisp sty displacement]
pprAddr sty (Addr base index displacement)
  =  uppBesides [pprDisp sty displacement,
                 uppChar '(',
                 pprBase base,
                 pprIndex index,
                 uppChar ')'
                ]
  where
    pprBase (Just r) = uppBesides [pprReg L r,
                                   case index of 
                                     Nothing -> uppPStr SLIT("")
                                     _       -> uppChar ','
                                  ]
    pprBase _        = uppPStr SLIT("")
    pprIndex (Just (r,i)) = uppBesides [pprReg L r, uppChar ',', uppInt i]
    pprIndex _       = uppPStr SLIT("")

pprDisp sty (ImmInt 0) = uppPStr SLIT("")
--pprDisp sty (ImmInteger 0) = uppPStr SLIT("")
pprDisp sty d = pprImm sty d

pprOperand :: PprStyle -> Size -> Operand -> Unpretty
pprOperand sty s (OpReg r) = pprReg s r
pprOperand sty s (OpImm i) = pprDollImm sty i
pprOperand sty s (OpAddr ea) = pprAddr sty ea

pprSize :: Size -> Unpretty
pprSize x = uppPStr
    (case x of
	B  -> SLIT("b")
	HB -> SLIT("b")
        S  -> SLIT("w")
	L  -> SLIT("l")
	F  -> SLIT("s")
	D  -> SLIT("l")
    )

pprSizeOp :: PprStyle -> FAST_STRING -> Size -> Operand -> Unpretty
pprSizeOp sty name size op1 =
    uppBesides [
    	uppChar '\t',
	uppPStr name,
    	pprSize size,
	uppChar ' ',
	pprOperand sty size op1
    ]

pprSizeOpOp :: PprStyle -> FAST_STRING -> Size -> Operand -> Operand -> Unpretty
pprSizeOpOp sty name size op1 op2 =
    uppBesides [
    	uppChar '\t',
	uppPStr name,
    	pprSize size,
	uppChar ' ',
	pprOperand sty size op1,
	uppComma,
	pprOperand sty size op2
    ]

pprSizeOpReg :: PprStyle -> FAST_STRING -> Size -> Operand -> Reg -> Unpretty
pprSizeOpReg sty name size op1 reg =
    uppBesides [
    	uppChar '\t',
	uppPStr name,
    	pprSize size,
	uppChar ' ',
	pprOperand sty size op1,
	uppComma,
	pprReg size reg
    ]

pprSizeAddr :: PprStyle -> FAST_STRING -> Size -> Addr -> Unpretty
pprSizeAddr sty name size op =
    uppBesides [
    	uppChar '\t',
	uppPStr name,
    	pprSize size,
	uppChar ' ',
	pprAddr sty op
    ]

pprSizeAddrReg :: PprStyle -> FAST_STRING -> Size -> Addr -> Reg -> Unpretty
pprSizeAddrReg sty name size op dst =
    uppBesides [
    	uppChar '\t',
	uppPStr name,
    	pprSize size,
	uppChar ' ',
	pprAddr sty op,
	uppComma,
        pprReg size dst
    ]

pprOpOp :: PprStyle -> FAST_STRING -> Size -> Operand -> Operand -> Unpretty
pprOpOp sty name size op1 op2 =
    uppBesides [
    	uppChar '\t',
	uppPStr name,
	uppChar ' ',
	pprOperand sty size op1,
	uppComma,
	pprOperand sty size op2
    ]

pprSizeOpOpCoerce :: PprStyle -> FAST_STRING -> Size -> Size -> Operand -> Operand -> Unpretty
pprSizeOpOpCoerce sty name size1 size2 op1 op2 =
    uppBesides [ uppChar '\t', uppPStr name, uppChar ' ',
	pprOperand sty size1 op1,
	uppComma,
	pprOperand sty size2 op2
    ]

pprCondInstr :: PprStyle -> FAST_STRING -> Cond -> Unpretty -> Unpretty
pprCondInstr sty name cond arg =
    uppBesides [ uppChar '\t', uppPStr name, pprCond cond, uppChar ' ', arg]

pprI386Instr :: PprStyle -> I386Instr -> Unpretty
pprI386Instr sty (MOV size (OpReg src) (OpReg dst)) -- hack
  | src == dst
  = uppPStr SLIT("")
pprI386Instr sty (MOV size src dst) 
  = pprSizeOpOp sty SLIT("mov") size src dst
pprI386Instr sty (MOVZX size src dst) = pprSizeOpOpCoerce sty SLIT("movzx") L size src dst
pprI386Instr sty (MOVSX size src dst) = pprSizeOpOpCoerce sty SLIT("movxs") L size src dst

-- here we do some patching, since the physical registers are only set late
-- in the code generation.
pprI386Instr sty (LEA size (OpAddr (Addr src1@(Just reg1) (Just (reg2,1)) (ImmInt 0))) dst@(OpReg reg3)) 
  | reg1 == reg3
  = pprSizeOpOp sty SLIT("add") size (OpReg reg2) dst
pprI386Instr sty (LEA size (OpAddr (Addr src1@(Just reg1) (Just (reg2,1)) (ImmInt 0))) dst@(OpReg reg3)) 
  | reg2 == reg3
  = pprSizeOpOp sty SLIT("add") size (OpReg reg1) dst
pprI386Instr sty (LEA size (OpAddr (Addr src1@(Just reg1) Nothing displ)) dst@(OpReg reg3)) 
  | reg1 == reg3
  = pprI386Instr sty (ADD size (OpImm displ) dst)
pprI386Instr sty (LEA size src dst) = pprSizeOpOp sty SLIT("lea") size src dst

pprI386Instr sty (ADD size (OpImm (ImmInt (-1))) dst) 
  = pprSizeOp sty SLIT("dec") size dst
pprI386Instr sty (ADD size (OpImm (ImmInt 1)) dst) 
  = pprSizeOp sty SLIT("inc") size dst
pprI386Instr sty (ADD size src dst) 
  = pprSizeOpOp sty SLIT("add") size src dst
pprI386Instr sty (SUB size src dst) = pprSizeOpOp sty SLIT("sub") size src dst
pprI386Instr sty (IMUL size op1 op2) = pprSizeOpOp sty SLIT("imul") size op1 op2
pprI386Instr sty (IDIV size op) = pprSizeOp sty SLIT("idiv") size op

pprI386Instr sty (AND size src dst) = pprSizeOpOp sty SLIT("and") size src dst
pprI386Instr sty (OR  size src dst) = pprSizeOpOp sty SLIT("or")  size src dst
pprI386Instr sty (XOR size src dst) = pprSizeOpOp sty SLIT("xor")  size src dst
pprI386Instr sty (NOT size op) = pprSizeOp sty SLIT("not") size op
pprI386Instr sty (NEGI size op) = pprSizeOp sty SLIT("neg") size op
pprI386Instr sty (SHL size imm dst) = pprSizeOpOp sty SLIT("shl")  size imm dst
pprI386Instr sty (SAR size imm dst) = pprSizeOpOp sty SLIT("sar")  size imm dst
pprI386Instr sty (SHR size imm dst) = pprSizeOpOp sty SLIT("shr")  size imm dst

pprI386Instr sty (CMP size src dst) = pprSizeOpOp sty SLIT("cmp")  size src dst
pprI386Instr sty (TEST size src dst) = pprSizeOpOp sty SLIT("test")  size src dst
pprI386Instr sty (PUSH size op) = pprSizeOp sty SLIT("push") size op
pprI386Instr sty (POP size op) = pprSizeOp sty SLIT("pop") size op

pprI386Instr sty (NOP) = uppPStr SLIT("\tnop")
pprI386Instr sty (CLTD) = uppPStr SLIT("\tcltd")

pprI386Instr sty (SETCC cond op) = pprCondInstr sty SLIT("set") cond (pprOperand sty B op)

pprI386Instr sty (JXX cond lab) = pprCondInstr sty SLIT("j") cond (pprCLabel sty lab)

pprI386Instr sty (JMP (OpImm imm)) = uppBeside (uppPStr SLIT("\tjmp ")) (pprImm sty imm)
pprI386Instr sty (JMP op) = uppBeside (uppPStr SLIT("\tjmp *")) (pprOperand sty L op)

pprI386Instr sty (CALL imm) =
    uppBesides [ uppPStr SLIT("\tcall "), pprImm sty imm ]

pprI386Instr sty SAHF = uppPStr SLIT("\tsahf")
pprI386Instr sty FABS = uppPStr SLIT("\tfabs")

pprI386Instr sty (FADD sz src@(OpAddr _)) 
  = uppBesides [uppPStr SLIT("\tfadd"), pprSize sz, uppChar ' ', pprOperand sty sz src]
pprI386Instr sty (FADD sz src) 
  = uppPStr SLIT("\tfadd")
pprI386Instr sty FADDP 
  = uppPStr SLIT("\tfaddp")
pprI386Instr sty (FMUL sz src) 
  = uppBesides [uppPStr SLIT("\tfmul"), pprSize sz, uppChar ' ', pprOperand sty sz src]
pprI386Instr sty FMULP 
  = uppPStr SLIT("\tfmulp")
pprI386Instr sty (FIADD size op) = pprSizeAddr sty SLIT("fiadd") size op
pprI386Instr sty FCHS = uppPStr SLIT("\tfchs")
pprI386Instr sty (FCOM size op) = pprSizeOp sty SLIT("fcom") size op
pprI386Instr sty FCOS = uppPStr SLIT("\tfcos")
pprI386Instr sty (FIDIV size op) = pprSizeAddr sty SLIT("fidiv") size op
pprI386Instr sty (FDIV sz src) 
  = uppBesides [uppPStr SLIT("\tfdiv"), pprSize sz, uppChar ' ', pprOperand sty sz src]
pprI386Instr sty FDIVP
  = uppPStr SLIT("\tfdivp")
pprI386Instr sty (FDIVR sz src)
  = uppBesides [uppPStr SLIT("\tfdivr"), pprSize sz, uppChar ' ', pprOperand sty sz src]
pprI386Instr sty FDIVRP
  = uppPStr SLIT("\tfdivpr")
pprI386Instr sty (FIDIVR size op) = pprSizeAddr sty SLIT("fidivr") size op
pprI386Instr sty (FICOM size op) = pprSizeAddr sty SLIT("ficom") size op
pprI386Instr sty (FILD sz op reg) = pprSizeAddrReg sty SLIT("fild") sz op reg
pprI386Instr sty (FIST size op) = pprSizeAddr sty SLIT("fist") size op
pprI386Instr sty (FLD sz (OpImm (ImmCLbl src))) 
  = uppBesides [uppPStr SLIT("\tfld"),pprSize sz,uppChar ' ',pprCLabel sty src]
pprI386Instr sty (FLD sz src) 
  = uppBesides [uppPStr SLIT("\tfld"),pprSize sz,uppChar ' ',pprOperand sty sz src]
pprI386Instr sty FLD1 = uppPStr SLIT("\tfld1")
pprI386Instr sty FLDZ = uppPStr SLIT("\tfldz")
pprI386Instr sty (FIMUL size op) = pprSizeAddr sty SLIT("fimul") size op
pprI386Instr sty FRNDINT = uppPStr SLIT("\tfrndint")
pprI386Instr sty FSIN = uppPStr SLIT("\tfsin")
pprI386Instr sty FSQRT = uppPStr SLIT("\tfsqrt")
pprI386Instr sty (FST sz dst) 
  = uppBesides [uppPStr SLIT("\tfst"), pprSize sz, uppChar ' ', pprOperand sty sz dst]
pprI386Instr sty (FSTP sz dst) 
  = uppBesides [uppPStr SLIT("\tfstp"), pprSize sz, uppChar ' ', pprOperand sty sz dst]
pprI386Instr sty (FISUB size op) = pprSizeAddr sty SLIT("fisub") size op
pprI386Instr sty (FSUB sz src) 
  = uppBesides [uppPStr SLIT("\tfsub"), pprSize sz, uppChar ' ', pprOperand sty sz src]
pprI386Instr sty FSUBP
  = uppPStr SLIT("\tfsubp")
pprI386Instr sty (FSUBR size src)
  = pprSizeOp sty SLIT("fsubr") size src
pprI386Instr sty FSUBRP
  = uppPStr SLIT("\tfsubpr")
pprI386Instr sty (FISUBR size op) 
  = pprSizeAddr sty SLIT("fisubr") size op
pprI386Instr sty FTST = uppPStr SLIT("\tftst")
pprI386Instr sty (FCOMP sz op) 
  = uppBesides [uppPStr SLIT("\tfcomp"), pprSize sz, uppChar ' ', pprOperand sty sz op]
pprI386Instr sty FUCOMPP = uppPStr SLIT("\tfucompp")
pprI386Instr sty FXCH = uppPStr SLIT("\tfxch")
pprI386Instr sty FNSTSW = uppPStr SLIT("\tfnstsw %ax")
pprI386Instr sty FNOP = uppPStr SLIT("")

pprI386Instr sty (LABEL clab) =
    uppBesides [
	if (externallyVisibleCLabel clab) then
	    uppBesides [uppPStr SLIT(".globl "), pprLab, uppChar '\n']
	else
	    uppNil,
    	pprLab,
	uppChar ':'
    ]
    where pprLab = pprCLabel sty clab

pprI386Instr sty (COMMENT s) = uppBeside (uppPStr SLIT("# ")) (uppPStr s)

pprI386Instr sty (SEGMENT TextSegment)
    = uppPStr SLIT(".text\n\t.align 2\054\&0x90")

pprI386Instr sty (SEGMENT DataSegment)
    = uppPStr SLIT(".data\n\t.align 2")

pprI386Instr sty (ASCII False str) =
    uppBesides [
    	uppStr "\t.asciz \"",
    	uppStr str,
    	uppChar '"'
    ]

pprI386Instr sty (ASCII True str) = uppBeside (uppStr "\t.ascii \"") (asciify str 60)
    where
    	asciify :: String -> Int -> Unpretty
    	asciify [] _ = uppStr ("\\0\"")
    	asciify s n | n <= 0 = uppBeside (uppStr "\"\n\t.ascii \"") (asciify s 60)
        asciify ('\\':cs) n = uppBeside (uppStr "\\\\") (asciify cs (n-1))
        asciify ('\"':cs) n = uppBeside (uppStr "\\\"") (asciify cs (n-1))
        asciify (c:cs) n | isPrint c = uppBeside (uppChar c) (asciify cs (n-1))
    	asciify [c] _ = uppBeside (uppStr (charToC c)) (uppStr ("\\0\""))
    	asciify (c:(cs@(d:_))) n | isDigit d =
    	    	    	    	    	uppBeside (uppStr (charToC c)) (asciify cs 0)
    	    	    	    	 | otherwise =
    	    	    	    	    	uppBeside (uppStr (charToC c)) (asciify cs (n-1))

pprI386Instr sty (DATA s xs) = uppInterleave (uppChar '\n') (map pp_item xs)
    where pp_item x = case s of
	    B -> uppBeside (uppPStr SLIT("\t.byte\t")) (pprImm sty x)
	    HB-> uppBeside (uppPStr SLIT("\t.byte\t")) (pprImm sty x)
	    S -> uppBeside (uppPStr SLIT("\t.word\t")) (pprImm sty x)
	    L -> uppBeside (uppPStr SLIT("\t.long\t")) (pprImm sty x)
	    F -> uppBeside (uppPStr SLIT("\t.long\t")) (pprImm sty x)
    	    D -> uppBeside (uppPStr SLIT("\t.double\t")) (pprImm sty x)

\end{code}

