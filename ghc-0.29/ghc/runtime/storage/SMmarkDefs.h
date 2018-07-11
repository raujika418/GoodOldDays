# line 12 "storage/SMmarkDefs.lh"
#if defined(GCgn)

#define SET_MARK_BIT(closure) 					\
    do {							\
      if (closure <= HeapLim) 	\
	{							\
	  long _hp_word = ((P_)closure) - HeapBase; 	    	\
	  ASSERT(!IS_STATIC(INFO_PTR(closure)));		\
	  DEBUG_SET_MARK(closure, _hp_word); 			\
	  BitArray[_hp_word / BITS_IN(BitWord)] |= 	    	\
    	    	1L << (_hp_word & (BITS_IN(BitWord) - 1));    	\
        }							\
    } while(0)

#define CLEAR_MARK_BIT(closure)					\
    do {							\
	long _hp_word = ((P_)closure) - HeapBase; 		\
	ASSERT(!IS_STATIC(INFO_PTR(closure)));			\
	BitArray[_hp_word / BITS_IN(BitWord)] &= 	    	\
    	    	~(1L << (_hp_word & (BITS_IN(BitWord) - 1)));  	\
    } while (0)

#else

#define SET_MARK_BIT(closure) 					\
    do {							\
	long _hp_word = ((P_)closure) - HeapBase; 		\
	ASSERT(!IS_STATIC(INFO_PTR(closure)));			\
	DEBUG_SET_MARK(closure, _hp_word); 			\
	BitArray[_hp_word / BITS_IN(BitWord)] |= 	    	\
    	    	1L << (_hp_word & (BITS_IN(BitWord) - 1));    	\
    } while (0)

#define CLEAR_MARK_BIT(closure)					\
    do {							\
	long _hp_word = ((P_)closure) - HeapBase; 		\
	ASSERT(!IS_STATIC(INFO_PTR(closure)));			\
	BitArray[_hp_word / BITS_IN(BitWord)] &= 	    	\
    	    	~(1L << (_hp_word & (BITS_IN(BitWord) - 1)));  	\
    } while (0)

# line 65 "storage/SMmarkDefs.lh"

#define GM_MASK(x) ((1L << (x)) - 1)

#define GET_MARKED_PTRS(dest,closure,ptrs)			\
    do {							\
	long hw = ((P_)(closure)) - HeapBase + 1;		\
	BitWord *bw = BitArray + (hw / BITS_IN(BitWord));	\
	int offset = hw & (BITS_IN(BitWord) - 1);	    	\
	int bat = BITS_IN(BitWord) - offset;			\
								\
	ASSERT(!IS_STATIC(INFO_PTR(closure)));			\
								\
	(dest) = (ptrs) <= bat ?				\
	    bw[0] >> offset & GM_MASK(ptrs) :		    	\
	    bw[0] >> offset | 	    	    	    	    	\
                (bw[1] & GM_MASK((ptrs) - bat)) << bat;	    	\
    } while (0)























#define SET_MARKED_PTRS(closure,ptrs,val)		    	\
    do {						    	\
	long hw = ((P_)(closure)) - HeapBase + 1;	    	\
	BitWord *bw = BitArray + (hw / BITS_IN(BitWord));	\
	int offset = hw & (BITS_IN(BitWord) - 1);    	    	\
	int bat = BITS_IN(BitWord) - offset;		    	\
    	BitWord bits;					    	\
    	    	    	    	    	    	    	    	\
	ASSERT( (ptrs) < BITS_IN(BitWord) );			\
	ASSERT(!IS_STATIC(INFO_PTR(closure)));			\
							    	\
        bits = bw[0] & ~(GM_MASK(ptrs) << offset); 	    	\
        bw[0] = bits | (val) << offset;    	    	    	\
	if ((ptrs) > bat) {				    	\
    	    bits = bw[1] & ~GM_MASK((ptrs) - bat);	    	\
	    bw[1] = bits | ((val) >> bat);  		    	\
	}						    	\
    } while (0)









	



#define __MIN__(a,b) (((a) < (b)) ? (a) : (b))

#define GET_GEN_MARKED_PTRS(dest,closure,ptrs)			\
	GET_MARKED_PTRS(dest,closure,__MIN__(ptrs,BITS_IN(BitWord)-1))

#define SET_GEN_MARKED_PTRS(closure,ptrs,val)		    	\
	SET_MARKED_PTRS(closure,__MIN__(ptrs,BITS_IN(BitWord)-1),val)



#define IS_MARK_BIT_SET(closure)				  		\
	((BitArray[(((P_)closure) - HeapBase) / BITS_IN(BitWord)] >>		\
	 ((((P_)closure) - HeapBase) & (BITS_IN(BitWord) - 1))) & 0x1)

#endif
# line 156 "storage/SMmarkDefs.lh"
#define	INIT_MARK_NODE(dbg,ptrs)		\
        do {					\
	  DEBUG_PRSTART(dbg, ptrs);		\
          LINK_GLOBALADDRESS(Mark);    	    	\
	  SET_MARK_BIT(Mark);			\
        } while (0)

#define	CONTINUE_MARKING_NODE(dbg,pos)				\
        do {							\
	  DEBUG_PRIN(dbg, pos);			\
        } while (0)
# line 174 "storage/SMmarkDefs.lh"
#define	JUMP_MARK						\
	JMP_(PRMARK_CODE(INFO_PTR(Mark)))

#define	JUMP_MARK_RETURN					\
	JMP_(PRRETURN_CODE(INFO_PTR(MStack)))
# line 186 "storage/SMmarkDefs.lh"
#define	INIT_MSTACK_FROM(closure_ptr,first_ptr)			\
    do { 							\
	P_ temp = (P_) closure_ptr(Mark, first_ptr);	    	\
    	closure_ptr(Mark, first_ptr) = (W_) MStack;   	    	\
 \
    	MStack = Mark;                                  	\
    	Mark = temp;                                  		\
        JUMP_MARK;						\
    } while (0)
# line 201 "storage/SMmarkDefs.lh"
#define	INIT_MSTACK(closure_ptr)				\
    INIT_MSTACK_FROM(closure_ptr,1)
# line 209 "storage/SMmarkDefs.lh"
#define	MOVE_TO_NEXT_PTR(closure_ptr,pos)			\
    do {							\
	P_ temp = (P_) closure_ptr(MStack, pos+1); 	    	\
	closure_ptr(MStack, pos+1) = closure_ptr(MStack, pos); 	\
	closure_ptr(MStack, pos) = (W_) Mark;   		\
	Mark = temp;						\
        JUMP_MARK;						\
    } while(0)
# line 223 "storage/SMmarkDefs.lh"
#define	POP_MSTACK(dbg,closure_ptr,pos)				\
    do {							\
	RESTORE_MSTACK(dbg,closure_ptr,pos);			\
        JUMP_MARK_RETURN;					\
    } while (0)

#define	RESTORE_MSTACK(dbg,closure_ptr,pos)			\
    do {							\
	P_ temp = Mark;                                  	\
        DEBUG_PRLAST(dbg, pos);	                   		\
	Mark = MStack;                                     	\
	MStack = (P_) closure_ptr(Mark, pos);		    	\
    	closure_ptr(Mark, pos) = (W_) temp;	  	    	\
    } while (0)
# line 242 "storage/SMmarkDefs.lh"
#if defined(DEBUG)

#define DEBUG_PRSTART(type, ptrsvar) \
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MARKING)                         \
        fprintf(stderr, "PRMark Start (%s): 0x%lx, info 0x%lx ptrs %ld\n", \
                type, Mark, INFO_PTR(Mark), ptrsvar)

#define DEBUG_PRIN(type, posvar) \
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MARKING)                     \
        fprintf(stderr, "PRRet  In    (%s): 0x%lx, info 0x%lx pos %ld\n", \
                type, MStack, INFO_PTR(MStack), posvar)

#define DEBUG_PRLAST(type, ptrvar) \
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MARKING)                       \
        fprintf(stderr, "PRRet  Last  (%s): 0x%lx, info 0x%lx ptrs %ld\n", \
                type, MStack, INFO_PTR(MStack), ptrvar)

#define DEBUG_PR_MARKED \
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MARKING)   \
        fprintf(stderr, "PRMark Marked      : 0x%lx, info 0x%lx\n", \
		Mark, INFO_PTR(Mark))

#define DEBUG_PR_STAT \
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MARKING) \
        fprintf(stderr, "PRMark Static      : 0x%lx, info 0x%lx\n", \
		Mark, INFO_PTR(Mark))

#define DEBUG_PR_IND  \
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MARKING) \
        fprintf(stderr, "PRMark Ind : 0x%lx -> PRMark(0x%lx), info 0x%lx\n", \
		Mark, IND_CLOSURE_PTR(Mark), INFO_PTR(Mark))

#define DEBUG_PR_CAF  \
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MARKING) \
        fprintf(stderr, "PRMark Caf : 0x%lx -> PRMark(0x%lx), info 0x%lx\n", \
		Mark, IND_CLOSURE_PTR(Mark), INFO_PTR(Mark))

#define DEBUG_PR_CONST \
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MARKING)  \
        fprintf(stderr, "PRMark Const : 0x%lx -> 0x%lx, info 0x%lx\n", \
		Mark, CONST_STATIC_CLOSURE(INFO_PTR(Mark)), INFO_PTR(Mark))

#define DEBUG_PR_CHARLIKE \
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MARKING)  \
        fprintf(stderr, "PRMark CharLike (%lx) : 0x%lx -> 0x%lx, info 0x%lx\n", \
		CHARLIKE_VALUE(Mark), Mark, CHARLIKE_CLOSURE(CHARLIKE_VALUE(Mark)), INFO_PTR(Mark))

#define	DEBUG_PR_INTLIKE_TO_STATIC \
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MARKING)  \
        fprintf(stderr, "PRMark IntLike to Static (%ld) : 0x%lx -> 0x%lx, info 0x%lx\n", \
		INTLIKE_VALUE(Mark), Mark, INTLIKE_CLOSURE(INTLIKE_VALUE(Mark)), INFO_PTR(Mark))

#define	DEBUG_PR_INTLIKE_IN_HEAP \
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MARKING)  \
        fprintf(stderr, "PRMark IntLike in Heap   (%ld) : 0x%lx, info 0x%lx\n", \
		INTLIKE_VALUE(Mark), Mark, INFO_PTR(Mark))

#define DEBUG_PR_OLDIND \
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MARKING) \
        fprintf(stderr, "PRMark OldRoot Ind : 0x%lx -> PRMark(0x%lx), info 0x%lx\n", \
		Mark, IND_CLOSURE_PTR(Mark), INFO_PTR(Mark))

#else

#define DEBUG_PRSTART(type, ptrvar)
#define DEBUG_PRIN(type, posvar)
#define DEBUG_PRLAST(type, ptrvar)
#define DEBUG_PR_MARKED
#define DEBUG_PR_STAT
#define DEBUG_PR_IND
#define DEBUG_PR_CAF
#define DEBUG_PR_CONST
#define DEBUG_PR_CHARLIKE
#define	DEBUG_PR_INTLIKE_TO_STATIC
#define	DEBUG_PR_INTLIKE_IN_HEAP
#define DEBUG_PR_OLDIND

#endif

