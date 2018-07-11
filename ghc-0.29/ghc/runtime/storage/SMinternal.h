# line 5 "storage/SMinternal.lh"
#ifndef SMinternals_H
#define SMinternals_H
# line 12 "storage/SMinternal.lh"



#if ! (defined(MAIN_REG_MAP) || defined(NULL_REG_MAP) || defined(MARK_REG_MAP) || defined(SCAN_REG_MAP) || defined(SCAV_REG_MAP))
**** please set your REG_MAP ****
#endif

#include "rtsdefs.h"

#ifdef HAVE_SYS_VADVISE_H
#include <sys/vadvise.h>
#endif

extern P_ heap_space;
extern P_ hp_start;

void stat_init    PROTO((char *collector, char *c1, char *c2));
void stat_startGC PROTO((I_ alloc));
void stat_endGC   PROTO((I_ alloc, I_ collect, I_ live, char *comment));
void stat_exit    PROTO((I_ alloc));

extern I_ MaxResidency;     
extern I_ ResidencySamples; 

#define DO_MAX_RESIDENCY(r) 	\
    do {					\
	I_ resid = (r);				\
	ResidencySamples++;			\
	if (resid > MaxResidency) {		\
	    MaxResidency = resid;		\
	}					\
    } while (0)

StgFunPtr _Dummy_entry(STG_NO_ARGS);

#if defined(DEBUG)
#define DEBUG_SCAN(str, pos, to, topos) \
	if (RTSflags.GcFlags.trace & DEBUG_TRACE_MINOR_GC) \
	    fprintf(stderr, "%s: 0x%lx, %s 0x%lx\n", str, pos, to, topos)
#define DEBUG_STRING(str) \
	if (RTSflags.GcFlags.trace & DEBUG_TRACE_MINOR_GC) \
	    fprintf(stderr, "%s\n", str)
#else
#define DEBUG_SCAN(str, pos, to, topos)
#define DEBUG_STRING(str)
#endif

#define NEXT_SEMI_SPACE(space) ((space + 1) % 2)




#define BIG_STRING_LEN	    	 512



#if defined(GC2s)

typedef struct {
    P_ base; 	
    P_ lim; 	
} semispaceData;

extern I_ semispace;	
extern semispaceData semispaceInfo[2];

#endif 




#if defined(GC1s)

typedef struct {
    P_ base;  
    P_ lim;   
    BitWord *bits; 	
    I_ bit_words; 	
    I_ heap_words;	
} compactingData;

extern compactingData compactingInfo;

#endif 




#if defined(GCdu)

typedef struct {
	I_ mode;
	StgFloat resid_to_compact;
	StgFloat resid_from_compact;
	struct {
	    P_ base; 
	    P_ lim;  
	    I_ heap_words; 
	    char *name;
	} modeinfo[3];
    	BitWord *bits;	    
    	I_ bit_words;   
} dualmodeData;

extern dualmodeData dualmodeInfo;

#define DEFAULT_RESID_TO_COMPACT   0.25
#define DEFAULT_RESID_FROM_COMPACT 0.20

#define TWO_SPACE_BOT 0
#define TWO_SPACE_TOP 1
#define COMPACTING    2

#endif 



#if defined(GCap)

typedef struct {
 	P_ oldbase;   
	P_ oldlim;    
	P_ oldlast;   
	P_ oldthresh; 
	P_ oldmax;    

	I_ newfixed;  
	I_ newmin;    
	P_ newbase;   
	P_ newlim;    

    	BitWord *bits;    

	P_ OldCAFlist; 
	I_ OldCAFno;   
    	I_ bit_words;  

        P_ PromMutables; 

	I_ semi_space;   
	struct {
	    P_ base;  
	    P_ lim;   
	} space[2];

} appelData;







extern appelData appelInfo;

#endif 




#if defined(GCgn)

typedef struct {
	I_ alloc_words;  
			   
	I_ new_words;    
			   
	I_ old_words;    

	I_  minor_since_major;
	                    

	P_ allocbase;  
	P_ alloclim;   

	I_ curnew;	   
	struct {
	 P_ newbase;   
	 P_ newlim;    
	} newgen[2];

	P_ oldbase;    
	P_ oldend;     
	P_ oldwas;     
	P_ oldlim;     
	P_ oldthresh;  
	BitWord *bit_vect; 

	P_ OldInNew;   
	I_ OldInNewno; 
	P_ NewCAFlist; 
	I_ NewCAFno;   
	P_ OldCAFlist; 
	I_ OldCAFno;   

        P_ PromMutables; 

	I_ semi_space;  
	struct {
	    P_ base; 
	    P_ lim;  
	} space[2];
} genData;

extern genData genInfo;







#endif 




#if defined(_INFO_COPYING)

#define EVAC_CODE(infoptr)  ((StgEvacPtr) ((P_)(INFO_RTBL(infoptr)))[COPY_INFO_OFFSET])
#define SCAV_CODE(infoptr)  ((StgScavPtr) ((P_)(INFO_RTBL(infoptr)))[COPY_INFO_OFFSET+1])

void Scavenge(STG_NO_ARGS);
void  _Scavenge_Forward_Ref(STG_NO_ARGS);





#define FORWARD_ADDRESS(closure)  (*(((P_)(closure)) + FIXED_HS))

#define FORWARDREF_ITBL(infolbl,entry,localness,evac_forward) 	\
CAT_DECLARE(infolbl,INTERNAL_KIND,"FORWARD_REF","FORWARD_REF")	\
localness W_ infolbl[] = {					\
        (W_) entry					    	\
	,(W_) INFO_OTHER_TAG					\
	,(W_) MK_REP_REF(,evac_forward,)			\
	INCLUDE_PROFILING_INFO(infolbl)				\
	}

P_ _Evacuate_Old_Forward_Ref PROTO((P_));
P_ _Evacuate_New_Forward_Ref PROTO((P_));
P_ _Evacuate_OldRoot_Forward PROTO((P_));
P_ _Evacuate_Forward_Ref PROTO((P_));

MAYBE_DECLARE_RTBL(,_Evacuate_Old_Forward_Ref,)
MAYBE_DECLARE_RTBL(,_Evacuate_New_Forward_Ref,)
MAYBE_DECLARE_RTBL(,_Evacuate_OldRoot_Forward,)
MAYBE_DECLARE_RTBL(,_Evacuate_Forward_Ref,)

#define FORWARDREF_RTBL(evac_forward) \
    const W_ MK_REP_LBL(,evac_forward,)[] = { \
	INCLUDE_TYPE_INFO(INTERNAL)				\
	INCLUDE_SIZE_INFO(INFO_UNUSED,INFO_UNUSED)		\
	INCLUDE_PAR_INFO					\
	INCLUDE_COPYING_INFO(evac_forward,_Scavenge_Forward_Ref)\
	INCLUDE_COMPACTING_INFO(INFO_UNUSED,INFO_UNUSED,INFO_UNUSED,INFO_UNUSED) \
	}

EXTDATA_RO(Caf_Evac_Upd_info);
extern StgEvacFun _Evacuate_Caf_Evac_Upd;

#define CAF_EVAC_UPD_ITBL(infolbl,entry,localness)		\
CAT_DECLARE(infolbl,INTERNAL_KIND,"CAF_EVAC_UPD","CAF_EVAC_UPD") \
localness W_ infolbl[] = {					\
        (W_) entry					    	\
	,(W_) INFO_OTHER_TAG					\
	,(W_) MK_REP_REF(Caf_Evac_Upd,,)			\
	INCLUDE_PROFILING_INFO(infolbl)				\
	}

MAYBE_DECLARE_RTBL(Caf_Evac_Upd,,)

#define CAF_EVAC_UPD_RTBL() \
    const W_ MK_REP_LBL(Caf_Evac_Upd,,)[] = { \
	INCLUDE_TYPE_INFO(INTERNAL)				\
	INCLUDE_SIZE_INFO(MIN_UPD_SIZE,INFO_UNUSED)		\
	INCLUDE_PAR_INFO					\
	INCLUDE_COPYING_INFO(_Evacuate_Caf_Evac_Upd,_Scavenge_Caf) \
	INCLUDE_COMPACTING_INFO(INFO_UNUSED,INFO_UNUSED,INFO_UNUSED,INFO_UNUSED) \
    }

#define EVACUATE_CLOSURE(closure)      \
    	(EVAC_CODE(INFO_PTR(closure)))(closure)

#endif 




#if defined(_INFO_MARKING)

I_ markHeapRoots PROTO((smInfo *sm, P_ cafs1, P_ cafs2,
			P_ base, P_ lim, BitWord *bit_array));

#define PRMARK_CODE(infoptr) \
	  (((FP_)(INFO_RTBL(infoptr)))[COMPACTING_INFO_OFFSET+1])


#define PRRETURN_CODE(infoptr) \
	  (((FP_)(INFO_RTBL(infoptr)))[COMPACTING_INFO_OFFSET+3])



#define DUMMY_PRRETURN_CLOSURE(closure_name, table_name, prreturn_code, dummy_code) \
const W_ table_name[] = { 	    	    	\
	(W_) dummy_code	    	    	    	\
	,(W_) INFO_OTHER_TAG			\
	,(W_) MK_REP_REF(,prreturn_code,)	\
	INCLUDE_PROFILING_INFO(Dummy_PrReturn)	\
	};					\
W_ closure_name = (W_) table_name

EXTFUN(_Dummy_PRReturn_entry);
EXTFUN(_PRMarking_MarkNextRoot);
EXTFUN(_PRMarking_MarkNextCAF);

#ifdef CONCURRENT
EXTFUN(_PRMarking_MarkNextSpark);
#endif

#if defined(GRAN)
EXTFUN(_PRMarking_MarkNextEvent);
EXTFUN(_PRMarking_MarkNextClosureInFetchBuffer);
#endif

#ifdef PAR
EXTFUN(_PRMarking_MarkNextGA);
MAYBE_DECLARE_RTBL(,_PRMarking_MarkNextGA,)
#else

EXTFUN(_PRMarking_MarkNextAStack);
EXTFUN(_PRMarking_MarkNextBStack);

MAYBE_DECLARE_RTBL(,_PRMarking_MarkNextAStack,)
MAYBE_DECLARE_RTBL(,_PRMarking_MarkNextBStack,)

#endif 

#ifdef CONCURRENT
MAYBE_DECLARE_RTBL(,_PRMarking_MarkNextSpark,)
#endif

#if defined(GRAN)
MAYBE_DECLARE_RTBL(,_PRMarking_MarkNextEvent,)
MAYBE_DECLARE_RTBL(,_PRMarking_MarkNextClosureInFetchBuffer,)
#endif

MAYBE_DECLARE_RTBL(,_PRMarking_MarkNextRoot,)
MAYBE_DECLARE_RTBL(,_PRMarking_MarkNextCAF,)

#define DUMMY_PRRETURN_RTBL(prreturn_code,dummy_code) 	\
    const W_ MK_REP_LBL(,prreturn_code,)[] = {	    	\
	INCLUDE_TYPE_INFO(INTERNAL)		    	\
	INCLUDE_SIZE_INFO(INFO_UNUSED,INFO_UNUSED)  	\
	INCLUDE_PAR_INFO   				\
	INCLUDE_COPYING_INFO(dummy_code,dummy_code) 	\
	INCLUDE_COMPACTING_INFO(dummy_code,dummy_code,dummy_code,prreturn_code) \
    }



 

#endif 




#if defined(_INFO_COMPACTING)

#ifndef PAR
P_ Inplace_Compaction PROTO((P_ base, P_ lim,
			     P_ scanbase, P_ scablim,
			     BitWord *bit_array, I_ bit_array_words,
			     StgPtr *MallocPtrList));
#else
P_ Inplace_Compaction PROTO((P_ base, P_ lim,
			     P_ scanbase, P_ scablim,
			     BitWord *bit_array, I_ bit_array_words));
#endif


#define SCAN_LINK_CODE(infoptr) \
	  ((StgScanPtr) ((P_)(INFO_RTBL(infoptr)))[COMPACTING_INFO_OFFSET])
#define SCAN_MOVE_CODE(infoptr) \
	  ((StgScanPtr) ((P_)(INFO_RTBL(infoptr)))[COMPACTING_INFO_OFFSET+2])







#define  LINK_GLOBALADDRESS(loc)

#if defined(GCgn)

extern StgScavFun _Scavenge_OldRoot;               
extern StgEvacFun _Evacuate_OldRoot;               

extern StgFunPtr  _PRStart_OldRoot(STG_NO_ARGS);   
extern StgScanFun _ScanMove_OldRoot;               

EXTDATA_RO(OldRoot_info);

#define OLDROOT_ITBL(infolbl,ind_code,localness,entry_localness)\
    CAT_DECLARE(infolbl,INTERNAL_KIND,"OLDROOT","OLDROOT")	\
    entry_localness(ind_code);					\
    localness W_ infolbl[] = {					\
        (W_) ind_code					    	\
	,(W_) INFO_OTHER_TAG					\
	,(W_) MK_REP_REF(OldRoot,,)				\
	INCLUDE_PROFILING_INFO(infolbl)				\
	}

MAYBE_DECLARE_RTBL(OldRoot,,)

#define OLDROOT_RTBL()							\
    const W_ MK_REP_LBL(OldRoot,,)[] = {				\
	INCLUDE_TYPE_INFO(SPEC_U)					\
	INCLUDE_SIZE_INFO(2,1)		\
	INCLUDE_PAR_INFO			\
	INCLUDE_COPYING_INFO(_Evacuate_OldRoot,_Scavenge_OldRoot)	\
	SPEC_COMPACTING_INFO(_ScanLink_2_1,_PRStart_OldRoot,_ScanMove_OldRoot,_PRIn_1) \
	}

#define LINK_LOCATION_TO_CLOSURE(loc,linklim)       	    	\
          { P_ _temp = (P_) *(loc);				\
          DEBUG_LINK_LOCATION(loc, _temp, linklim);	      	\
	  if (DYNAMIC_CLOSURE(_temp) && (_temp <= linklim)) { 	\
              *((P_)(loc)) = (W_) INFO_PTR(_temp);      	\
	      INFO_PTR(_temp)  = MARK_LOCATION(loc);   	    	\
	  }}

#else 

#define LINK_LOCATION_TO_CLOSURE(loc)       	\
          { P_ _temp = (P_) *(loc);    	\
          DEBUG_LINK_LOCATION(loc, _temp);	\
	  if (DYNAMIC_CLOSURE(_temp)) {	    	\
              *((P_)(loc)) = (W_) INFO_PTR(_temp); \
	      INFO_PTR(_temp) = MARK_LOCATION(loc); 	\
	  }}

#endif 

#if defined(DEBUG)

#if defined(GCgn)
#define DEBUG_LINK_LOCATION(location, closure, linklim)	\
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MAJOR_GC) {	        		\
	if (DYNAMIC_CLOSURE(closure) && (closure <= linklim)) \
            fprintf(stderr, "  Link Loc: 0x%lx to 0x%lx\n", location, closure); \
	else if (! DYNAMIC_CLOSURE(closure))   	\
	     fprintf(stderr, "  Link Loc: 0x%lx to 0x%lx Static Closure -- Not Done\n", location, closure); \
	else                               	\
	     fprintf(stderr, "  Link Loc: 0x%lx to 0x%lx OutOfRange Closure -- Not Done\n", location, closure); \
    }
#else 
#define DEBUG_LINK_LOCATION(location, closure)	\
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MAJOR_GC) {	        		\
	if (DYNAMIC_CLOSURE(closure))		\
            fprintf(stderr, "  Link Loc: 0x%lx to 0x%lx\n", location, closure); \
	else					\
	    fprintf(stderr, "  Link Loc: 0x%lx to 0x%lx Static Closure -- Not Done\n", location, closure); \
    }
#endif 

#define DEBUG_UNLINK_LOCATION(location, closure, newlocation) 	\
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MAJOR_GC)                     			\
        fprintf(stderr, "  UnLink Loc: 0x%lx, 0x%lx -> 0x%lx\n", location, closure, newlocation)

#define DEBUG_LINK_CAF(caf) \
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MAJOR_GC)        	\
	fprintf(stderr, "Caf: 0x%lx  Closure: 0x%lx\n", caf, IND_CLOSURE_PTR(caf))

#define DEBUG_SET_MARK(closure, hp_word) \
    if (RTSflags.GcFlags.trace & DEBUG_TRACE_MARKING)			 \
        fprintf(stderr, "  Set Mark Bit: 0x%lx, word %ld, bit_word %ld, bit %d\n", closure, hp_word, hp_word / BITS_IN(BitWord), hp_word & (BITS_IN(BitWord) - 1))

#else
#if defined(GCgn)
#define DEBUG_LINK_LOCATION(location, closure, linklim)
#else
#define DEBUG_LINK_LOCATION(location, closure)
#endif
#define DEBUG_UNLINK_LOCATION(location, closure, newlocation)
#define DEBUG_LINK_CAF(caf)
#define DEBUG_SET_MARK(closure, hp_word)
#endif

#endif 

#endif 

