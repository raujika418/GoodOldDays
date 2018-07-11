# line 4 "storage/SMextn.lh"
#ifndef PAR

void initExtensions PROTO((smInfo *sm));

# if defined(_INFO_COPYING)

void evacSPTable PROTO((smInfo *sm));
void reportDeadForeignObjs PROTO((StgPtr oldMPList, StgPtr new, StgPtr *newMPLust));

# endif 

# if defined(_INFO_COMPACTING)

void sweepUpDeadForeignObjs PROTO((P_ ForeignObjList,
				   P_ base,
				   BitWord *bits
				));

# endif 

void TrashMem PROTO(( P_ from, P_ to ));

# if defined(DEBUG)

void Trash_ForeignObj_Closure PROTO((P_ mptr));
void Validate_ForeignObj PROTO(( P_ ForeignObjList ));

void Trace_FOdies  PROTO((void));
void Trace_FOlives PROTO((void));
void Trace_FOforwarded PROTO(( P_ FOPtr, P_ newAddress ));

# endif 

#endif 
