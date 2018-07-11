# line 4 "storage/SMcopying.lh"
void SetCAFInfoTables	PROTO(( P_ CAFlist ));
void EvacuateRoots	PROTO(( P_ roots[], I_ rootno ));
void EvacuateAStack	PROTO(( PP_ stackA, PP_ botA ));
void EvacuateBStack	PROTO(( P_ stackB, P_ botB, I_ *roots ));
void Scavenge (STG_NO_ARGS);

#ifdef GRAN
void EvacuateEvents(STG_NO_ARGS);
#endif
#ifdef CONCURRENT
void EvacuateSparks(STG_NO_ARGS);
#endif

#ifdef GCdu
extern void EvacuateCAFs PROTO(( P_ CAFlist ));
#else 
extern void EvacAndScavengeCAFs PROTO(( P_ CAFlist, I_ *extra_words, I_ *roots ));
#endif 
