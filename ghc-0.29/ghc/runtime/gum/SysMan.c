# line 78 "gum/SysMan.lc"
#define NON_POSIX_SOURCE 

#include "rtsdefs.h"
#include "LLC.h"
# line 89 "gum/SysMan.lc"
GLOBAL_TASK_ID mytid, SysManTask;
rtsBool IAmMainThread;
# line 95 "gum/SysMan.lc"
static GLOBAL_TASK_ID gtids[MAX_PES];
static long PEbuffer[MAX_PES];
int nPEs = 0;
static GLOBAL_TASK_ID sysman_id, sender_id, mainThread_id;
static unsigned PEsTerminated = 0;
static rtsBool Finishing = rtsFalse;
# line 104 "gum/SysMan.lc"
#define checkerr(c)	do {if((c)<0) { pvm_perror("Sysman"); EXIT(EXIT_FAILURE); }} while(0)
# line 110 "gum/SysMan.lc"
static void
DoGlobalGC(STG_NO_ARGS)
{}





# line 121 "gum/SysMan.lc"
main(int argc, char **argv)
{
    int rbufid;
    int opcode, nbytes;
    char **pargv;
    int i, cc;
    int spawn_flag = PvmTaskDefault;
    PACKET addr;

    char *petask, *pvmExecutable;

    setbuf(stdout, NULL);
    setbuf(stderr, NULL);

    if (argc > 1) {
	if (*argv[1] == '-') {
	    spawn_flag = PvmTaskDebug;
	    argv[1] = argv[0];
	    argv++; argc--;
	}
	sysman_id = pvm_mytid();

	checkerr(sysman_id);

	



	pvmExecutable = argv[1];

	nPEs = atoi(argv[2]);

	if ((petask = getenv(PETASK)) == NULL)
	    petask = PETASK;

#if 0
	fprintf(stderr, "nPEs (%s) = %d\n", petask, nPEs);
#endif

	
	if (nPEs > MAX_PES) {
	    fprintf(stderr, "No more than %d PEs allowed (%d requested)\n", MAX_PES, nPEs);
	    EXIT(EXIT_FAILURE);
	}
        
	



	nPEs--;
	if (nPEs > 0) {
	    
	    pargv = argv + 2;
#if 0
	    fprintf(stderr, "Spawning %d PEs(%s) ...\n", nPEs, petask);
	    fprintf(stderr, "  args: ");
	    for (i = 0; pargv[i]; ++i)
		fprintf(stderr, "%s, ", pargv[i]);
	    fprintf(stderr, "\n");
#endif
	    checkerr(pvm_spawn(petask, pargv, spawn_flag, "", nPEs, gtids));
	    


	    
	    for (i = 0; i < nPEs; i++)
		PEbuffer[i+1] = (long) gtids[i];
#if 0
	    fprintf(stderr, "Spawned \n");
#endif
	}

	




	nPEs++;				
	if (cc = fork()) {
            checkerr(cc);		
#if 0
	    fprintf(stderr, "SysMan Task is [t%x]\n", sysman_id);
#endif
	    








	    checkerr(pvm_joingroup(PECTLGROUP));
#if 0
	    fprintf(stderr, "Joined PECTLGROUP \n");
#endif
	    
	    checkerr(pvm_barrier(PECTLGROUP, nPEs + 1));
#if 0
	    fprintf(stderr, "PECTLGROUP  barrier passed \n");
#endif
	    
	    pvm_initsend(PvmDataDefault);
	    pvm_bcast(PEGROUP, PP_SYSMAN_TID);

	    
	    addr = WaitForPEOp(PP_MAIN_TASK, ANY_GLOBAL_TASK);
            pvm_bufinfo(addr, &nbytes, &opcode, &mainThread_id );
	    PEbuffer[0] = mainThread_id;
#if 0
    	    fprintf(stderr,"SysMan received Main Task = %x\n",mainThread_id); 
#endif	    
  	    
  	    pvm_initsend(PvmDataDefault);
	    PutArgs(PEbuffer, nPEs);
	    pvm_bcast(PEGROUP, PP_PETIDS);
#if 0
 	    fprintf(stderr, "Sysman successfully initialized!\n");
#endif
 	    
	    while (1) {
	        if ((rbufid = pvm_recv(ANY_TASK, ANY_OPCODE)) < 0)
	    	    pvm_perror("Sysman: Receiving Message");
	        else {
	  	    pvm_bufinfo(rbufid, &nbytes, &opcode, &sender_id);
#if 0
	  	  fprintf(stderr, "HWL-DBG(SysMan; main loop): rbufid=%x, nbytes = %d, opcode = %x, sender_id = %x\n",
	  	      rbufid, nbytes, opcode, sender_id);
#endif
	  	  switch (opcode) {
		    case PP_GC_INIT:
		      
		      fprintf(stderr, "Global GC from %x Not yet implemented for GUM!\n", sender_id);
		      sync(PECTLGROUP, PP_FULL_SYSTEM);
		      broadcast(PEGROUP, PP_GC_INIT);
		      DoGlobalGC();

		      break;

		    case PP_STATS_ON:
		    case PP_STATS_OFF:
		        
		        break;

		    case PP_FINISH:
		        if (!Finishing) {
		          fprintf(stderr, "\nFinish from %x\n", sender_id);
		    	  Finishing = rtsTrue;
		  	  pvm_initsend(PvmDataDefault);
		  	  pvm_bcast(PEGROUP, PP_FINISH);
		      } else {
		  	  ++PEsTerminated;
		      }
		      if (PEsTerminated >= nPEs) {
		    	  broadcast(PEGROUP, PP_FINISH);
		  	  broadcast(MGRGROUP, PP_FINISH);
		  	  pvm_lvgroup(PECTLGROUP);
		  	  pvm_lvgroup(MGRGROUP);
		  	  pvm_exit();
		  	  EXIT(EXIT_SUCCESS);
		      }
		      break;

		  case PP_FAIL:
		      fprintf(stderr, "Fail from %x\n", sender_id);
		      if (!Finishing) {
		  	  Finishing = rtsTrue;
		  	  broadcast(PEGROUP, PP_FAIL);
		      }
		      break;

		  default:
		      {



		  	  fprintf(stderr, "Sysman: Unrecognised opcode (%x)\n",
		  	  	opcode);
		      }
		      break;
		  } 	
	      }		
	  }		
      }      		
      else {
            pvmendtask();		
					
   	    *argv[0] = '-';		
	    execv(pvmExecutable,argv);	
      }
  }			  
}			
# line 318 "gum/SysMan.lc"

void
myexit(n)
I_ n;
{
#ifdef exit
#undef exit
#endif
    exit(n);
}

