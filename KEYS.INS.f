C KEYS.INS.f, PRIMOS>INSERT, PRIMOS GROUP, 11/18/91
C KEYS.INS.f  MNEMONIC KEYS FOR FILE SYSTEM (FTN)
C Copyright (c) 1986, Prime Computer, Inc., Natick, MA 01760
C
C NOTE: THIS FILE SHOULD AGREE EXACTLY WITH KEYS.INS.PLP AND KEYS.INS.PMA
C       THAT IS, BOTH THE INTEGER DECLARATIONS AND THE PARAMETER ( DECLARATIONS
C       SHOULD BE IN THE SAME ORDER AND INCLUDE THE SAME ITEMS.
C
C
C MODIFICATIONS:
C Date     Programmer     Description of modification
C 11/18/91 Sager          Added K$ENCR, K$DSCR, K$MLEN, K$CRLF, K$ENSH,
C                         and K$ENLG for Message 512 support.
C 04/11/91 Gorton         Added K$CHPR, K$ENCP, K$ENPW, K$DSCP, K$DSPW for
C                         for CHANGE_PROJECT.
C 12/07/90 Snay           Added E$CRAE, E$DTAM, and E$RTNR
C 12/04/90 RRM            Added k$elng for long entry(point)names fnchk$.
C 09/04/90 A. Conte       Added k$getf, k$getn, k$gmyw, k$setw, k$strt, and
C                         k$stop.
C 08/30/90 Hunt           Added k$int0 and k$data for acl protected memory.
C 06/18/90 Morin          Added K$NTLK for PRWF$$ key  option mode.
C 03/12/90 Slutz          Changed k$nmnt from 32 to 128.
C 10/19/89 Tung           Added K$FACR for sgdr$$ and K$PEOF for prwf$$.
C 05/03/89 RRM            Added new key K$NULF for OPSRS$/SRSFX$ null first search.
C                         [4039718] -Rich Malloy
C 04/17/89 Huber          Added K$NMNT key for srch$$.
C 12/26/88 Slutz          Readded K$RW & K$RWX for VINIT$;
C                         K$VMRW for SRCH$$ & VINIT$.
C 09/23/88 A. Conte       Added K$GTAL for As$Lst gate.
C 08/16/88 Vergin         Added keys for BKP$SATR and BKP$OP.
C 02/23/88 Allen          Added k$qual to indicate qualified pathnames only
C                         for tnchk$().
C 12/18/87 Poh            Added k$brief and k$long to indicate brief or long
C                         prompt.  It is used in CL$MSG.
C 07/17/87 Roper          Remove k$rrsv from search rule keys (unused)
C 03/18/87 M. Sadigh      Changed K$RIAR to K$RIRL for epf$init.
C 01/12/87 A. Conte       Added K$SLS and K$PLST for As$Set.
C 07/14/86 M. Sadigh      Added a new key K$FUNR for EPF$UNREG.
C 06/16/86 Kazin          Deleted dynamic storage manager keys. [SPAR 3019441]
C 05/01/86 Tate           Added comment, (new) SNCHK$'s uses xxCHK$ keys.
C 04/02/86 M. Sadigh      Added K$RIAR for registered EPFS.
C 02/06/86 Silveira       Added K$WIRE for wired class dynamic storage.
C 01/30/86 Kao            Corrected K$REST from 2 to 1.
C 12/26/85 M. Sadigh      Added K$SYS1 for dynamic allocation of DTAR1 segments.
     +)

C 12/20/85 M. Sadigh      Added K$REIT for registered EPFs.
C 11/27/85 Moore          Changed K$NCNT to K$NCAM.
C 11/01/85 Dossett/Yang   Added K$(TEXT HMDR ORDR RFDR KEYW ANYTYPE UNKN
C                         RRSV ACAT FILE SDIR DIR) for Search Rules primitives
C                         (SR$NEXTR, SR$EXSTR, OPSR$ and OPSRS$).
C 01/03/85 JBall          Added K$CMWR, K$CMRD for IPC$CM.
C 10/22/84 Pinkoski       Added K$BKUP for srch$$, K$DTA & K$DTC for satr$$.
C                         Also put in keys for DS$ASY & DS$UNI to match w/keys.ins.plp.
C 08/06/84 RMorris        Added K$INB and K$OUTB for TTY$RS.
C 07/06/84 Sadigh         Added K$COMO to GPATH$.
C 04/16/84 Rees           Added K$NCNT, K$BKIO to SRCH$$ keys.
C                         Added K$AWWT,K$SAVE CHBK$$ keys.
C 01/16/84 JBall          Added K$MINE for IPC$GU, also added low water key
C                         (K$IPCB) for IPC$ST.
C 01/12/84 JBall          K$MMSG no longer defined for IPC.
C 08/10/83 Kazin          Added k$segn key for GPATH$, added K$NO_FRC_DEL and
C                         K$FRC_DEL for DELEPF$, added mods to EPF keys,
C                         K$SPRC for subsystem process class dynamic storage,
C                         and removed K$VMRW.
C 07/14/83 Kroczak        Changed K$DTLS (date/time last saved) to DTB (date/time backed-up).
C 04/18/83 JBall          Added keys for IPC support [osi 1566]
C 01/27/83 Kazin          Added K$SPEC to VINIT$ keys. [OSINFO 1362]
C 01/24/83 Kazin          Made keys.ins.ftn agree with keys.ins.plp. [OSI 1329]
C 01/24/83 Kazin          Added keys for dynamic storage manager. [OSI 1329]
C 12/15/82 Swartzendruber Deleted k$gate, k$rw, and k$rwx from integer*2 stmt.
C 09/10/82 Kroczak        Added K$LTYP key to SATR$$ and K$RESV to SRCH$$
C 07/28/82 Kazin          Deleted K$RW, K$RWX, and K$GATE from VINIT$ keys.
C 07/03/82 Weinberg       Added keys for DIR$CR.
C 06/17/82 Kazin          Added K$DUPL for VINIT$.
C 05/21/82 Goggin         Added k$st$s, k$st$n, k$nlop, and k$lonp for event
C ........                logging module LGINI$.PLP.
C 04/29/82 Kazin          Added keys for SW$INT.
C 01/04/82 Slutz          Added new acl keys.
C 12/03/81 Curreri        Added keys for LGINI$ (log_init in pl1).
C 11/12/81 Weinberg       Added k$grp key for idchk$.
C 11/09/81 Weinberg       Removed non-standard ERROR code definitions from
C ........                MGSET$ section; defined keys for setting read/write
C ........                locks with SATR$$.
C 10/06/81 Weinberg       Removed initial attach point keys for ATCH$$,
C ........                ACL keys for RDEN$$; added keys for DIR$RD.
C
C
C     TABSET 6 11 28 69
C
      INTEGER*2 K$READ,K$WRIT,K$POSN,K$TRNC,K$RPOS,K$PRER,K$PREA,
     X    K$POSR,K$POSA,K$PEOF,K$CONV,K$FRCW,K$NTLK,K$VMRW,
     X    K$RDWR,K$CLOS,K$DELE,K$EXST,K$VMR,K$BKIO,K$GETU,K$RESV,
     X    K$IUFD,K$ISEG,K$NMNT,K$CACC,K$NSAM,K$NDAM,K$NSGS,K$NSGD,
     X    K$NCAM,K$CURR,K$ANY,K$SPEC,K$DUPL,K$CNSC,K$R,K$RX,K$RW,K$RWX,
     X    K$DOWN,K$UP,K$UPC,K$DWNC,
     X    K$IMFD,K$ICUR,K$SETC,K$SETH,K$HOME,K$ALLD,
     X    K$CREA,K$REP,
     X    K$SPOS,K$GOND,K$GPOS,K$MSIZ,
     X    K$MVNT,K$FULL,K$FREE,K$FACR,K$RSUB,K$UPOS,
     X    K$NAME,K$INIT,
     X    K$PROT,K$DTIM,K$DMPB,K$RWLK,K$SOWN,K$SDL,
     X    K$LTYP,K$DTB,K$TRUN,K$DFLT,K$EXCL,K$UPDT,K$NONE,
     X    K$NRTN,K$SRTN,K$IRTN,K$CPLM,K$LGLM,
     X    K$UNIT,K$CURA,K$HOMA,K$INIA,K$SEGN,K$COMO,
     X    K$ACPT,K$DEFR,K$RJCT,K$ENCR,K$DSCR,K$ENSH,K$ENLG,K$CRLF,
     X    K$PROC,K$LEVL,K$PROG,K$SYST,K$FRBK,K$ANYW,
     X    K$ZERO,K$COPY,K$DBG,K$INAL,K$REIN,K$INVK,K$IVD,
     X    K$REST,K$FRDL,K$DL,K$REIT,
     X    K$UPRC,K$WLDC,K$NULL,K$NUM,K$GRP,K$QUAL,K$ELNG,K$SMAX,
     X    K$LOF,K$NLOF,K$LON,K$NLON,K$LONP,K$NLOP,K$ST$S,K$ST$N,
     X    K$ALL,K$LOCL,K$REM,K$SYS,
     X    K$ON,K$OFF,K$RDON,K$RDOF,K$RDAL,K$ALON,K$ALOF,K$RAON,K$RAOF,
     X    K$SAME,K$PWD,
     X    K$SEMT,K$INTT,K$NFIN,K$NFSM,K$RDWT,K$NMSG,K$MROM,K$MINE,
     X    K$ROOM,K$NUSR,K$NFYS,K$IPCB,K$IPCE,K$AWWT,K$SAVE,
     X    K$OUTB,K$INB,K$BKUP,K$DTA,K$DTC,K$LINS,K$LINE,K$LINU,
     X    K$LINC,K$NEXT,K$CMWR,K$CMRD,K$TEXT,K$HMDR,K$ORDR,K$RFDR,
     X    K$KEYW,K$ANYT,K$UNKN,K$ACAT,K$FILE,K$SDIR,K$DIR,
     X    K$RIRL,K$FUNR,K$UREG,K$DUCT,K$SLS,K$PLST,
     X    K$LONG,K$BRF,K$OBJ,K$INCR,K$BUFR,K$DBIT,K$FDTB,K$UDTB,
     X    K$UNAM,K$UUNT,K$GTAL,K$NULF,K$INT0,K$DATA,K$GETF,K$GETN,
     X    K$GMYW,K$SETW,K$STRT,K$CRAE,K$DTAM,K$RTNR,K$STOP,
     X    K$CHPR,K$ENCP,K$DSCP,K$ENPW,K$DSPW

C  Long (Integer*4) parameters:
      INTEGER*4 K$BGN,K$END

C
      PARAMETER (
     X
     X !***********************************************************
     X !
     X !
     X !      KEY DEFINITIONS
     X !
     X !
     X !******************** PRWF$$ *********************
     X !              ****** RWKEY  ******
     X    K$READ = 1,     ! READ
     X    K$WRIT = 2,     ! WRITE
     X    K$POSN = 3,     ! POSITION ONLY
     X    K$TRNC = 4,     ! TRUNCATE
     X    K$RPOS = 5,     ! READ CURRENT POSITION
     X    K$PEOF = 128,   ! PRE-POSITION TO EOF
     X !              ****** POSKEY ******
     X    K$PRER = 0,     ! PRE-POSITION RELATIVE
     X    K$PREA = 8,    ! PRE-POSITION ABSOLUTE
     X    K$POSR = 16,    ! POST-POSITION RELATIVE
     X    K$POSA = 24,    ! POST-POSITION ABSOLUTE
     X !              ****** MODE   ******
     X    K$CONV = 256,   ! CONVENIENT NUMBER OF WORDS
     X    K$FRCW = 16384, ! FORCED WRITE TO DISK
     X    K$NTLK = 8192, ! AVOID TAKING TRAN LOCK FOR WRITING
     X !
     X !******************** SRCH$$ *********************
     X !              ****** ACTION ******
     X ! K$READ = 1,     ! OPEN FOR READ
     X ! K$WRIT = 2,     ! OPEN FOR WRITE
     X    K$RDWR = 3,     ! OPEN FOR READING AND WRITING
     X    K$CLOS = 4,     ! CLOSE FILE UNIT
     X    K$DELE = 5,     ! DELETE FILE
     X    K$EXST = 6,     ! CHECK FILE'S EXISTENCE
     X    K$BKUP = 7,     ! OPEN FOR READ BY BACKUP UTILITY
     X    K$VMR  = 16,    ! OPEN FOR VMFA READING
     X    K$VMRW = 48,    ! OPEN FOR VMFA READING & Writing
     X    K$BKIO = 8192, ! OPEN FOR BLOCK MODE I/O
     X    K$GETU = 16384, ! SYSTEM RETURNS UNIT NUMBER
     X    K$RESV = 0,! reserved
     X !              ****** REF    ******
     X    K$IUFD = 0,     ! FILE ENTRY IS IN UFD
     X    K$ISEG = 64,   ! FILE ENTRY IS IN SEGMENT DIRECTORY
     X    K$NMNT = 128,   ! Don't cross mount points.
     X    K$CACC = 512,  ! CHANGE ACCESS
     X !              ****** NEWFIL ******
     X    K$NSAM = 0,     ! NEW SAM FILE
     X    K$NDAM = 1024,  ! NEW DAM FILE
     X    K$NSGS = 2048,  ! NEW SAM SEGMENT DIRECTORY
     X    K$NSGD = 3072,  ! NEW DAM SEGMENT DIRECTORY
     X    K$NCAM = 4096, ! NEW CONTIGUOUS FILE
     X    K$CURR = 32767,! CURRENTLY ATTACHED UFD
     X !
     X !
     X !******************** VINIT$ *********************
     X !
     X    K$ANY  = 0,       ! USE ANY SEGMENTS
     X    K$SPEC = 1,       ! USE SPECIFIED SEGMENTS
     X    K$DUPL = 16,      ! USE DUPLICATE SEGMENTS
     X    K$CNSC = 8,      ! CONSECUTIVE SEGMENTS REQUIRED
     X    K$R    = 2,       ! READ ACCESS ON SEGMENT (^= K$READ!)
     X    K$RX   = 6,       ! READ/EXECUTE ACCESS
     X    K$RW   = 3,       ! READ/WRITE ACCESS
     X    K$RWX  = 7,       ! READ/WRITE/EXECUTE ACCESS
     X !
     X !******************** GETSN$, FIND_SEG ********************
     X !
     X    K$DOWN = 0,       ! ALLOCATE DECREASING SEGMENT #'S
     X    K$UP   = 1,       ! ALLOCATE INCREASING SEGMENT #'S
     X    K$UPC  = 2,       ! ALLOCATE INCREASING CONSEC. SEGS
     X    K$DWNC = 4,       ! ALLOCATE DECREASING CONSEC. SEGS
     X !
     X !******************** ATCH$$ *********************
     X !              ****** KEY    ******
     X    K$IMFD = 0,     ! UFD IS IN MFD
     X    K$ICUR = 2,     ! UFD IS IN CURRENT UFD
     X !              ****** KEYMOD ******
     X    K$SETC = 0,     ! SET CURRENT UFD (DO NOT SET HOME)
     X    K$SETH = 1,     ! SET HOME UFD (AS WELL AS CURRENT)
     X !              ****** NAME   ******
     X    K$HOME = 0,     ! RETURN TO HOME UFD (KEY=K$IMFD)
     X !              ****** LDISK  ******
     X    K$ALLD = 0,! SEARCH ALL DISKS
     X ! K$CURR = 32767,! SEARCH MFD OF CURRENT DISK
     X !
     X ! *********************** AC$SET ***********************
     X !
     X ! K$ANY  = 0,     ! Do it regardless
     X    K$CREA = 1,     ! Create new ACL (error if already exists)
     X    K$REP  = 2,     ! Replace existing ACL (error if does not exist)
     X !
     X !******************** SGDR$$ *********************
     X !              ****** KEY    ******
     X    K$SPOS = 1,     ! POSITION TO ENTRY NUMBER IN SEGDIR
     X    K$GOND = 2,     ! POSITION TO END OF SEGDIR
     X    K$GPOS = 3,     ! RETURN CURRENT ENTRY NUMBER
     X    K$MSIZ = 4,     ! MAKE SEGDIR GIVEN NR OF ENTRIES
     X    K$MVNT = 5,     ! MOVE FILE ENTRY TO DIFFERENT POSITION
     X    K$FULL = 6,     ! POSITION TO NEXT NON-EMPTY ENTRY
     X    K$FREE = 7,     ! POSITION TO NEXT FREE ENTRY
     X    K$FACR = 11,    ! FIND NEXT FREE ENTRY AND CREATE FILE
     X !
     X !******************** RDEN$$ *********************
     X !              ****** KEY    ******
     X ! K$READ = 1,     ! READ NEXT ENTRY
     X    K$RSUB = 2,     ! READ NEXT SUB-ENTRY
     X ! K$GPOS = 3,     ! RETURN CURRENT POSITION IN UFD
     X    K$UPOS = 4,     ! POSITION IN UFD
     X    K$NAME = 5,     ! READ ENTRY SPECIFIED BY NAME
     X !
     X !************************* DIR$RD *************************
     X !
     X ! K$READ = 1,     ! Read next entry
     X    K$INIT = 2,     ! Initialize directory (read header)
     X !
     X !******************** SATR$$ *********************
     X !              ****** KEY    ******
     X    K$PROT = 1,     ! SET PROTECTION
     X    K$DTIM = 2,     ! SET DATE/TIME MODIFIED
     X    K$DMPB = 3,     ! SET DUMPED BIT
     X    K$RWLK = 4,     ! SET PER FILE READ/WRITE LOCK
     X    K$SOWN = 5,     ! SET OWNER FIELD ON FILE
     X    K$SDL  = 6,     ! SET ACL/DELETE SWITCH ON FILE
     X    K$LTYP = 7,     ! SET LOGICAL FILE TYPE
     X    K$DTB  = 8,    ! SET DATE/TIME BACKED-UP
     X    K$TRUN = 9,    ! SET TRUNCATED BY FIX_DISK BIT
     X    K$DTA  = 10,    ! SET DATE/TIME LAST ACCESSED
     X    K$DTC  = 11,    ! SET DATE/TIME LAST CREATED
     X !
     X !              ****** RWLOCK ******
     X    K$DFLT = 0,     ! Use system default value
     X    K$EXCL = 1,     ! N readers OR one writer
     X    K$UPDT = 2,     ! N readers AND one writer
     X    K$NONE = 3,     ! N readers AND N writers
     X !
     X !******************** ERRPR$ *********************
     X !              ****** KEY    ******
     X    K$NRTN = 0,     ! NEVER RETURN TO USER
     X    K$SRTN = 1,     ! RETURN AFTER START COMMAND
     X    K$IRTN = 2,     ! IMMEDIATE RETURN TO USER
     X !
     X !******************** LIMIT$ *************************
     X !              ****** KEY    ******
     X ! K$READ = 0,     ! RETURNS INFORMATION
     X ! K$WRIT = 1,     ! SETS INFORMATION
     X !              ****** SUBKEY ******
     X    K$CPLM = 256,   ! CPU TIME IN SECONDS
     X    K$LGLM = 512,  ! LOGIN TIME IN MINUTES
     X !
     X !
     X !******************** GPATH$ *******************************
     X !              ****** KEY    ******
     X    K$UNIT = 1,     ! PATHNAME OF UNIT RETURNED
     X    K$CURA = 2,     ! PATHNAME OF CURRENT ATTACH POINT
     X    K$HOMA = 3,     ! PATHNAME OF HOME ATTACH POINT
     X    K$INIA = 4,     ! Pathname of initial attach point
     X    K$SEGN = 5,     ! Pathname of segment returned
     X    K$COMO = 6,     ! Pathname of comoutput file
     X !
     X ! ************************** ds$uni ***********************
     X ! ds$uni uses keys declared for gpath$ plus:
     X !
     X    K$NEXT = -1,     ! pathname of next open unit
     X !
     X !******************** MSG$ST *******************************
     X !
     X    K$ACPT = 0,      ! ACCEPT MSGS (ALSO MGSET)
     X    K$DEFR = 1,      ! DEFER MSGS  (ALSO MGSET)
     X    K$RJCT = 2,      ! REJECT MSGS (ALSO MGSET)
     X    K$ENCR = 3,      ! ENABLE CRLF IN MESSAGES
     X    K$DSCR = 4,      ! DISABLE CRLF IN MESSAGES
     X    K$ENSH = 5,      ! ENABLE BRIEF MESSAGES 80 CHARS
     X    K$ENLG = 6,      ! ENABLE LONG MESSAGES
     X    K$CRLF = 7,      ! QUERY CRLF STATE
     X    K$MLEN = 8,      ! QUERY MAX MESSAGE LENGTH
     X !
     X !********************* FNSID$ ******************************
     X !
     X    K$ADD  = 2,      ! Add to existing list
     X    K$SRCH = 3,      ! Search for specific node
     X !
     X !********** KEYS FOR RESUME FUNCTIONALITY FOR EPFS**********
     X !******************** STR$AL, STR$FR ***********************
     X !
     X    K$PROC = 1,      ! STORAGE TYPES :  PER PROCESS
     X    K$LEVL = 2,      ! PER LEVEL
     X    K$PROG = 2,      ! PER PROGRAM
     X    K$SYST = 4,      ! PER SYSTEM
     X    K$FRBK = 5,      ! FREE A BLOCK OF STORAGE
     X    K$ANYW = -1,     ! BASE THE STORAGE BLOCK ANYWHERE
     X    K$ZERO = 0,      ! BASE THE STORAGE BLOCK AT WORD 0
     X !
     X !*************** EPF$MAP, EPF$INIT, EPF$ALLC ***************
     X !*************** EPF$RUN, EPF$INVK, EPF$DEL  ***************
     X !*************** DELEPF$                     ***************
     X !
     X    K$COPY = 1,      ! COPY EPF FILE INTO TEMP SEGS
     X    K$DBG = 2,       ! MAP DBG INFO EPF INTO MEMORY
     X    K$INAL = 1,      ! INIT ALL OF THE EPF'S LINKAGE
     X    K$REIN = 2,      ! ONLY INIT REINIT EPF LINKAGE
     X    K$INVK = 0,      ! INVOKE AND DELETE EPF FROM MEMORY
     X    K$IVD = 2,       ! INVOKE AND  DELETE EPF FROM MEMORY
     X    K$REST = 1,      ! DO NOT INVOKE EPF, JUST RESTORE
     X    K$RIRL = 3,      ! FOR REGISTERED EPFS ONLY
     X    K$REIT = 4,      ! USED FOR REGISTERED EPF ONLY
     X    K$FRDL = 1,      ! FORCE TERMINATE EPF
     X    K$DUCT = 4,      ! DECREMENT USER COUNT ON REGISTERED EPF
     X    K$UREG = 5,      ! UNREGISTER THE EPF
     X    K$FUNR = 6,      ! FORCE UNREGISTER THE EPF
     X    K$DL   = 0,      ! DO NOT FORCE TERMINATE EPF
     X !
     X !********* FNCHK$, TNCHK$, IDCHK$, PWCHK$, SNCHK$ **********
     X !
     X    K$UPRC = 1,      ! Mask string to uppercase
     X    K$WLDC = 2,      ! Allow wildcards (not PWCHK$, SNCHK$)
     X    K$NULL = 4,      ! Allow null names
     X    K$NUM  = 8,      ! Allow numeric names (FNCHK$ only)
     X    K$GRP  = 8,      ! Check group name (IDCHK$ only)
     X    K$QUAL = 16,     ! Allow qualified names only (TNCHK$ only)
     X    K$ELNG = 32,     ! Allow long entry(point)names (fnchk$ only)
     X !
     X !************************ Q$SET ****************************
     X !
     X    K$SMAX = 1,      ! SET MAX QUOTA
     X !
     X !********************** LGINI$ *****************************
     X !
     X    K$LOF = 0,       ! OS logging on
     X    K$NLOF = 1,      ! Net logging off
     X    K$LON =  2,      ! OS logging on
     X    K$NLON = 3,      ! Net logging on
     X    K$LONP = 4,      ! Turn sys logging on, use prev file
     X    K$NLOP = 5,      ! Turn net logging on, use prev file
     X    K$ST$S = 6,      ! Return system logging status
     X    K$ST$N = 7,      ! Return network logging status
     X !
     X !************************* LDISK$ **************************
     X !
     X    K$ALL  = 0,      ! RETURN ALL DISKS
     X    K$LOCL = 1,      ! LOCAL DISKS ONLY
     X    K$REM  = 2,      ! REMOTE DISKS ONLY
     X    K$SYS  = 3,      ! DISKS FROM A SPECIFIED SYSTEM
     X !
     X !************************** SW$INT *************************
     X !
     X ! K$READ = 1       ! Read present status
     X    K$ON = 2,        ! Turn on interrupt(s)
     X    K$OFF = 3,       ! Turn off interrupt(s)
     X    K$RDON = 4,      ! Read present status and
     X !                     turn on interrupt(s)
     X    K$RDOF = 5,      ! Read present status and
     X !                     turn off interrupt(s)
     X    K$RDAL = 6,      ! Read present status of all interrupts
     X    K$ALON = 7,      ! Turn on all interrupts
     X    K$ALOF = 8,      ! Turn off all interrupts
     X    K$RAON = 9,      ! Read present status and
     X !                     turn on all interrupts
     X    K$RAOF = 10,     ! Read present status and
     X !                     turn off all interrupts
     X !
     X !************************* DIR$CR **************************
     X !
     X    K$SAME = 0,      ! Create directory of parent's type
     X    K$PWD  = 1,      ! Create password directory
     X !
     X !************************* IPC *****************************
     X !
     X !*********************** ipc_ckac **************************
     X !
     X ! K$ANY  = 0,      ! Check mailbox user ID for any access
     X ! K$READ = 1,      ! Check mailbox user ID for read access
     X ! K$WRIT = 2,      ! Check mailbox user ID for write access
     X    K$SEMT = 3,      ! Check for semaphore type open
     X    K$INTT = 4,      ! Check for interrupt type open
     X !
     X !************************ ipc$cm ***************************
     X    K$CMWR = 1,      ! Change mode to write only
     X    K$CMRD = 2,      ! Change mode to read only
     X !
     X !************************ ipc$gu ***************************
     X !           ********** access key *********
     X ! K$READ = 1,      ! Get user IDs for reading
     X ! K$WRIT = 2,      ! Get user IDs for writing
     X ! K$RDWR = 3,      ! Get user IDs for reading and writing
     X    K$MINE = 4,      ! Get my mailbox user ID
     X !
     X !************************ ipc$o ****************************
     X !           *********** mode key **********
     X ! K$READ = 1,      ! Open mailbox for reading
     X ! K$WRIT = 2,      ! Open mailbox for writing
     X ! K$RDWR = 3,      ! Open mailbox for reading and writing
     X !            ********** notify key ********
     X    K$NFIN = 1,      ! Notify with interrupts for msg waiting
     X    K$NFSM = 2,      ! Notify semaphore for message waiting
     X !
     X !******************** ipc$r, ipc$ra ************************
     X ! K$READ = 1,      ! Read without waiting
     X    K$RDWT = 2,      ! Read and wait if no data
     X !
     X !************************ ipc$st ***************************
     X !
     X    K$IPCB = 2,  ! First key for IPC$ST
     X ! K$MMSG = 1,  ! Get maximum msgs per mailbox (obsolete)
     X    K$NMSG = 2,  ! Get number of msgs waiting for this user
     X    K$MROM = 3,  ! Get maximum space allowed for mailbox msgs
     X    K$ROOM = 4,  ! Get remaining space available in mailbox
     X    K$NUSR = 5,  ! Get number of users attached to mailbox
     X    K$NFYS = 6,  ! Notify IPC$SEM database lock (for debug)
     X    K$IPCE = 5,  ! Make this last for debug
     X !
     X !
     X !************************* chbk$$ **************************
     X !
     X    K$AWWT = 2,      ! Wait for completion of async writes.
     X    K$SAVE = 4,      ! Save error information
     X !
     X !***********************************************************
     X !************************* TTY$RS **************************
     X !
     X    K$OUTB = 0,     ! CLEAR OUTPUT BUFFER
     X    K$INB  = 16384,      ! CLEAR INPUT BUFFER
     X !
     X !************************ ds$asy **************************
     X !
     X    K$LINS = 1,           ! return summary of all async lines
     X    K$LINE = 2,           ! return info for a specified async line
     X    K$LINU = 3,           ! return info for all async lines in use
     X    K$LINC = 4,           ! return info for all configured async lines
     X !
     X !*********************** sr$nextr & sr$exstr ***************
     X !
     X    K$BGN  = 268369920,   ! binary value of null pointer
     X    K$END  = 268369920,   ! binary value of null pointer
     X    K$TEXT = 1,           ! the search rule is text.
     X    K$HMDR = 2,           ! the rule is [HOME_DIR]
     X    K$ORDR = 3,           ! the rule is [ORIGIN_DIR]
     X    K$RFDR = 4,           ! the rule is [REFERENCING_DIR]
     X    K$KEYW = 8,           ! the rule is a keyword.
     X    K$ANYT = -1,          ! find a rule regardless of its type.
     X !
     X !************************* opsr$ & opsrs$ *******************
     X !
     X    K$UNKN = 0,           ! unknown type
     X    K$ACAT = 4096,      ! access category
     X    K$FILE = 8192,      ! file
     X    K$SDIR = 16384,      ! segment directory
     X    K$DIR  = 0,     ! directory
     X    K$NULF = 256,        ! search for the null suffix first
     X                          ! for srsfx$, but not opsr$
     X !
     X !********************As$Set/As$Lst***************************
     X !
     X    K$SLS  = 0,           ! system login settings
     X    K$PLST = 1,           ! parameter list
     X    K$GTAL = 2,           ! get all parameters
     X !
     X !******************* Show$/Watch$ ***************************
     X !
     X    K$GETF = 0,          ! get first user I am watching
     X    K$GETN = 1,          ! get next user I am watching
     X    K$GMYW = 2,          ! get user watching me
     X    K$SETW = 3,          ! set watch access
     X    K$STRT = 4,          ! start watch session
     X    K$STOP = 5,          ! stop watch session
     X !
     X !************************ cl$msg ****************************
     X !
     X    K$LONG  = 0,          ! long prompt indicator
     X    K$BRF   = 0,     ! brief prompt indicator
     X !
     X !************************ bkp$op ****************************
     X !
     X ! K$BKUP  = 7,           ! do not update dta when close
     X    K$OBJ   = 8,         ! access and possibly open named obj
     X    K$INCR  = 16,         ! incr backup
     X    K$BUFR  = 32,         ! open for prwf$$ I/O
     X !               The following keys are used during incr bkup.
     X !               They indicate how to determine if an object
     X !               should be opened.
     X    K$DBIT  = 128,        ! check the dumped bit
     X    K$FDTB  = 256,        ! compare dtb and dtm
     X    K$UDTB  = 512,       ! compare dtm and user date
     X !
     X !************************ bkp$satr **************************
     X !
     X ! K$DMPB  = 3,              set the dumped bit
     X ! K$DTB   = 8,            set the dtb
     X    K$UNAM  = 2048,       ! set attributes on the named obj
     X    K$UUNT  = 4096,      ! set attributes on the open obj
     X !
     X !************************mm$share_apm ***********************
     X !
     X    K$INT0  = 0,          ! initialize segment to all zeros
     X    K$DATA  = 1,          ! reserved for future enhancement
     X !
     X !************************************************************
     X !******************* disk error tracking ********************
     X    K$CRAE  = 1,         !CRA mismatch detected on icop read
     X    K$DTAM  = 2,         ! Bad set dta/dtm request
     X    K$RTNR  = 3,         ! Error on rtn_rec to disk rat
     X !************************************************************
     X !******************** chprj$ and change_project *************
     X    K$CHPR  = 0,         ! Change project
     X    K$ENCP  = 1,         ! Enable CHPRJ$ and CHANGE_PROJECT
     X    K$DSCP  = 2,         ! Disable CHPRJ$ and CHANGE_PROJECT
     X    K$ENPW  = 3,         ! Enable passwords during CP
     X    K$DSPW  = 4          ! Disable passwords during CP
     X !************************************************************
     +)
