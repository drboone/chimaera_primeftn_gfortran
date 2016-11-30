C ERRD.INS.f, PRIMOS>INSERT, PRIMOS GROUP, 07/02/93
C MNEMONIC CODES FOR FILE SYSTEM (FTN)
C Copyright (c) 1991, Prime Computer, Inc., Natick, MA 01760
C
C     TABSET 6 11 26 59 68
C  Adding a code requires changes to: ERRD.INS.@@, ERRD.DEF.MOD, KS>ERRCOM.PMA
C
C  MODIFICATIONS:
C   Date   Programmer     Description of modification
C 07/02/93 A. Griffith    Added error code E$NOUX.
C 01/15/92 A. Griffith    Added error code E$PNIR, E$FDER, and E$FDIR for
C                          On_Line Fix_Disk Gates.
C 11/18/91 Sager          Added E$METL for 512 message support.
C 06/21/91 Gorton         Added E$PWRU and E$CPLE for CHG$PW.
C 06/13/91 RLove          Added E$ACDD for ASNDE$.
C 06/12/91 Bugos          Added forgotten declarations of E$BFMT and E$ACPG.
C 06/12/91 RLove          Added E$ACPG for ASSIGN_DISK.
C 06/11/91 Peterson       Added e$bfmt.
C 02/20/91 A. Griffith    Added E$DNTA for RAS$RA_TO_PATH.
C 12/07/90 Snay           added E$NDSM
C 10/02/90 Tung           Added error codes E$NOWR, E$ICUT and E$IANL
C                         for UT$COPY.
C 09/04/90 A. Conte       Added E$UNLI, E$MULI, E$CWAT, E$UBW
C 08/30/90 Hunt           Added E$APSM for Acl Protected Memory.
C 06/20/90 Slutz          Added e$ernf.
C 05/24/90 Slutz          Added e$nd1s, e$umle.
C 04/17/89 Huber          Added error code for the Name Service: E$RXMH,
C                         E$NNET, E$MTPT, E$RPMH, E$MNSH, E$IPTR, E$PTHU,
C                         E$IGMT, E$IROO, E$NEWF, E$BPOR, E$PRVT.
C 10/21/88 Fisher         Added e$nadm.
C 12/21/87 Roper          Added e$zero.
C 11/03/87 Van Seters     Added e$ista.
C 10/06/87 Ng             Added comment on ERRD.DEF.MOD.
C 09/15/87 Slutz          Added e$bmpc.
C 09/03/87 Y.K.Yang       Added e$dnts and e$snts.
C 07/22/87 Tsang          Added e$ok.
C 07/08/87 Dossett        Added E$GPON and E$NGPW.
C 06/02/87 Hornbaker      Added e$bchk and e$expd
C 05/06/87 A. Conte       Added e$itlb, e$ips, e$dpar, and e$pns.
C 04/02/87 Tsang          Added e$rmln.
C 02/02/87 Tsang          Added e$lna, e$ldes and e$lny.
C 01/20/87 Magnan         Add E$LNP Line Not Present
C 11/10/86 Rosenstock     Removed E$NASU.
C                         [11/03/86]
C 11/10/86 Rosenstock     Removed E$NNTS.
C                         [10/23/86]
C 11/10/86 A. Conte       added e$nba, e$lnow
C                         [09/16/86]
C 07/21/86 Simon          Added E$NPDA.
C 07/21/86 Phillips (UK)  Added E$NINT.
C 07/14/86 M. Sadigh      Added a new error code E$REIU for EPF$UNREG.
C 07/09/86 Leblang        Rev 21 integration comprised of:
C          James R. Ward  Added e$ variants of iguana error codes.
C          James R. Ward  Added e$ntsh.
C          Kiefer         Added declaration for e$nxcb, e$doqf, e$lnoc,e$rqf,
C                         e$crej, e$ctmo, e$lhdn, e$ltdn.
C          James R. Ward  (Pacer Software, Inc.) added NSS and NTS codes:
C                         e$nsni, e$nsnc, e$nsac, n$nthn, e$ntns,
C                         e$ntst, e$ntcf, e$ntlc, e$ntdl, e$ntin,
C                         e$plaa, e$llaa, e$naso, e$nasu, e$ncfg
C 07/08/86 Becker/Conte   Add messages for AS$Get/AS$Set/AS$Lin:   e$blin,
C                          e$bbuf, e$bpro, e$lnus, e$bfus, e$irbf, e$iabf,
C                          e$nnts, e$iasd, e$iasp, e$ilod.
C 07/07/86 Milne          Added E$ISMR.
C 04/06/86 Fichter        Added E$NOWN, E$BLOK, E$AREA.
C 03/11/86 JBall          Added e$ndrb and e$cqpt for Q_UPDT use.
C 02/27/86 M. Sadigh      Fixed multiple declarartion of a variable.
C 12/30/85 Sadigh/Ng      Added for EPF II: e$ireg, e$inai, e$illn, e$buid,
C                           e$inre, e$npsg, e$uinf, e$ivpt, e$snal, e$natf,
C                           e$nd3s, e$bsmt, e$ialn, e$bptr, e$idbt, e$bdtr,
C                           e$lunr, e$enrg.
C 12/27/85 Pinkoski       Added codes for Primix:  e$ilus, e$nchd,e$int,
C                         e$xshdn, e$nopx, e$nous, e$incom
C 12/17/85 Wright         Added e$nres.
C 11/27/85 Moore          Added e$imem and e$ifcb for CAM file support.
C                         E$NEST, E$ADMN, E$EOL, E$ADRL.
C 02/13/85 Hornbaker      Added e$aele.
C 02/05/85 Bloom          Added e$nrfc.
C 01/31/85 Abelli         Added e$cpov, e$ioov, e$bhov.
C 01/29/85 Chan           Added E$NOPD.
C 01/29/85 Bogardus       Added E$RSHD, and E$N231-E$N238.
C 03/05/84 Sadigh         Corrected key 156.
C 11/30/83 Slutz          Added Smith's error codes to integer statement.
C          WD Smith       Added e$indl & e$peof.
C          Slutz          Added e$fidc and e$uafu.
C 10/20/83 Kazin          Added E$ALSZ, E$FRER, E$HPER, E$EPFT, E$EPFS, E$ILTD,
C                         E$ILTE, E$ECEB, E$EPFL, E$NTA, E$SWPS, E$SWPR, and
C                         E$ADCM for EPF support.
C 09/25/83 HANTMAN        Added E$BLEF and E$BLET.
C 04/29/83 Abelli-Raizen  Added E$UDMA and E$UDMC.
C 04/18/83 JBall          Added E$NACC (Not accessable) for IPC support. [osi 1566]
C 01/21/83 HANTMAN        Added E$NSB for detection of NSB labelled tapes
C                         by MAGNET,MAGLIB and LABEL.
C 11/20/82 HChen          Added E$IDNF (Slave ID not found).
C 10/29/82 HChen          Added E$MNPX (illegal multiple hoppings in NPX).
C 09/10/82 Kroczak        Added E$RESF (Improper access to restricted file).
C 04/22/82 HChen          Added E$WSLC, E$VCGC and E$MSLV.
C 04/04/82 HChen          Added E$APND (for R$BGIN) and E$BVCC.
C 03/24/82 Weinberg       Added E$NFAS (not found in attach scan).
C 12/14/81 Huber          changed T$GPPI error codes to match rev 18. To
C                         do this moved E$RSNU from 137 to 140 and filled
C                         in the previously used codes with E$CTPR, E$DFPR,
C                         and E$DLPR.
C 11/06/81 Weinberg       changed codes for ACL rewrite.
C 10/26/81 Goggin         Added new error codes for translator group.
C 10/22/81 HChen          used a spare one, 137, for E$RSNU.
C 05/22/81 Detroy         add T$GPPI error codes.
C 04/07/81 Cecchin        merged new errors for Acls  (for Ben Crocker).
C 03/25/81 Cecchin        added NPX error codes from 18 to fix mismatch
C                         between 18 and 19. Also added spare 18 error codes
C                         as a temporary solution.
C
      INTEGER*2 E$OK,E$EOF,E$BOF,E$UNOP,E$UIUS,E$FIUS,E$BPAR,E$NATT,
     X        E$FDFL,E$DKFL,E$NRIT,E$FDEL,E$NTUD,E$NTSD,E$DIRE,
     X        E$FNTF,E$FNTS,E$BNAM,E$EXST,E$DNTE,E$SHUT,E$DISK,
     X        E$BDAM,E$PTRM,E$BPAS,E$BCOD,E$BTRN,E$OLDP,E$BKEY,
     X        E$BUNT,E$BSUN,E$SUNO,E$NMLG,E$SDER,E$BUFD,E$BFTS,
     X        E$FITB,E$NULL,E$IREM,E$DVIU,E$RLDN,E$FUIU,E$DNS,
     X        E$TMUL,E$FBST,E$BSGN,E$FIFC,E$TMRU,E$NASS,E$BFSV,
     X        E$SEMO,E$NTIM,E$FABT,E$FONC,E$NPHA,E$ROOM,E$WTPR,
     X        E$ITRE,E$FAMU,E$TMUS,E$NCOM,E$NFLT,E$STKF,E$STKS,
     X        E$NOON,E$CRWL,E$CROV,E$CRUN,E$CMND,E$RCHR,E$NEXP,
     X        E$BARG,E$CSOV,E$NOSG,E$TRCL,E$NDMC,E$DNAV,E$DATT,
     X        E$BDAT,E$BLEN,E$BDEV,E$QLEX,E$NBUF,E$INWT,E$NINP,
     X        E$DFD,E$DNC,E$SICM,E$SBCF,E$VKBL,E$VIA,E$VICA,
     X        E$VIF,E$VFR,E$VFP,E$VPFC,E$VNFC,E$VPEF,E$VIRC,
     X        E$IVCM,E$DNCT,E$BNWD,E$SGIU,E$NESG,E$SDUP,E$IVWN,
     X        E$WAIN,E$NMVS,E$NMTS,E$NDAM,E$NOVA,E$NECS,E$NRCV,
     X        E$UNRV,E$UBSY,E$UDEF,E$UADR,E$PRTL,E$NSUC,E$NROB,
     X        E$NETE,E$SHDN,E$UNOD,E$NDAT,E$ENQD,E$PHNA,E$IWST,
     X        E$BKFP,E$BPRH,E$ABTI,E$ILFF,E$TMED,E$DANC,E$NENB,
     X        E$NSLA,E$PNTF,E$SVAL,E$IEDI,E$WMST,E$DNSK,E$RSNU,
     X        E$S18E,E$NFQB,E$MXQB,E$NOQD,E$QEXC,E$IMFD,E$NACL,
     X        E$PNAC,E$NTFD,E$IACL,E$NCAT,E$LRNA,E$CPMF,E$ACBG,
     X        E$ACNF,E$LRNF,E$BACL,E$BVER,E$NINF,E$CATF,E$ADRF,
     X        E$NVAL,E$LOGO,E$NUTP,E$UTAR,E$UNIU,E$NFUT,E$UAHU,
     X        E$PANF,E$MISA,E$SCCM,E$BRPA,E$DTNS,E$SPND,E$BCFG,
     X        E$BMOD,E$BID,E$ST19,E$CTPR,E$DFPR,E$DLPR,E$BLUE,
     X        E$NDFD,E$WFT,E$FDMM,E$FER,E$BDV,E$BFOV,E$NFAS,
     X        E$APND,E$BVCC,E$RESF,E$MNPX,E$SYNT,E$USTR,E$WNS,
     X        E$IREQ,E$VNG,E$SOR,E$TMVV,E$ESV,E$VABS,E$BCLC,
     X        E$NSB,E$WSLV,E$VCGC,E$MSLV,E$IDNF,E$NACC,E$UDMA,
     X        E$UDMC,E$BLEF,E$BLET,E$ALSZ,E$FRER,E$HPER,E$EPFT,
     X        E$EPFS,E$ILTD,E$ILTE,E$ECEB,E$EPFL,E$NTA,E$SWPS,
     X        E$SWPR,E$ADCM,E$UAFU,E$FIDC,E$INDL,E$PEOF,
     X        E$EXMF,E$BKIO,E$AWER,E$RAMC,E$RIER,E$NSLV,E$RSIN,
     X        E$ATNS,E$RSHD,E$NOPD,E$NRFC,E$CPOV,E$IOOV,E$BHOV,
     X        E$ADRL,E$IFCB,E$IMEM,E$NRES,E$ILUS,E$NCHD,E$INT,
     X        E$XSHD,E$NOPX,E$NOUS,E$INCO,
     X        E$ND3S,E$BSMT,E$IALN,E$BPTR,E$IDBT,E$BDTR,E$LUNR,
     X        E$ENRG,E$IREG,E$INAI,E$ILLN,E$BUID,E$INRE,E$NPSG,
     X        E$UINF,E$IVPT,E$SNAL,E$NATF,E$NDRB,E$CQPT,E$AREA,
     X        E$NOWN,E$BLOK,E$ISMR,E$BLIN,E$BBUF,E$BPRO,E$LNUS,
     X        E$BFUS,E$IRBF,E$IABF,E$IASD,E$IASP,E$ILOD,
     X        E$NSNI,E$NSNC,E$NSAC,E$NTHN,E$NTNS,E$NTST,
     X        E$NTCF,E$NTLC,E$NTDL,E$NTIN,E$PLAA,E$LLAA,E$NASO,
     X        E$NXCB,E$DOQF,E$LNOC,E$RQF, E$CREJ,E$CTMO,
     X        E$LHDN,E$LTDN,E$NCFG,E$NTSH,E$QFUL,E$QEMP,E$NOQ,E$VAL,
     X        E$COMM,E$AWIR,E$IWIR,E$NPDA,E$NINT,E$REIU,E$NBA,
     X        E$LNOW,E$LNP,E$LNA,E$LDES,E$LNY,E$RMLN, E$ITLB,E$IPS,
     X        E$DPAR,E$PNS,
     X        E$BCHK,E$EXPD,E$DNTS,E$SNTS,E$BMPC,
     X        E$GPON,E$NGPW,E$ISTA,E$ZERO,E$NADM,
     X        E$RXMH,E$NNET,E$MTPT,E$RPMH,E$MNSH,
     X        E$IPTR,E$PTHU,E$IGMT,E$IROO,E$NEWF,E$BPOR,E$PRVT,
     X        E$ND1S,E$UMLE,E$ERNF,
     X        E$APSM,E$UNLI,E$MULI,E$CWAT,E$UBW,E$NOWR,E$ICUT,
     X        E$IANL,E$NDSM,E$DNTA,E$BFMT,E$ACPG,E$ACDD,
     X        E$PWRU,E$CPLE,E$METL,E$PNIR,E$FDER,E$FDIR,E$LAST
C
      PARAMETER (
     X
     X !*******************************************************
     X !
     X !
     X !      CODE DEFINITIONS
     X !
     X !
     X    E$OK  = 00000, ! NORMAL RETURN
     X    E$EOF = 00001, ! END OF FILE                   PE
     X    E$BOF = 00002, ! BEGINNING OF FILE             PG
     X    E$UNOP= 00003, ! UNIT NOT OPEN                 PD,SD
     X    E$UIUS= 00004, ! UNIT IN USE                   SI
     X    E$FIUS= 00005, ! FILE IN USE                   SI
     X    E$BPAR= 00006, ! BAD PARAMETER                 SA
     X    E$NATT= 00007, ! NO UFD ATTACHED               SL,AL
     X    E$FDFL= 00008, ! UFD FULL                      SK
     X    E$DKFL= 00009, ! DISK FULL                     DJ
     X    E$NRIT= 00010, ! NO RIGHT                      SX
     X    E$FDEL= 00011, ! FILE OPEN ON DELETE           SD
     X    E$NTUD= 00012, ! NOT A UFD                     AR
     X    E$NTSD= 00013, ! NOT A SEGDIR                  --
     X    E$DIRE= 00014, ! IS A DIRECTORY                --
     X    E$FNTF= 00015, ! (FILE) NOT FOUND              SH,AH
     X    E$FNTS= 00016, ! (FILE) NOT FOUND IN SEGDIR    SQ
     X    E$BNAM= 00017, ! ILLEGAL NAME                  CA
     X    E$EXST= 00018, ! ALREADY EXISTS                CZ
     X    E$DNTE= 00019, ! DIRECTORY NOT EMPTY           --
     X    E$SHUT= 00020, ! BAD SHUTDN (FAM ONLY)         BS
     X    E$DISK= 00021, ! DISK I/O ERROR                WB
     X    E$BDAM= 00022, ! BAD DAM FILE (FAM ONLY)       SS
     X    E$PTRM= 00023, ! PTR MISMATCH (FAM ONLY)       PC,DC,AC
     X    E$BPAS= 00024, ! BAD PASSWORD (FAM ONLY)       AN
     X    E$BCOD= 00025, ! BAD CODE IN ERRVEC            --
     X    E$BTRN= 00026, ! BAD TRUNCATE OF SEGDIR        --
     X    E$OLDP= 00027, ! OLD PARTITION                 --
     X    E$BKEY= 00028, ! BAD KEY                       --
     X    E$BUNT= 00029, ! BAD UNIT NUMBER               --
     X    E$BSUN= 00030, ! BAD SEGDIR UNIT               SA
     X    E$SUNO= 00031, ! SEGDIR UNIT NOT OPEN          --
     X    E$NMLG= 00032, ! NAME TOO LONG                 --
     X    E$SDER= 00033, ! SEGDIR ERROR                  SQ
     X    E$BUFD= 00034, ! BAD UFD                       --
     X    E$BFTS= 00035, ! BUFFER TOO SMALL              --
     X    E$FITB= 00036, ! FILE TOO BIG                  --
     X    E$NULL= 00037, ! (NULL MESSAGE)                --
     X    E$IREM= 00038, ! ILL REMOTE REF                --
     X    E$DVIU= 00039, ! DEVICE IN USE                 --
     X    E$RLDN= 00040, ! REMOTE LINE DOWN              --
     X    E$FUIU= 00041, ! ALL UNITS IN USE              --
     X    E$DNS = 00042, ! DEVICE NOT STARTED            --
     X    E$TMUL= 00043, ! TOO MANY UFD LEVELS           --
     X    E$FBST= 00044, ! FAM - BAD STARTUP             --
     X    E$BSGN= 00045, ! BAD SEGMENT NUMBER            --
     X    E$FIFC= 00046, ! INVALID FAM FUNCTION CODE     --
     X    E$TMRU= 00047, ! MAX REMOTE USERS EXCEEDED     --
     X    E$NASS= 00048, ! DEVICE NOT ASSIGNED           --
     X    E$BFSV= 00049, ! BAD FAM SVC                   --
     X    E$SEMO= 00050, ! SEM OVERFLOW                  --
     X    E$NTIM= 00051, ! NO TIMER                      --
     X    E$FABT= 00052, ! FAM ABORT                     --
     X    E$FONC= 00053, ! FAM OP NOT COMPLETE           --
     X    E$NPHA= 00054, ! NO PHANTOMS AVAILABLE         -
     X    E$ROOM= 00055, ! NO ROOM                       --
     X    E$WTPR= 00056, ! DISK WRITE-PROTECTED          JF
     X    E$ITRE= 00057, ! ILLEGAL TREENAME              FE
     X    E$FAMU= 00058, ! FAM IN USE                    --
     X    E$TMUS= 00059, ! MAX USERS EXCEEDED            --
     X    E$NCOM= 00060, ! NULL_COMLINE                  --
     X    E$NFLT= 00061, ! NO_FAULT_FR                   --
     X    E$STKF= 00062, ! BAD STACK FORMAT              --
     X    E$STKS= 00063, ! BAD STACK ON SIGNAL           --
     X    E$NOON= 00064, ! NO ON UNIT FOR CONDITION      --
     X    E$CRWL= 00065, ! BAD CRAWLOUT                  --
     X    E$CROV= 00066, ! STACK OVFLO DURING CRAWLOUT   --
     X    E$CRUN= 00067, ! CRAWLOUT UNWIND FAIL          --
     X    E$CMND= 00068, ! BAD COMMAND FORMAT            --
     X    E$RCHR= 00069, ! RESERVED CHARACTER            --
     X    E$NEXP= 00070, ! CANNOT EXIT TO COMMAND PROC   --
     X    E$BARG= 00071, ! BAD COMMAND ARG               --
     X    E$CSOV= 00072, ! CONC STACK OVERFLOW           --
     X    E$NOSG= 00073, ! SEGMENT DOES NOT EXIST        --
     X    E$TRCL= 00074, ! TRUNCATED COMMAND LINE        --
     X    E$NDMC= 00075, ! NO SMLC DMC CHANNELS          --
     X    E$DNAV= 00076, ! DEVICE NOT AVAILABLE         DPTX
     X    E$DATT= 00077, ! DEVICE NOT ATTACHED           --
     X    E$BDAT= 00078, ! BAD DATA                      --
     X    E$BLEN= 00079, ! BAD LENGTH                    --
     X    E$BDEV= 00080, ! BAD DEVICE NUMBER             --
     X    E$QLEX= 00081, ! QUEUE LENGTH EXCEEDED         --
     X    E$NBUF= 00082, ! NO BUFFER SPACE               --
     X    E$INWT= 00083, ! INPUT WAITING                 --
     X    E$NINP= 00084, ! NO INPUT AVAILABLE            --
     X    E$DFD = 00085, ! DEVICE FORCIBLY DETACHED      --
     X    E$DNC = 00086, ! DPTX NOT CONFIGURED           --
     X    E$SICM= 00087, ! ILLEGAL 3270 COMMAND          --
     X    E$SBCF= 00088, ! BAD 'FROM' DEVICE             --
     X    E$VKBL= 00089, ! KBD LOCKED                    --
     X    E$VIA = 00090, ! INVALID AID BYTE              --
     X    E$VICA= 00091, ! INVALID CURSOR ADDRESS        --
     X    E$VIF = 00092, ! INVALID FIELD                 --
     X    E$VFR = 00093, ! FIELD REQUIRED                --
     X    E$VFP = 00094, ! FIELD PROHIBITED              --
     X    E$VPFC= 00095, ! PROTECTED FIELD CHECK         --
     X    E$VNFC= 00096, ! NUMERIC FIELD CHECK           --
     X    E$VPEF= 00097, ! PAST END OF FIELD             --
     X    E$VIRC= 00098, ! INVALID READ MOD CHAR         --
     X    E$IVCM= 00099, ! INVALID COMMAND               --
     X    E$DNCT= 00100, ! DEVICE NOT CONNECTED          --
     X    E$BNWD= 00101, ! BAD NO. OF WORDS              --
     X    E$SGIU= 00102, ! SEGMENT IN USE                --
     X    E$NESG= 00103, ! NOT ENOUGH SEGMENTS (VINIT$)  --
     X    E$SDUP= 00104, ! DUPLICATE SEGMENTS (VINIT$)   --
     X    E$IVWN= 00105, ! INVALID WINDOW NUMBER         --
     X    E$WAIN= 00106, ! WINDOW ALREADY INITIATED      --
     X    E$NMVS= 00107, ! NO MORE VMFA SEGMENTS         --
     X    E$NMTS= 00108, ! NO MORE TEMP SEGMENTS         --
     X    E$NDAM= 00109, ! NOT A DAM FILE                --
     X    E$NOVA= 00110, ! NOT OPEN FOR VMFA             --
     X    E$NECS= 00111, ! NOT ENOUGH CONTIGUOUS SEGMENTS
     X    E$NRCV= 00112, ! REQUIRES RECEIVE ENABLED      --
     X    E$UNRV= 00113, ! USER NOT RECEIVING NOW        --
     X    E$UBSY= 00114, ! USER BUSY, PLEASE WAIT        --
     X    E$UDEF= 00115, ! USER UNABLE TO RECEIVE MESSAGES
     X    E$UADR= 00116, ! UNKNOWN ADDRESSEE             --
     X    E$PRTL= 00117, ! OPERATION PARTIALLY BLOCKED   --
     X    E$NSUC= 00118, ! OPERATION UNSUCCESSFUL        --
     X    E$NROB= 00119, ! NO ROOM IN OUTPUT BUFFER      --
     X    E$NETE= 00120, ! NETWORK ERROR ENCOUNTERED     --
     X    E$SHDN= 00121, ! DISK HAS BEEN SHUT DOWN       FS
     X    E$UNOD= 00122, ! UNKNOWN NODE NAME (PRIMENET)
     X    E$NDAT= 00123, ! NO DATA FOUND                 --
     X    E$ENQD= 00124, ! ENQUED ONLY                   --
     X    E$PHNA= 00125, ! PROTOCOL HANDLER NOT AVAIL   DPTX
     X    E$IWST= 00126, ! E$INWT ENABLED BY CONFIG     DPTX
     X    E$BKFP= 00127, ! BAD KEY FOR THIS PROTOCOL    DPTX
     X    E$BPRH= 00128, ! BAD PROTOCOL HANDLER (TAT)   DPTX
     X    E$ABTI= 00129, ! I/O ABORT IN PROGRESS        DPTX
     X    E$ILFF= 00130, ! ILLEGAL DPTX FILE FORMAT     DPTX
     X    E$TMED= 00131, ! TOO MANY EMULATE DEVICES     DPTX
     X    E$DANC= 00132, ! DPTX ALREADY CONFIGURED      DPTX
     X    E$NENB= 00133, ! REMOTE MODE NOT AVAILABLE     NPX
     X    E$NSLA= 00134, ! NO NPX SLAVES AVAILABLE       ---
     X    E$PNTF= 00135, ! PROCEDURE NOT FOUND          R$CALL
     X    E$SVAL= 00136, ! SLAVE VALIDATION ERROR       R$CALL
     X    E$IEDI= 00137, ! I/O error or device interrupt (GPPI)
     X    E$WMST= 00138, ! Warm start happened (GPPI)
     X    E$DNSK= 00139, ! A pio instruction didn't skip (GPPI)
     X    E$RSNU= 00140, ! REMOTE SYSTEM NOT UP         R$CALL
     X    E$S18E= 00141, ! SPARE CODES FOR REV18
C
C    New error codes added for REV 19 begin here:
C
     X    E$NFQB= 00142, ! NO FREE QUOTA BLOCKS          --
     X    E$MXQB= 00143, ! MAXIMUM QUOTA EXCEEDED        --
     X    E$NOQD= 00144, ! NOT A QUOTA DISK (RUN VFIXRAT)
     X    E$QEXC= 00145, ! SETTING QUOTA BELOW EXISTING USAGE
     X    E$IMFD= 00146, ! QUOTA NOT PERMITTED ON MFD
     X    E$NACL= 00147, ! NOT AN ACL UFD.
     X    E$PNAC= 00148, ! PARENT NOT ACL UFD
     X    E$NTFD= 00149, ! Not a file or directory
     X    E$IACL= 00150, ! ENTRY IS AN ACL
     X    E$NCAT= 00151, ! Not an access category
     X    E$LRNA= 00152, ! Like reference not available
     X    E$CPMF= 00153, ! Category protects MFD
     X    E$ACBG= 00154, ! ACL too big
     X    E$ACNF= 00155, ! Access category not found
     X    E$LRNF= 00156, ! Like reference not found
     X    E$BACL= 00157, ! BAD ACL
     X    E$BVER= 00158, ! BAD VERSION
     X    E$NINF= 00159, ! NO INFORMATION
     X    E$CATF= 00160, ! Access category found (Ac$rvt)
     X    E$ADRF= 00161, ! ACL directory found (Ac$rvt)
     X    E$NVAL= 00162, ! Validation error (login)
     X    E$LOGO= 00163, ! Logout (code for fatal$)      --
     X    E$NUTP= 00164, ! No unit table availabe.(PHANT$)
     X    E$UTAR= 00165, ! Unit table already returned.(UTDALC)
     X    E$UNIU= 00166, ! Unit table not in use.(RTUTBL)
     X    E$NFUT= 00167, ! No free unit table.(GTUTBL)
     X    E$UAHU= 00168, ! User already has unit table.(UTALOC)
     X    E$PANF= 00169, ! Priority ACL not found.
     X    E$MISA= 00170, ! Missing argument to command.
     X    E$SCCM= 00171, ! System console command only.
     X    E$BRPA= 00172, ! Bad Remote Password          R$CALL
     X    E$DTNS= 00173, ! Date and time not set yet.
     X    E$SPND= 00174, ! REMOTE PROCEDURE CALL STILL PENDING
     X    E$BCFG= 00175, ! NETWORK CONFIGURATION MISMATCH
     X    E$BMOD= 00176, ! Illegal access mode (AC$SET)
     X    E$BID=  00177, ! Illegal identifer   (AC$SET)
     X    E$ST19= 00178, ! Operation illegal on pre-19 disk
     X    E$CTPR= 00179, ! Object is category-protected (Ac$chg)
     X    E$DFPR= 00180, ! Object is default-protected (Ac$chg)
     X    E$DLPR= 00181, ! File is delete-protected (Fil$dl)
     X    E$BLUE= 00182, ! Bad LUTBL entry.   F$IO
     X    E$NDFD= 00183, ! No driver for device.  F$IO
     X    E$WFT = 00184, ! Wrong file type  F$IO
     X    E$FDMM= 00185, ! Format/data mismatch.  F$IO
     X    E$FER = 00186, ! Bad format.  F$IO
     X    E$BDV = 00187, ! Bad dope vector.  F$IO
     X    E$BFOV= 00188, ! F$IO BF overflow.  F$IO
     X    E$NFAS= 00189, ! Top-level dir not found or inaccessible
     X    E$APND= 00190, ! Asynchronous procedure still pending
     X    E$BVCC= 00191, ! Bad virtual circuit clearing
     X    E$RESF= 00192, ! Improper access to restricted file
     X    E$MNPX= 00193, ! Illegal multiple hoppings in NPX
     X    E$SYNT= 00194, ! SYNTanx error
     X    E$USTR= 00195, ! Unterminated STRing
     X    E$WNS = 00196, ! Wrong Number of Subscripts
     X    E$IREQ= 00197, ! Integer REQuired
     X    E$VNG = 00198, ! Variable Not in namelist Group
     X    E$SOR = 00199, ! Subscript Out of Range
     X    E$TMVV= 00200, ! Too Many Values for Variable
     X    E$ESV = 00201, ! Expected String Value
     X    E$VABS= 00202, ! Variable Array Bounds or Size
     X    E$BCLC= 00203, ! Bad Compiler Library Call
     X    E$NSB = 00204, ! NSB labelled tape was detected
     X    E$WSLV= 00205, ! Slave's ID mismatch.
     X    E$VCGC= 00206, ! Virtual circuit was claered by NETMAN
     X    E$MSLV= 00207, ! Exceeds the Max number of slaves/user
     X    E$IDNF= 00208, ! Slave's ID not found
     X    E$NACC= 00209, ! Not accessible
     X    E$UDMA= 00210, ! Not Enough DMA channels
     X    E$UDMC= 00211, ! Not Enough DMC channels
     X    E$BLEF= 00212, ! Bad tape record length and EOF
     X    E$BLET= 00213, ! Bad tape record length and EOT
     X    E$ALSZ= 00214, ! Allocate request too small
     X    E$FRER= 00215, ! Free request with invalid pointer
     X    E$HPER= 00216, ! User storage heap is corrupted
     X    E$EPFT= 00217, ! Invalid EPF type
     X    E$EPFS= 00218, ! Invalid EPF search type
     X    E$ILTD= 00219, ! Invalid EPF LTD linkage descriptor
     X    E$ILTE= 00220, ! Invlaid EPF LTE linkage discriptor
     X    E$ECEB= 00221, ! Exceeding command environment breadth
     X    E$EPFL= 00222, ! EPF file exceeds file size limit
     X    E$NTA = 00223, ! EPF file not active for this user
     X    E$SWPS= 00224, ! EPF file suspended within program session
     X    E$SWPR= 00225, ! EPF file suspended within this process
     X    E$ADCM= 00226, ! System administrator command ONLY
     X    E$UAFU= 00227, ! Unable to allocate file unit
     X    E$FIDC= 00228, ! File inconsistent data count
     X    E$INDL= 00229, ! Insufficient Dam (index) level(s)
     X    E$PEOF= 00230, ! Past End Of File
     X    E$EXMF= 00231, ! Extent map full
     X    E$BKIO= 00232, ! Unit open for block mode i/o
     X    E$AWER= 00233, ! Asynchronous disk write error
     X    E$RAMC= 00234, ! R0AM access mode conflict
     X    E$RIER= 00235, ! R0AM internal error
     X    E$NSLV= 00236, ! Process not a slave.
     X    E$RSIN= 00237, ! Remote system has initialized.
     X    E$ATNS= 00238, ! Attribute not supported.
     X    E$RSHD= 00239, ! Remote disk has been shut down.
     X    E$NOPD= 00240, ! No paging device defined.
     X    E$NRFC= 00241, ! Specified reverse flow control on AMLC
     X    E$CPOV= 00242, ! PX$SVTIM overflow of CPU seconds
     X    E$IOOV= 00243, ! PX$SVTIM overflow of I/O seconds
     X    E$BHOV= 00244, ! PX$SVTIM overflow both CPU & I/O sec.s
     X    E$AELE= 00245, ! Attempt to execute non-executable library
     X    E$RULE= 00247, ! Search rule not found or invalid.
     X    E$NTOP= 00248, ! Search rule was not an optional rule.
     X    E$NEST= 00249, ! Template files nested too deeply.
     X    E$ADMN= 00250, ! Illegal attempt to change admin rules.
     X    E$EOL = 00251, ! End of list reached.
     X    E$ADRL= 00252, ! Error in administrator rules.
     X    E$IFCB= 00253, ! Insufficient free contiguous blocks.
     X    E$IMEM= 00254, ! Insufficient memory for extent map.
     X    E$NRES= 00255, ! No resources available for request.
     X    E$ILUS= 00256, ! Illegal use of Primix gate
     X    E$NCHD= 00257, ! No child found for this process.
     X    E$INT = 00258, ! Wait terminated by interrupt
     X    E$XSHD= 00259, ! Can NOT initialize Primix when running
     X    E$NOPX= 00260, ! Can NOT shutdown Primix when NOT running
     X    E$NOUS= 00261, ! Primix process table has no users when it should have users.
     X    E$INCO= 00262, ! Primix process table returned is incomplete
     X    E$IREG= 00263, ! Illegal EPF Registration.
     X    E$INAI= 00264, ! Invalid number of args. in initialization
     X    E$ILLN= 00265, ! Illegal link at registration
     X    E$BUID= 00266, ! Bad user id
     X    E$INRE= 00267, ! Invalid request
     X    E$NPSG= 00268, ! Not enough per-user DATR1 segments
     X    E$UINF= 00269, ! User Id not found
     X    E$IVPT= 00270, ! An invalid block pointer was given
     X    E$SNAL= 00271, ! Segment not allocated
     X    E$NATF= 00272, ! Not able to free storage
     X    E$ND3S= 00273, ! No Dtar 3 Segments available
     X    E$BSMT= 00274, ! Null smt_ptr or bad field within SMT
     X    E$IALN= 00275, ! Illegal alias name
     X    E$BPTR= 00276, ! Bad pointer (within SMT?)
     X    E$IDBT= 00277, ! Illegal database
     X    E$BDTR= 00278, ! Bad DTAR
     X    E$LUNR= 00279, ! Library unregistered
     X    E$ENRG= 00280, ! EPF has not been registered
     X    E$NDRB= 00281, ! No directory block for unit.
     X    E$CQPT= 00282, ! Circular Quota parent thread.
     X    E$AREA= 00283, ! Corrupted area encountered.
     X    E$NOWN= 00284, ! Not owner of resource.
     X    E$BLOK= 00285, ! Bad block encountered.
     X    E$ISMR= 00286, ! Invalid static mode resume.
     X    E$BLIN= 00287, ! Bad line number.
     X    E$BBUF= 00288, ! Bad buffer number
     X    E$BPRO= 00289, ! Bad protocol
     X    E$LNUS= 00290, ! Line in use
     X    E$BFUS= 00291, ! Buffer in use
     X    E$IRBF= 00292, ! Invalid use of remote buffer
     X    E$IABF= 00293, ! Invalid use of assign line buffer
     X    E$IASD= 00294, ! Invalid ASD use
     X    E$IASP= 00295, ! Invalid sample speed for ASD
     X    E$ILOD= 00296, ! Invalid use of DISLOG
     X    E$NSNI= 00297, ! nss database not initialized
     X    E$NSNC= 00298, ! nss database naming conflict
     X    E$NSAC= 00299, ! nss database address conflict
     X    E$NTHN= 00300, ! nts host not configured
     X    E$NTNS= 00301, ! nts not started
     X    E$NTST= 00302, ! nts already started
     X    E$NTCF= 00303, ! not an nts configuration file
     X    E$NTLC= 00304, ! LHC is unconfigured
     X    E$NTIN= 00305, ! nts database is uninitialized
     X    E$NTDL= 00306, ! LHC is not down-line loaded
     X    E$PLAA= 00307, ! primos line already assoc
     X    E$LLAA= 00308, ! lts line already assoc
     X    E$NASO= 00309, ! not associated
     X    E$NCFG= 00310, ! not configured
     X    E$NXCB= 00311, ! XCB unavailable for request
     X    E$DOQF= 00312, ! Device output queue full
     X    E$LNOC= 00313, ! Line not connected
     X    E$RQF=  00314, ! Request queue full
     X    E$CREJ= 00315, ! Connection rejected
     X    E$CTMO= 00316, ! Connection request timed out
     X    E$LHDN= 00317, ! LHC down
     X    E$LTDN= 00318, ! LTS down
     X    E$NTSH= 00319, ! NTS shutdown
     X    E$QFUL= 00320, ! Queue is full
     X    E$QEMP= 00321, ! Queue is empty
     X    E$NOQ=  00322, ! No queue for queue operation
     X    E$VAL=  00323, ! Validation error
     X    E$COMM= 00324, ! Command illegal for this operation
     X    E$AWIR= 00325, ! Page is already wired
     X    E$IWIR= 00326, ! Page is not wired
     X    E$NPDA= 00327, ! No password directories allowed
     X    E$NINT= 00328, ! System not initialized
     X    E$REIU= 00329, ! Registered EPF in-use.
     X    E$NBA = 00330, ! No Buffers Available
     X    E$LNOW= 00331, ! Line Not Owned By You
     X    E$LNP = 00332, ! Line Not Present on System
     X    E$LNA = 00333, ! LOCK NOT ALLOCATED
     X    E$LDES= 00334, ! LOCK HAS BEEN DESTROYED
     X    E$LNY = 00335, ! LOCK IS NOT YOURS
     X    E$RMLN= 00336, ! ILLEGAL OPERATION ON REMOTE LINE
     X    E$ITLB= 00337, ! INVALID USE OF TERMINAL LINE BUFFER.
     X    E$IPS = 00338, ! INVALID PARAMETER SETTING.
     X    E$DPAR= 00339, ! DUPLICATE PARAMETER.
     X    E$PNS = 00340, ! PARAMETER NOT SETTABLE.
     X    E$BCHK= 00341, ! Bad checksum
     X    E$EXPD= 00342, ! Software has expired
     X    E$DNTS= 00343, ! Density not selected.
     X    E$SNTS= 00344, ! Speed not selected.
     X    E$BMPC= 00345, ! Magtape controller hung.
     X    E$GPON= 00346, ! Generated Passwords ON.
     X    E$NGPW= 00347, ! No Generated PassWords.
     X    E$ISTA= 00348, ! Invalid state
     X    E$ZERO= 00349, ! Uninitialized block on robust part
     X    E$NADM= 00350, ! System not admitting users
     X    E$RXMH= 00351, ! Multiple NPX hop on ext dir entry(root)
     X    E$NNET= 00352, ! The Network isn't running
     X    E$MTPT= 00353, ! Operation illegal on Mount-Point
     X    E$RPMH= 00354, ! Multiple NPX hop on portals
     X    E$MNSH= 00355, ! Multiple name space hop
     X    E$IPTR= 00356, ! Null pointer given.
     X    E$PTHU= 00357, ! Pathname unavailable.
     X    E$IGMT= 00358, ! Inconsistent GMTs on different machines
     X    E$IROO= 00359, ! Operation illegal on root directory.
     X    E$NEWF= 00360, ! Primos rev on remote machine too old.
     X    E$BPOR= 00361, ! Target of a portal must be remote node
     X    E$PRVT= 00362, ! Illegal remote ref. to private disk.
     X    E$ND1S= 00363, ! No Dtar 1 Segments available
     X    E$UMLE= 00364, ! Unexpected GetMutexLock error
     X    E$ERNF= 00365, ! EPF registration level not found
     X    E$APSM= 00366, ! Acat currently protecting shared memory
     X    E$UNLI= 00367, ! Specified user not logged in.
     X    E$MULI= 00368, ! Multiple users logged in
     X    E$CWAT= 00369, ! Can not watch.
     X    E$UBW=  00370, ! User being watched another user.
     X    E$ICUT= 00371, ! Incompatible unit table entry type.
     X    E$IANL= 00372, ! Initial attach point is not local.
     X    E$NOWR= 00373, ! Not open for write
     X    E$NDSM= 00374, ! no dsm message necessary
     X    E$DNTA= 00375, ! Invalid physical disk number (off_line)
     X    E$BFMT= 00376, ! Format not supported on this drive
     X    E$ACPG= 00377, ! Cannot assign active paging disk
     X    E$ACDD= 00378, ! Cannot assign active crash dump disk
     X    E$PWRU= 00379, ! Password recently used.
     X    E$CPLE= 00380, ! Change Password Limit Exceeded.
     X    E$METL= 00381, ! Remote system has truncated the msg
     X    E$PNIR= 00382, ! On_Line Disk Part. NOT being Fix_Disked
     X    E$FDER= 00383, ! Fatal error del file info, OnLine Fix_D
     X    E$FDIR= 00384, ! OnLine Fix_Disk Internal error
     X    E$NOUX= 00385, ! SAD version does not supp. comment fld.
     X    E$LAST= 00385  ! THIS ***MUST*** BE LAST       --
     +)

