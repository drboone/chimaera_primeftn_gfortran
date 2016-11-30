C G_KEYS.INS.f,  SYSCOM,  WODEHOUSE, M.L.ARNAUTOV,  07/01/83
C Definitions and keys for GGRLIB.
C Copyright (c) 1983, Glaxo Group Research, Greenford, Middx
C
C
C Modifications
C Date      Programmer   Description
C 08-Nov-83 M.L.Arnautov Added G$CONT, G$LGID, G$SURN and G$DPT keys
C                        for USRLS$.
C 08-Aug-83 G.M.Lack     Added G$NQRY for YESNOQ.
C 03-Aug-83 Wodehouse    Add CSUB$R as INTEGER*2 (left out in error).
C 06-Jul-83 Wodehouse    Add G$TRPL for YESNOQ and change values of
C                        G$DNO, G$DYES and G$QUIT.
C 06-Jul-83 Wodehouse    Change G$NOQT for G$QUIT for YESNOQ.
C 05-Jul-83 Wodehouse    Add G$NOQT key for YESNOQ.
C 01-Jul-83 Wodehouse    Revamp header and add keys for CMATCH
C                        and YESNOQ.
C
C     TABSET 6 10 24 67
C
      INTEGER*2
     X   COMIN,        ! Terminal/com'-file input length
     X   TNIN,         ! Terminal input length
     X   PASSWD,       ! Password input length
     X   LENGTH,       ! Length of text in buffer
     X   LENBTS,       ! Length of bit-screen
     X   NBTS,         ! Number of bits set
     X   NBITS,        ! Number of bits set (Old name)
     X   NXTBIT,       ! Next bit found set
     X   NUFRD,        ! NUmber FRom Date
     X   NINDW,        ! Number INto Day of Week
     X   COMP$R,       ! String comparison
     X   CSUB$R,       ! Sub-string  comparison
     X   ICOMPN,       ! String comparison
     X   TRNBTS,       ! Overflow indicator
     X   TTYIN,        ! TTY input character
     X   NLSP$R,       ! Number of leading spaces in string
     X   JUSTFY,       ! Length of justified string
     X   CMATCH,       ! Position of match/no-match
     X   YESNOQ        ! Yes, no or quit
C
      INTEGER*4
     X   NLBTS,        ! Number of bits set
     X   NBITSL,       ! Number of bits set (long bits) (old name)
     X   NXTBTL        ! Next bit found set
C
      LOGICAL
     X   STRNG$,       ! String found and not truncated
     X   CPARS$,       ! Command line parsed OK
     X   COMIN$,       ! Got some input
     X   TOKEN$,       ! Not end of buffer or token
     X   TRUBIT,       ! Bit set
     X   BITRUE,       ! Bit set (old name)
     X   BTSLFT,       ! Check on bit lists
     X   TRUBTL,       ! Bit set (long bit)
     X   BTRUEL,       ! Bit set in long bit lists (old name)
     X   TTYRDY,       ! TTY input ready
     X   M$INIT        ! Argument check
C
      REAL*8
     X   DAFRN         ! DAte FRom Number
C
C    The following are keys for GGRLIB
C
      INTEGER*2
     X   G$KEY,  G$KEY0, G$KEY1, G$KEY2, G$KEY3, G$KEY4,
     X   G$KEY5, G$KEY6, G$KEY7, G$KEY8, G$KEY9, G$LAST,
     X   G$BLNK, G$NULL, G$ZERO,
     X   G$UPPR, G$JUST, G$PRNT, G$NOEK, G$NPAD, G$TTY,
     X   G$CANC, G$NDEF, G$BLCM, G$NESC, G$QUOT, G$PARS,
     X   G$DFLT, G$REM,  G$UNDR, G$NEWL, G$PAD1, G$EXP,
     X   G$A1,   G$IN,   G$OUT,  G$INOU, G$UNIT, G$CLOS,
     X   G$RTRN, G$MUTE, G$COML, G$DELE, G$APND, G$LEAV,
     X   G$RDWR, G$COMM, G$DKEY, G$FORC, G$MAND, G$MULT,
     X   G$NONE, G$OPT,  G$SYN,  G$END,  G$INTS,
     X   G$INTL, G$OCTS, G$OCTL, G$SNGL, G$DBL,  G$UPTO,
     X   G$IN2,  G$IN4,  G$RL4,  G$RL8,  G$READ, G$WRIT,
     X   G$EXST, G$ATCH, G$SETH, G$GETU, G$AOPT, G$NEW,
     X   G$OLD,  G$NDAM, G$NSGS, G$REL,  G$ABS,  G$NQRY,
     X   G$STOP, G$CLEA, G$SKIP, G$RANG, G$LINE, G$PRMP,
     X   G$REPL, G$FLD,  G$STOR, G$DISP, G$COND, G$OVER,
     X   G$RDRL, G$RDRS, G$UNST, G$LEAD,
     X   G$6804, G$V100,
     X   G$SCLE, G$CEOS, G$CEOL, G$LINS, G$LDEL, G$BSTP,
     X   G$CURR,
     X   G$PRES, G$ABSN,
     X   G$DNO,  G$DYES, G$QUIT, G$TRPL,
     X   G$LGID, G$DPT,  G$SURN, G$CONT
C
      PARAMETER (
     X
     X !******************** FILL$R ************************
     X !
     X    G$BLNK=8352,  ! Two ASCII spaces
     X    G$NULL=128,  ! Two ASCII nulls
     X    G$ZERO=0,  ! Two ASCII nulls with no parity
     X                     ! (same as zero).
     X !
     X !******************* COMIN$ *************************
     X !
     X    G$DFLT=0,  ! Default key
     X    G$UPPR=1,  ! Force upper case
     X    G$JUST=2,  ! Ignore leading spaces
     X    G$PARS=4,  ! Parse entry already in buffer
     X    G$NOEK=8,  ! No erase/kill processing
     X    G$NESC=16,  ! Switch off logical escape
     X    G$PAD1=32,  ! Pad  to nearest word boundary
     X    G$CANC=64,  ! Echo 'Cancelled' on kill
     X    G$NDEF=128,  ! Do not accept null lines
     X    G$BLCM=256,  ! Replace commas with blanks
     X    G$NPAD=512,  ! Do not pad with blanks
     X    G$QUOT=1024,  ! Switch on quote processing
     X    G$PRNT=2048,  ! Ignore non-printing characters
     X    G$TTY =4096,  ! Force terminal input
     X !
     X !******************* TOKEN$ *************************
     X !
     X ! G$DFLT=0,  ! Default key
     X ! G$UPPR=1,  ! Force upper case
     X ! G$BLCM=256,  ! Treat commas as blanks
     X ! G$NPAD=512,  ! Do not pad with blanks
     X ! G$QUOT=1024,  ! Switch on quote processing
     X    G$REM =2048,  ! Deliver rest of buffer
     X ! G$PAD1=4096,  ! Pad to nearest word boundary
     X !
     X !******************  TTYO  **************************
     X !
     X ! G$DFLT=0,  ! Default key
     X ! G$NPAD=512,  ! Omit trailing blanks
     X    G$UNDR=1024,  ! Underline
     X ! G$PRNT=2048,  ! Print non-printing chars as ~
     X    G$NEWL=4096,  ! Finish with a line feed
     X    G$EXP =8192,  ! Bottom bytes only
     X    G$A1  =16384,  ! Top bytes only
     X    G$UPTO=0,  ! Repeat up to character count
     X !
     X !*****************  FILIO  **************************
     X !
     X    G$IN  =1,  ! Open an input file
     X    G$OUT =2,  ! Open an output file
     X    G$INOU=3,  ! Open an input and output files
     X    G$CLOS=4,  ! Close specified file units
     X    G$UNIT=8,  ! Return file units as units
     X    G$RTRN=16,  ! On error do not crash out
     X    G$COML=64,  ! Search command line only
     X    G$MUTE=128,  ! Suppress error messages
     X    G$RDWR=256,  ! All files in read/write mode
     X    G$DELE=512,  ! If output exists, leave it
     X    G$APND=1024,  ! If output exists, append to it
     X    G$LEAV=2048,  ! If output exists, delete it
     X ! G$TTY =4096,  ! Query user only
     X    G$COMM=8192,  ! Grab name from common buffer
     X !
     X !******************* CPARS$ *************************
     X !
     X ! G$DFLT=0,  ! Default option
     X    G$OPT =0,  ! Optional option
     X    G$DKEY=1,  ! Default keyword
     X    G$FORC=2,  ! Coerce keywords into options
     X    G$MAND=4,  ! Must have an option
     X    G$MULT=8,  ! Multiple options permitted
     X    G$NONE=16,  ! Must not have an option
     X    G$SYN =32,  ! Synonymous keyword
     X ! G$REM =2048,  ! Option is remainder of comline
     X    G$END =16384,  ! End of keyword list
     X !
     X !******************* TREE$$ *************************
     X !
     X    G$READ=1,  ! Open for reading
     X    G$WRIT=2,  ! Open for writing
     X    G$EXST=4,  ! Check existence
     X    G$AOPT=8,  ! Make attach optional
     X    G$ATCH=16,  ! Attach to last UFD in treename
     X    G$SETH=32,  ! Set home when attached
     X    G$GETU=64,  ! Get file unit
     X ! G$RDWR=256,  ! Open for reading and writing
     X ! G$DELE=512,  ! Delete file
     X    G$NDAM=1024,  ! Make it DAM, if creating
     X    G$NSGS=2048,  ! If creating, make it a SEG
     X    G$NEW =4096,  ! File must not exist
     X    G$OLD =8192,  ! File must exist
     X !
     X !******************* FTOVFI *************************
     X !
     X    G$INTS=1,  ! Short decimal
     X    G$INTL=2,  ! Long decimal
     X    G$OCTS=4,  ! Short octal
     X    G$OCTL=8,  ! Long octal
     X !
     X !******************* FTINIT *************************
     X !
     X ! G$TTY =4096,  ! Terminal output required
     X !
     X !******************* FTOVFR *************************
     X !
     X    G$SNGL=1,  ! Single precision real
     X    G$DBL =2,  ! Double precision real
     X !
     X !*******************  FTAB  *************************
     X !
     X    G$REL =1,  ! Relative tabbing
     X    G$ABS =2,  ! Absolute tabbing
     X !
     X !******************* PSORTM *************************
     X !
     X    G$IN2 =32767,  ! INTEGER*2 field
     X    G$IN4 =32766,  ! INTEGER*4 field
     X    G$RL4 =32765,  ! REAL*4 field
     X    G$RL8 =32764,  ! REAL*8 field
     X !
     X !******************* USER$$ *************************
     X !
     X    G$CURR=32767,  ! Return info for current user
     X !
     X !
     X !******************* CMATCH *************************
     X !
     X    G$PRES=0,  ! Find 1st char in string
     X    G$ABSN=1,  ! Find 1st char NOT in string
     X !
     X !******************* YESNOQ *************************
     X !
     X ! G$SNGL=1,  ! Prompt once only
     X ! G$DBL =2,  ! Prompt twice only
     X    G$TRPL=3,  ! Prompt three times only
     X    G$NQRY=64,  ! Do not append ?
     X ! G$NDEF=128,  ! No default answer
     X    G$DNO =16384,  ! Default answer is no
     X    G$DYES=8192,  ! Default answer is yes
     X    G$QUIT=0,  ! Quit allowed
     X !
     X !****************************************************
     X !******************* SCRIPT *************************
     X !****************************************************
     X !******************* ADDR$S *************************
     X !
     X    G$PRMP=64,  ! Prompt field
     X    G$REPL=2048,  ! Reply field
     X !
     X !******************* DRAW$S *************************
     X !
     X ! G$FORC=2,  ! Force complete redraw
     X    G$LINE=4,  ! Draw lines (default)
     X    G$RANG=8,  ! Draw range of items
     X ! G$PRMP=64,  ! Draw prompts too
     X    G$FLD =128,  ! Draw fields
     X    G$COND=256,  ! Do not bother if refresh
     X    G$OVER=512,  ! Display reply buffers
     X    G$CLEA=2048,  ! Clear reply buffers
     X    G$UNST=8192,  ! Overwrite field stop
     X    G$STOP=16384,  ! Display field stop
     X !
     X !******************* INIT$S *************************
     X !
     X ! G$DFLT=0,  ! No special action
     X ! G$MUTE=128,  ! Don't draw screen
     X ! G$CLEA=2048,  ! Clear reply buffers
     X ! G$END =16384,  ! Prepare to exit
     X !
     X    G$6804=1,  ! Pericom 6804
     X    G$V100=2,  ! VT100
     X !
     X ! G$PRMP=64,  ! Prompt field
     X ! G$REPL=2048,  ! Reply field
     X    G$SKIP=32767,  ! Skip this field
     X !
     X !******************* INPT$S *************************
     X !
     X ! G$UPPR=1,  ! Force upper case
     X ! G$JUST=2,  ! Left justify
     X ! G$NOEK=8,  ! Disable erase/kill
     X ! G$OVER=512,  ! Overlay
     X ! G$QUOT=1024,  ! Enable quote processing
     X ! G$CLEA=2048,  ! Clear reply buffer
     X ! G$TTY =4096,  ! TTY input only
     X ! G$STOP=16384,  ! Display field stop
     X !
     X    G$RDRL=4096,  ! Re-draw line
     X    G$RDRS=8192,  ! Re-draw screen
     X !
     X !******************* QUER$S *************************
     X !
     X ! G$UPPR=1,  ! Force upper case
     X ! G$JUST=2,  ! Left justify
     X ! G$NOEK=8,  ! No erase/kill
     X ! G$NPAD=512,  ! No padding
     X ! G$QUOT=1024,  ! Quote processing on
     X ! G$TTY =4096,  ! TTY input only
     X    G$LEAD=8192,  ! Display leader
     X ! G$STOP=16384,  ! Display field stop
     X !
     X ! G$RDRL=4096,  ! Re-draw line
     X ! G$RDRS=8192,  ! Re-draw screen
     X !
     X !******************* RMRK$S *************************
     X !
     X    G$STOR=1,  ! Store remark
     X    G$DISP=2,  ! Display remark
     X !
     X !******************* SCRN$S *************************
     X !
     X    G$SCLE=1,  ! Clear screen
     X    G$CEOS=2,  ! Clear to end of screen
     X    G$CEOL=3,  ! Clear to end of line
     X    G$LINS=4,  ! Insert line
     X    G$LDEL=5,  ! Delete line
     X    G$BSTP=6,  ! Output back stop
     X !
     X !******************* USRLS$ *************************
     X !
     X    G$CONT=0,  ! Continue last call
     X    G$LGID=1,  ! Search lognam index
     X    G$DPT =2,  ! Search departmental index
     X    G$SURN=4,  ! Search surname index
     X !
     X !****************************************************
     X !
     X    G$LAST=0   ! Must be last
     X !
     X !****************************************************
C
C     Two SCRIPT library functions
C
     +)
      INTEGER*2 INPT$S,QUER$S
C
C     The variables below have been left out of KEYS.INS.f
C
      INTEGER*2 GCHAR,DUPLX$
C
      LOGICAL NAMEQ$,COMEQ$,NAMEQV,COMEQV,TEXTO$,MISSIN
C

