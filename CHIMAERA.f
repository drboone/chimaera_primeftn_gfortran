C*********************************************************************
C*
C*    CHIMAERA
C*
C*    An adventure program using automatic text generation
C*    and with consistent but unpredictable moves generated
C*    by  an integer function in subroutine LOCATE.
C*
C*    *****************************************
C*    *                                       *
C*    *  Written by Nicholas Perre-Wetherall  *
C*    *          Copyright 1984               *
C*    *        All rights reserved            *
C*    *                                       *
C*    *****************************************
C*
C*    Version 1.101    -    16th January 1984.
C*
C*    ***  Revision History  ***
C*
C*    Version 1.102  15th Mar 1984  New form of WHERE verb added.
C*                   12th Apr 1984  Minor bug in KILL fixed.
C*                   16th Apr 1984  TIME command added.
C*
C*    Version 1.103  11th Sep 1984  ADVENTURE, CHASE/FOLLOW and EXAMINE
C*                                  commands added. HELP command improved.
C*                                  TIME value incremented automatically
C*                                  if user response is slow.
C*
C*    Version 1.104  13th Nov 1984  PLUGH routine made more friendly.
C*
C*    Version 1.105   5th Dec 1984  Minor bugs in HELP and SLEEP fixed.
C*
C*    Version 1.106   7th Jul 1985  Further bug in HELP fixed.
C*                                  Experimental version warning removed.
C*                                  Expletive handling improved.
C*                                  Further amendment to PLUGH routine.
C*                                  CHIMAERA_NEWS file inserted.
C*
C*    Version 1.107  22nd Jul 1985  Improved handing of elixir in EAT.
C*                                  Scores in EAT rationlaised.
C*
C*    Version 1.108  20th Aug 1985  Minor bug in troll text fixed.
C*                                  Minor amendment to snake coding
C*                                  Minor amendment to dwarf coding
C*                                  Amendments to code and scoring
C*                                  in vampire (label 8180).
C*                                  Give player score and remove snake
C*                                  permanently after charming it.
C*                                  Score incremented for stamping
C*                                  out dwarves.
C*
C*    Version 1.109  21st Aug 1985  Thirst handling improved and messages
C*                                  added.
C*
C*    Version 1.110  21st May 1986  Code added to stop players continually
C*                                  relighting a flickering lamp.
C*
C*    Version 1.111  18th Jun 1986  Minor bug in STAMP DWARF fixed.
C*                                  LOCATE routine tidied up.
C*
C*    Version 1.112  30th Jun 1986  Monster status in restored game
C*                                  written out to log file.
C*
C*    Version 1.113  30th Sep 1986  Bug in PUT routine ($1135) fixed.
C*
C*    *****************************************************************
C*
      SUBROUTINE MAIN

      IMPLICIT INTEGER*2 (A-Z)

C*
C*
#include "KEYS.INS.f"
#include "ERRD.INS.f"
#include "G_KEYS.INS.f"
#include "CHIMAERA.INS.f"
C*
      INTEGER*2 LINE(66),    !  Input buffer
     +          TIMBUF(28),  ! TIMDAT buffer
     +          BUFFER(10),  ! Temporary text buffer
     +          LEN,         !  LINE length
     +          TOKNO,       !  Tokens on line
     +          LITACT(8),   !  Literal form of action verb
     +          LITOBJ(8)    !  Literal form of object noun
C*
      REAL*8    DICACT(H$ACT),         !  Action verbs
     +          PSEUDO(H$PSEU)              !  Pseudo objects (Camp, etc.)
C*
      INTEGER*2 OBJLOC(H$OBJ),    !  Object locations
     +          POINTS(H$OBJ),    !  Object scores
     +          THINGS(20,H$OBJ),      !  Object descriptions
     +          IPT(H$OBJ)             ! Pointers for random access
C*
      INTEGER*2 VERBS(10,7),      !  Text verbs
     +          NOUNS(10,18),     !  Text nouns
     +          ADJONE(10,16),    !  First adjective
     +          ADJTWO(10,12),    !  Second adjective
     +          ROUTES(5,8)       !  Direction text
C*
      INTEGER*2 MDAYS(12),   ! Days in month
     +          MONTHS(5,12),     ! Names of months
     +          WEEKDY(5,7)       ! Names of days
C*
      INTEGER*2 COWS(5,7),        !  Herbivours
     +          CATS(5,4),        !  Carnivours
     +          BEASTS(4,H$MON)   !  Monsters
C*
      INTEGER*2 XINC(10),YINC(10),ZINC(10), !  Move increments
     +          WAYBIT(10)   !  Move bit screens
C*
      INTEGER*2 MOVENO,      !  Move number
     +          SCORE   !  Current score excluding objects in tent
C*
      INTEGER*2 VALUES(H$VAL)     ! Integer values
      INTEGER*2 MONSTR(H$MON)     ! Monster values
      LOGICAL   FLAGS(H$FLAG)     ! Logical flags
C*
      INTEGER*4 DELAY        !  Delay period for SLEEP$
C*
      REAL*4 DISTX,          ! Distance east from tent
     +       DISTY           ! Distance north from tent
C*
      REAL*8 EXPLET(H$EXP)   !  Expletives
      REAL*8 NUMBER(5)       ! Text numbers 1 - 5
C*
      REAL*8 ACTION,    ! 8 char word for actions from user
     +       OBJECT,    ! 8 char word for objects from user
     +       THING,     ! 8 char UC word from object array
     +       BLANK,     ! Spacefilled 8 char word
     +       TMPOBJ,    ! Temporary 8 char store for object
     +       TMPACT     ! Temporary 8 char store for action
C*
      LOGICAL START
      LOGICAL COMEQV,YES
      LOGICAL WAYS(15)       !  Move directions
      LOGICAL HITYOU
C*
      LOGICAL OBHERE(H$OBJ)       !  Objects (not) here.
      LOGICAL ALLDUN(H$OBJ)       !  Objects in tent.
      LOGICAL GONE(H$OBJ)         !  Objects vanished for ever.
C*
      PARAMETER ( NORTH=1,
     +          SOUTH=2,
     +          EAST=3,
     +          WEST=4,
     +          SE=5,
     +          SW=6,
     +          NE=7,
     +          NW=8,
     +          UP=9,
     +          DOWN=10,
     +          IN=11,
     +          OUT=12
C*
C*    Paramters for VALUES array
C*
     +)
      PARAMETER ( BASE=6,      !  Basecamp location
     +          FIRSTX=7,    !  Base X co-ordinate
     +          FIRSTY=8,    !  Base Y co-ordinate
     +          LASTX=9,     !  Last X co-ordinate
     +          LASTY=10,    !  Last Y co-ordinate
     +          LASTZ=11,    !  Last Z co-ordinate
     +          LUCK=12,     !  Players luck quota
     +          TIME=13,     !  Time of day
     +          DAYLEN=14,   !  Length of day
     +          DUSK=15,     !  Dusk pointer
     +          HELD=16,     !  Number of things he holds
     +          LIGHT=17,    !  Amount of light left in lamp
     +          POOL=18,     !  Water pool
     +          MAGONE=19,   !  Times he has used first magic word
     +          DWFNUM=20,   !  Total number of dwarves
     +          DWFNOW=21,   !  Number of active dwarves
     +          MONCNT=22,   !  Number of active monsters
     +          GORLOC=23,   !  Location of gorgon
     +          SEED=24,     !  Seed for RND(N)
     +          BRKBOT=25,   !  Broken glass from bottle
     +          BRKVAS=26,   !  Broken glass from vase
     +          BRKGOB=27,   !  Broken glass from goblet
     +          BRKMIR=28,   !  Broken glass from mirror
     +          THIRST=29,   !  Thirst rating
     +          FLICK=30     ! Flickering lamp count
C*
     +)
      PARAMETER ( BATS=1,
     +          DWARF=2,
     +          SNAKE=3,
     +          GORGON=4,
     +          ELF=5,
     +          TROLL=6,
     +          DRAGON=7,
     +          VAMPIR=8
C*
     +)
      PARAMETER ( MASTER=1,    ! Master player flag
     +          NIGHT=2,     ! Night flag
     +          DARK=3,      ! Dark here flag
     +          GLOW=4,      ! Underground glow flag
     +          LAMPON=5,    ! Lamp on flag
     +          WAVER=6,     ! Glow (if any) wavers
     +          FR13=7,      ! Friday 13th flag
     +          HALLOW=8,    ! Halloween flag
     +          LIFE=9       ! Life (or death) flag
C*
     +)
      PARAMETER ( BOXLOK=10,   ! Box locked flag
     +          DORLOK=11    ! Door locked flag
C*
     +)
      PARAMETER ( ROLING=12,   ! Rolling grass desc. flag
     +          HIGRAS=13,   ! High grass flag
     +          ANIMAL=14,   ! Animals in sight flag
     +          HERD=15,     ! Herd of animals flag
     +          CARNIV=16,   ! Carnivores flag
     +          SEEN=17      ! Seen by carnivores flag
C*
     +)
      PARAMETER ( DDOT=18,     ! Full Stop flag
     +          TREE=19,     ! Tree in sight flag
     +          NOTUP=20,    ! Not up tree flag
     +          HOLE=21,     ! Hole in sight flag (Not used)
     +          HOME=22,     ! At base camp flag
     +          INTENT=23    ! In the tent flag
C*
     +)
      PARAMETER ( SMALL=24,    ! Small location flag
     +          LARGE=25,    ! Large location flag
     +          SWITCH=26,   ! Flip flop flag
     +          LEAD=27      ! Lead(s/ing) flag
C*
     +)
      PARAMETER ( UNDEAD=28,   ! Player undead flag
     +          GENIE=29,    ! Genie flag
     +          ELIXIR=30,   ! Drunk elixir of life
     +          EMPTY=31,    ! Water bottle empty flag
     +          DRIP=32      ! Water drips
C*
     +)
      PARAMETER ( COPRNT=33,   ! Flag for printing coordinates
     +          SHAFT=34     ! Underground shaft flag
C*
     +)
      COMMON/INPUT/LINE,LEN,TOKNO,ACTION,LITACT,OBJECT,LITOBJ
      COMMON/DICTON/DICACT,VERBS,NOUNS,ADJONE,ADJTWO
      COMMON/DESCRP/THINGS,PSEUDO
      COMMON/SCORES/SCORE,MOVENO,ALLDUN,POINTS
      COMMON/DMPCOM/DUMP(40)
C*
      DATA DUMP/2,'C_',1,'_ ',4,'MAGE',33*0/
C*
      DATA MONTHS/'January   December  February  November  ',
     +            'March     October   April     September ',
     +            'May       August    June      July      '/
C*
      DATA WEEKDY/'Monday    Tuesday   Wednesday Thursday  ',
     +            'Friday    Saturday  Sunday    '/
C*
      DATA COWS/'antelopes wildebeest',
     +          'zebras    gazelles  ',
     +          'deer      elephants ',
     +          'buffalos  '/
      DATA CATS/'leopard   tiger     lion      lynx      '/
C*
      DATA ROUTES/'north     south     east      west      ',
     +            'northeast northwest southeast southwest '/
C*
      DATA EXPLET/'BLAST   BUGGER  DAMN    SHIT    SOD     ',
     +            'HELL    FUCK    PISS    WANKER  BOLLOCKS',
     +            'CUNT    ARSEHOLE                        '/
C*
      DATA NUMBER/'one     two     three   four    five    '/
C*
      DATA BEASTS/'bat     dwarf   snake   gorgon  ',
     +            'elf     troll   dragon  vampire '/
C*
      DATA MDAYS/31,31,28,30,31,31,30,30,31,31,30,31/
      DATA WAYBIT/8,8,16,16,32,64,64,32,128,128/
C*
      DATA XINC/0,0,1,-1,1,-1,1,-1,0,0/
      DATA YINC/1,-1,0,0,1,1,-1,-1,0,0/
      DATA ZINC/0,0,0,0,0,0,0,0,-1,1/
C*
      DATA MONSTR/H$MON*-1/       ! Initialise all monsters
C*
C*    Initialise all VALUES to 0
C*
      CALL ZFIL$R(VALUES,H$VAL)
C*
      DELAY=2000
      HELPNO=0
      ADVNO=11
      VALUES(THIRST)=0       !  Not thirsty yet
      VALUES(BRKBOT)=-1      !  Location of smashed bottle
      VALUES(BRKVAS)=-1      !  Location of smashed vase
      VALUES(BRKGOB)=-1      !  Location of smashed goblet
      VALUES(BRKMIR)=-1      !  Location of smashed mirror
      VALUES(DWFNOW)=0  ! No dwarves active yet
      VALUES(GORLOC)=-1 ! Gorgon has no location yet
      VALUES(SEED)=-1   !  Initialise for use as flag first
      VALUES(FLICK)=0        ! Lamp not flickering yet
C*
      START=.TRUE.
      FLAGS(MASTER)=.FALSE.
      FLAGS(COPRNT)=.FALSE.
      FLAGS(FR13)=.FALSE.
      FLAGS(HALLOW)=.FALSE.
      FLAGS(LIFE)=.TRUE.
      FLAGS(HOME)=.TRUE.
      FLAGS(DARK)=.FALSE.
      FLAGS(NIGHT)=.FALSE.
      FLAGS(LAMPON)=.FALSE.
      FLAGS(INTENT)=.FALSE.
      FLAGS(HIGRAS)=.FALSE.
      FLAGS(ANIMAL)=.FALSE.
      FLAGS(HERD)=.TRUE.
      FLAGS(CARNIV)=.FALSE.
      FLAGS(SEEN)=.FALSE.
      FLAGS(BOXLOK)=.TRUE.
      FLAGS(DORLOK)=.TRUE.
      FLAGS(UNDEAD)=.FALSE.
      FLAGS(GENIE)=.FALSE.
      FLAGS(ELIXIR)=.FALSE.
      FLAGS(EMPTY)=.TRUE.
      FLAGS(GLOW)=.FALSE.
      FLAGS(WAVER)=.FALSE.
      FLAGS(SHAFT)=.FALSE.
C*
      DO 5 I=1,H$OBJ
         ALLDUN(I)=.FALSE.
         GONE(I)=.FALSE.
         OBHERE(I)=.FALSE.
5     CONTINUE
C*
      BLANK='        '
      TMPOBJ=BLANK
      TMPACT=BLANK
C*
      CALL TNOUA(BLANK,8)
      CALL TONL
      CALL TNOU(
     +'               ***  Welcome to the world of  CHIMAERA  ***',58)
      CALL TONL
      CALL TNOU(
     +'                  (Created by Nicholas Perre-Wetherall)',55)
      CALL TONL
      CALL TNOU(
C*     1--------1---------2---------3---------4---------5---------6
     +'                   (GGR release 30th September  1986)  ',55)
      CALL TNOU(
     +'                            Version : 1.113',43)
      CALL TONL
      CALL TNOU(
     +'    Command me and I will be your guide. There is treasure',58)
      CALL TNOU(
     +'to be found but also much danger. Few who venture here escape',61
     +)
      CALL TNOU(
     +'unchanged but you may succeed where others have failed!!',56)
C*
      CALL TONL
C*
      CALL TIMDAT(TIMBUF,28)
      I=MOD(TIMBUF(8),50)+1
      IF (I.GT.4) GOTO 9999
C*
      CALL TNOU(
     +'==================================================',50)
      CALL TONL
      GOTO (9910,9920,9930,9940),I
C*
9910  CALL TNOU('Having trouble with space and time? If so use',45)
      CALL TNOU('TIME and WHERE to get a fix!',28)
      GOTO 9995
C*
9920  CALL TNOU('The dwarves seem more aggressive these days,',44)
      CALL TNOU('you had better find out how to get rid of them!',47)
      GOTO 9995
C*
9930  CALL TNOU('Flickering lamps can not be relit indefinitely,',47)
      CALL TNOU('you must find a more permanent solution!',40)
      GOTO 9995
C*
9940  CALL TNOU(
     +'Help is precious, away from your camp you can only',50)
      CALL TNOU(
     +'use it twice. Save it for real emergencies only or you may ',59)
      CALL TNOU(
     +'get completely stuck underground. There is also a magic',55)
      CALL TNOU(
     +'word to assist you. Wonder if anybody has found it yet...',57)
C*
9995  CALL TNOU(
     +'==================================================',50)
      CALL TONL
C*
9999  CONTINUE
C*
C*********************************************************************
C*
      CALL LEXICN       !  Load dictionary
      CALL WORTH        !  Load object points array
      SCORE=0
C*
      CALL COMIN$(G$PRNT+G$JUST+G$UPPR,
     +'Do you want instructions? ',26,LINE,132,LEN,TOKNO)
      IF (LEN.EQ.0) GOTO 10
      IF (COMEQV(LINE,'YES')) CALL INSTR(START)
10    CALL TONL
      CALL TNOU(
     +'There are eleven starting points, ten are standard',50)
      CALL TNOU('and one (number 0) is random.',29)
      CALL TONL
20    CALL COMIN$(G$PRNT+G$JUST+G$UPPR,'Choose one (0 - 10) > ',22,
     +LINE,20,LEN,TOKNO)
      IF (TOKNO.NE.1) GOTO 20
      IF (COMP$R(8,LINE,'MASTER  ').NE.0) GOTO 27
      CALL STATUS(FLAGS(MASTER),SCORE)
      IF (.NOT.FLAGS(MASTER)) GOTO 20
21    CALL TONL
      CALL COMIN$(G$JUST+G$PRNT,
     +'Seed for random numbers > ',26,LINE,132,LEN,TOKENS)
      IF (TOKENS.EQ.0) GOTO 23
      READ(UNIT=LINE,ERR=21) VALUES(SEED)
23    CALL TONL
      CALL COMIN$(G$UPPR+G$JUST+G$PRNT,
     +'Print co-ordinates? ',20,LINE,132,LEN,TOKENS)
      IF (COMEQV(LINE,'YES')) FLAGS(COPRNT)=.TRUE.
      GOTO 20
C*
27    READ(UNIT=LINE,ERR=20) ADVNO
      IF (ADVNO.LT.0.OR.ADVNO.GT.10) GOTO 20
C*
C*    Seed random number genarator
C*
      CALL RND(10*ADVNO)
      CALL LOCWUN(OBJLOC)
      CALL TIMDAT(TIMBUF,28)
      VALUES(SEED)=TIMBUF(4)+60.*TIMBUF(5)
      CALL RND(VALUES(SEED))
C*
      VALUES(DWFNUM)=RANDOM(4)+2       ! Initial dwarf population
      VALUES(MONCNT)=0                 ! No active monsters
      MONTH=RANDOM(12)+1               ! Set the month
      DAYNUM=MDAYS(MONTH)
      DAY=RANDOM(DAYNUM)+1
      CALL TONL
C*
C*    Get date and day of month and print them
C*
      WKDAY=RANDOM(7)+1
      CALL TONL
      CALL TNOUA('It is ',6)
      RAND=RANDOM(20)
      IF (RAND.EQ.13) FLAGS(FR13)=.TRUE.
      IF (RAND.EQ.10) FLAGS(HALLOW)=.TRUE.
      IF (FLAGS(FR13)) GOTO 60
      CALL TNOUA(WEEKDY(1,WKDAY),LENGTH(WEEKDY(1,WKDAY),10))
      CALL T1OU(160)
      IF (FLAGS(HALLOW)) GOTO 80
      CALL T1OU(160)
      CALL TOVFIS(DAY,0)
      IF (DAY.EQ.1.OR.DAY.EQ.21) GOTO 30
      IF (DAY.EQ.2.OR.DAY.EQ.22) GOTO 40
      IF (DAY.EQ.3.OR.DAY.EQ.23) GOTO 50
      CALL TNOUA('th. ',4)
      GOTO 70
30    CALL TNOUA('st. ',4)
      GOTO 70
40    CALL TNOUA('nd. ',4)
      GOTO 70
50    CALL TNOUA('rd. ',4)
      GOTO 70
60    CALL TNOUA('Friday 13th. ',13)
70    CALL TNOUA(MONTHS(1,MONTH),LENGTH(MONTHS(1,MONTH),10))
      GOTO 90
80    CALL TNOUA('31st. October (Halloween)',25)
      MONTH=10
90    CALL T1OU(174)
      CALL TONL
      VALUES(DAYLEN)=MONTH*2+12        ! Range 14 - 36
      CALL TNOU(
     +'The sun shines blood red through the dawn!',42)
      VALUES(TIME)=1
      FLAGS(NIGHT)=.FALSE.
      Z=1
      IF (ADVNO.NE.11) GOTO 100
      X=RANDOM(15)*15+15
      Y=2000/X
      GOTO 110
100   X=10*ADVNO
      Y=1500/X
C*
C*    First move, set the move counter and print the first
C*    move text.
C*
110   MOVENO=-1
      VALUES(FIRSTX)=X
      VALUES(FIRSTY)=Y
      VALUES(TIME)=1
      VALUES(HELD)=0
      VALUES(MAGONE)=0
      VALUES(LIGHT)=100
      COW=1
      CAT=1
      VALUES(DUSK)=VALUES(DAYLEN)-1
      VALUES(LUCK)=RANDOM(40)
      IF (FLAGS(FR13)) VALUES(LUCK)=RANDOM(10)
      CALL MOVE$R(177,DICACT,273,DICACT,1)
      CALL LOCATE(X,Y,Z,VALUES(BASE))
      CALL TONL
      CALL TNOUA(
     +'You are standing on a wide grassy plain; far off the snow',57)
      CALL TNOU(' clad',5)
      CALL MOVE$R(548,DICACT,274,DICACT,1)
      CALL TNOUA(
     +'tops of distant mountains gleam in the rays of the rising',57)
      CALL TNOU(' sun.',5)
      CALL MOVE$R(18,DICACT,275,DICACT,1)
      CALL TNOU(
     +'Isolated trees are dotted about the landscape and groups',56)
      CALL MOVE$R(81,DICACT,276,DICACT,1)
      CALL TNOU(
     +'of animals can dimly be seen moving about some way off.',55)
      CALL MOVE$R(545,DICACT,277,DICACT,1)
C*
C*    ***************************************
C*    *                                     *
C*    *  All set up, now for the main loop  *
C*    *                                     *
C*    ***************************************
C*
120   CALL LOCATE(X,Y,Z,HERE)
      CHARS=0           ! Ensure display line length is zero for now.
      DO 125 I=1,H$OBJ
         OBHERE(I)=.FALSE.   ! Unset all object here flags
125   CONTINUE
      VALUES(TIME)=VALUES(TIME)+1
      IF (OBJLOC(O$LAMP).NE.0) FLAGS(LAMPON)=.FALSE.
      IF (FLAGS(LAMPON)) VALUES(LIGHT)=VALUES(LIGHT)-1
      IF (FLAGS(LAMPON).AND.OBJLOC(O$LAMP).EQ.0) FLAGS(DARK)=.FALSE.
      FLAGS(NIGHT)=.FALSE.
      FLAGS(DRIP)=.FALSE.
      FLAGS(SHAFT)=.FALSE.
      VALUES(POOL)=-1   ! Unset this variable
      IF (VALUES(TIME).GT.48) VALUES(TIME)=1
      IF (VALUES(TIME).GT.VALUES(DAYLEN)) FLAGS(NIGHT)=.TRUE.
      IF (.NOT.FLAGS(NIGHT).AND.FLAGS(UNDEAD)) GOTO 150
      IF (FLAGS(NIGHT).AND.(.NOT.FLAGS(LAMPON))) FLAGS(DARK)=.TRUE.
      IF (Z.GT.1.AND.(.NOT.FLAGS(LAMPON))) FLAGS(DARK)=.TRUE.   ! Dark here
      IF (FLAGS(UNDEAD)) FLAGS(DARK)=.FALSE.     ! The undead can always see
      MOVENO=MOVENO+1
      IF (.NOT.FLAGS(UNDEAD)) VALUES(THIRST)=VALUES(THIRST)+1
      GOTO 160
C*
C*    Redistribute all the objects as at the start of the game.
C*
130   CALL TONL
      CALL TNOU(
     +'There is a blinding flash!!!!',29)
      CALL SLEEP$(DELAY)     ! Delay
      CALL TONL
      CALL TNOU(
     +'                and',19)
      CALL SLEEP$(DELAY)     !  Delay
      CALL TONL
      Z=1
      X=10*ADVNO
      Y=1500/X
      FLAGS(ANIMAL)=.FALSE.
      FLAGS(CARNIV)=.FALSE.
      FLAGS(DARK)=.FALSE.
      FLAGS(LAMPON)=.FALSE.
      FLAGS(GLOW)=.FALSE.
      FLAGS(WAVER)=.FALSE.
      FLAGS(SHAFT)=.FALSE.
      FLAGS(DRIP)=.FALSE.
      FLAGS(NIGHT)=.FALSE.
      IF (FLAGS(UNDEAD)) GOTO 120
C*
      SCORE=SCORE-50
      VALUES(TIME)=1
      VALUES(HELD)=0
C*
C*    Reposition objects, etc.
C*
      CALL LOCWUN(OBJLOC)    ! Randomise objects
C*
C*    and reset those in tent and gone forever.
C*
      DO 140 I=1,H$OBJ
         IF (ALLDUN(I)) OBJLOC(I)=-1000
         IF (GONE(I)) OBJLOC(I)=-2000
         IF (OBJLOC(I).EQ.-2000) GONE(I)=.TRUE.  ! Set by LOCWUN
140   CONTINUE
C*
C*    Reinitialise all monsters that still exist, those
C*    that don't remain at zero.
C*
      DO 145 I=1,H$MON
         IF (MONSTR(I).NE.0) MONSTR(I)=-1
145   CONTINUE
      VALUES(DWFNOW)=0  ! Dwarves not active yet
C*
      GOTO 120
C*
C*    You have become a vampire yourself (undead) and must sleep
C*    during the day.
C*
150   IF (Z.GT.1) GOTO 154
      CALL TNOUA('T',1)
      GOTO 156
154   CALL TNOUA('Outside t',9)
156   CALL TNOU(
     +'he sun is rising and you fall into a deep slumber',49)
      CALL TNOU(
     +'from which you do not awake until after nightfall.',50)
      VALUES(TIME)=VALUES(DAYLEN)+1
      CALL SLEEP$(DELAY)
      GOTO 120
C*
160   DO 170 I=1,15
         WAYS(I)=.FALSE.     !  All ways = .FALSE.
170   CONTINUE
            IF (.NOT.FLAGS(COPRNT)) GOTO 175
            CALL TODEC(X)
            CALL T1OU(160)
            CALL TODEC(Y)
            CALL T1OU(160)
            CALL TODEC(Z)
            CALL TONL
175   IF (.NOT.FLAGS(INTENT)) GOTO 180
      WAYS(OUT)=.TRUE.       !  Only way is out.
      CALL TNOUA(
     +'You are in the tent, your possessions lie scattered',51)
      CALL TNOU(' about on your camp bed.',24)
      CALL TNOUA(
     +'These include a few torn and useless clothes',44)
      GOTO 4550
C*
180   IF (Z.GT.1) GOTO 185
      IFLAG=0
C*
      IF (MOVENO.GT.0.AND.VALUES(TIME).LE.2) IFLAG=1
      IF (IFLAG.EQ.1) CALL TNOU('The sun is rising in the east.',30)
C*
      IF (VALUES(TIME).LT.VALUES(DUSK).AND.
     +VALUES(TIME).GE.VALUES(DUSK)-2) IFLAG=2
      IF (IFLAG.EQ.2) CALL TNOU('The sun is getting low.',23)
C*
      IF (.NOT.FLAGS(NIGHT).AND.VALUES(TIME).GE.VALUES(DUSK))
     +IFLAG=3
      IF (IFLAG.EQ.3) CALL TNOU('The sun is sinking in the west.',31)
C*
      IF (VALUES(TIME).EQ.(VALUES(DAYLEN)+1)) IFLAG=4
      IF (IFLAG.EQ.4) CALL TNOU(
     +'The sun has just set, it is night!',34)
C*
185   IF (Z.NE.1) GOTO 330   ! Below ground or up tree
C*
C*    *************************************************
C*    *                                               *
C*    *  On the plain, all horizontal moves possible  *
C*    *                                               *
C*    *************************************************
C*
      IF (VALUES(TIME).EQ.1) FLAGS(DARK)=.FALSE.
      DO 190 I=1,8
         WAYS(I)=.TRUE.
190   CONTINUE
      FLAGS(HOME)=.FALSE.
      IF (HERE.EQ.VALUES(BASE)) FLAGS(HOME)=.TRUE.
      IF (FLAGS(HOME)) WAYS(IN)=.TRUE.
C*
      FLAGS(TREE)=.FALSE.
      IF (MOVENO.EQ.0) GOTO 260   ! Skip calc. text.
C*
C*    Find out if there is a hole or tree here.
C*
      IF (AND(HERE,18).EQ.18) FLAGS(TREE)=.TRUE.      ! Probability 1:8 overall
      UNDER=Z+1
      CALL LOCATE(X,Y,UNDER,BELOW)
      DDOWN=WAYBIT(DOWN)
      IF (AND(HERE,DDOWN).EQ.DDOWN) WAYS(DOWN)=.TRUE.
      IF (FLAGS(HOME)) FLAGS(TREE)=.FALSE.       ! No tree at camp.
      IF (FLAGS(HOME)) WAYS(DOWN)=.FALSE.        ! No hole at camp.
      IF(WAYS(DOWN)) FLAGS(TREE)=.FALSE.              ! No tree by a hole
      IF (WAYS(DOWN).AND.FLAGS(DARK)) GOTO 327        ! Fallen down hole, kill.
      IF (FLAGS(DARK)) GOTO 325
C*
C*    Find out if you can get up again
C*
      IF (.NOT.WAYS(DOWN)) GOTO 200         !  No hole
      FLAGS(NOTUP)=.TRUE.
      IF (AND(BELOW,DDOWN).EQ.DDOWN) FLAGS(NOTUP)=.FALSE.
C*
200   IF(FLAGS(TREE)) WAYS(UP)=.TRUE.            ! Can climb tree
      FLAGS(DDOT)=.TRUE.
      IF (IFLAG.GT.0) CALL TONL
      CALL TNOUA('You are standing on the plain',29)
      CHARS=CHARS+29
      IF (.NOT.FLAGS(NIGHT)) GOTO 205       ! No long view at night
      CALL TNOUA('. It is night',13)
      CHARS=CHARS+14
      FLAGS(CARNIV)=RANDOM(5).EQ.1     !* ?? MLA
      IF (.NOT.FLAGS(LAMPON)) CALL TNOUA('. ',2)
      IF (.NOT.FLAGS(LAMPON)) GOTO 260
C*
      IF (CHARS.GT.0) CALL T1OU(160)
      CALL TNOUA('and you can ',12)
      CHARS=CHARS+13
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('see nothing ',12)
      CHARS=CHARS+12
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('beyond the small ',17)
      CHARS=CHARS+17
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('area illuminated ',17)
      CHARS=CHARS+17
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('by the rays of ',15)
      CHARS=CHARS+15
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('your lamp. ',11)
      CHARS=CHARS+11
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      GOTO 260
C*
205   IF (RANDOM(8).EQ.5) FLAGS(ANIMAL)=.TRUE.
      IF(FLAGS(ANIMAL)) FLAGS(DDOT)=.FALSE.
      FLAGS(HERD)=.TRUE.
      IF (RANDOM(4).EQ.2) FLAGS(HERD)=.FALSE.
      FLAGS(ROLING)=.FALSE.
      IF (AND(HERE,72).NE.72) FLAGS(ROLING)=.TRUE.
      FLAGS(HIGRAS)=.FALSE.
      IF (AND(HERE,40).EQ.40) FLAGS(HIGRAS)=.TRUE.
      IF (FLAGS(ROLING)) FLAGS(DDOT)=.FALSE.
      IF (FLAGS(HIGRAS)) FLAGS(DDOT)=.FALSE.
      IF (FLAGS(DDOT)) CALL TNOUA('. ',2)
      IF (.NOT.FLAGS(ROLING)) GOTO 210
      CALL TNOUA(', rolling ',10)
      CHARS=CHARS+10
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('grassland ',10)
      CHARS=CHARS+10
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('stretches to ',13)
      CHARS=CHARS+13
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('the horizon. ',13)
      CHARS=CHARS+13
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
210   IF (.NOT.FLAGS(ANIMAL)) COW=RANDOM(7)+1         !  Herbivore species
      IF (.NOT.FLAGS(CARNIV)) CAT=RANDOM(4)+1         !  Carnivore species
      IF (FLAGS(HIGRAS)) GOTO 240
      IF (.NOT.FLAGS(ANIMAL)) GOTO 260
C*
      IF (FLAGS(ROLING)) GOTO 220
      IF (FLAGS(HERD)) CALL TNOUA(', h',3)
      IF (.NOT.FLAGS(HERD)) CALL TNOUA(', i',3)
      GOTO 230
220   IF (FLAGS(HERD)) CALL T1OU(200)
      IF (.NOT.FLAGS(HERD)) CALL T1OU(201)
230   IF (FLAGS(HERD)) CALL TNOUA('erds of ',8)
      IF (.NOT.FLAGS(HERD)) CALL TNOUA('solated ',8)
      CHARS=CHARS+11
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      LEN=LENGTH(COWS(1,COW),10)
      CALL TNOUA(COWS(1,COW),LEN)
      CALL T1OU(160)
      CHARS=CHARS+LEN+1
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('are roaming ',12)
      CHARS=CHARS+12
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('about. ',7)
      CHARS=CHARS+7
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
      IF (RANDOM(5).EQ.1) FLAGS(CARNIV)=.TRUE.
      IF (RANDOM(8).EQ.3) FLAGS(CARNIV)=.FALSE.
      IF (FLAGS(UNDEAD)) FLAGS(CARNIV)=.FALSE.   ! No cats if undead
      IF (RANDOM(4).EQ.2) FLAGS(ANIMAL)=.FALSE.       ! Animals disappear
      GOTO 260
C*
240   IF (FLAGS(ROLING)) GOTO 260
      CALL TNOUA(', ',2)
      CHARS=CHARS+2
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('the grass is very ',18)
      CHARS=CHARS+18
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('high here',9)
      CHARS=CHARS+9
      IF (.NOT.FLAGS(CARNIV)) GOTO 250
      CALL T1OU(160)
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      IF (CHARS.GT.0) CALL T1OU(160)
      CALL TNOUA('and you can ',12)
      CHARS=CHARS+13
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('hear large ',11)
      CHARS=CHARS+11
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('animals moving ',15)
      CHARS=CHARS+15
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('about',5)
      CHARS=CHARS+5
250   CALL TNOUA('. ',2)
      CHARS=CHARS+2
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
260   IF (.NOT.FLAGS(HOME)) GOTO 270
      WAYS(UP)=.FALSE.
      WAYS(DOWN)=.FALSE.
      CALL TNOUA('You are at base ',16)
      CHARS=CHARS+16
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('camp, your tent ',16)
      CHARS=CHARS+16
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOU('stands close by.',16)
      CHARS=0
      IF (MOVENO.EQ.1) GOTO 450
C*
C*    Text for hole in ground
C*
270   IF (.NOT.WAYS(DOWN)) GOTO 290
      CALL TNOUA('There is a ',11)
      CHARS=CHARS+11
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('deep hole ',10)
      CHARS=CHARS+10
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('at your feet',12)
      CHARS=CHARS+12
      IF (.NOT.FLAGS(NOTUP)) GOTO 280
      CALL TNOUA(', ',2)
      CHARS=CHARS+2
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('if you go down it ',18)
      CHARS=CHARS+18
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('you won''t be able ',18)
      CHARS=CHARS+18
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('to get back up',14)
      CHARS=CHARS+14
280   CALL TNOUA('. ',2)
      CHARS=CHARS+2
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
290   IF(.NOT.FLAGS(TREE)) GOTO 300         ! Get next instruction
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('There is a tall ',16)
      CHARS=CHARS+16
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('tree standing ',14)
      CHARS=CHARS+14
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('nearby. ',8)
      CHARS=CHARS+8
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
C*    Attacks by carnivores
C*
300   IF (.NOT.FLAGS(CARNIV)) GOTO 450
      IF (.NOT.FLAGS(HIGRAS)) GOTO 320
      IF (.NOT.FLAGS(SEEN)) GOTO 450
      IF (RANDOM(5).NE.1) GOTO 450     ! No attack
      IF (CHARS.NE.0) CALL TONL   ! Terminate previous text line
      CALL TONL
      CHARS=0
      CALL TNOUA('A ',2)
      LEN=LENGTH(CATS(1,CAT),10)
      CALL TNOUA(CATS(1,CAT),LEN)
      CALL T1OU(160)
      CHARS=CHARS+LEN+3
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('leaps at you ',13)
      CHARS=CHARS+13
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('from amongst ',13)
      CHARS=CHARS+13
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('the long grass. ',16)
      CHARS=CHARS+16
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      IF (VALUES(LUCK).GT.20) GOTO 310
      IF (CAT.EQ.4) GOTO 310      !  Lynx
C*
      CALL TONL
      CALL TNOU('It gets you!!!',14)
      CALL DEAD(FLAGS,VALUES(LUCK))
      GOTO 130
C*
310   CALL TNOUA('You avoid it ',13)
      CHARS=CHARS+13
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('adroitly and ',13)
      CHARS=CHARS+13
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOU('it slinks away.',15)
      CHARS=0
      VALUES(LUCK)=VALUES(LUCK)-5
      CALL LUCKY(VALUES(LUCK))
      FLAGS(CARNIV)=.FALSE.
      GOTO 450
C*
C*    Carnivore in open country
C*
320   IF (RANDOM(5).NE.2) GOTO 450
      FLAGS(SEEN)=.TRUE.
      CALL TNOUA('A ',2)
      LEN=LENGTH(CATS(1,CAT),10)
      CALL TNOUA(CATS(1,CAT),LEN)
      CALL T1OU(160)
      CHARS=CHARS+LEN+3
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('eyes you hungrily ',18)
      CHARS=CHARS+18
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('from some ',10)
      CHARS=CHARS+10
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOU('long grass.',11)
      CHARS=0
      GOTO 450
C*
325   CALL TNOU(
     +'It is very dark, if you move about you are likely',49)
      CALL TNOU(
     +'to fall into a pit.',19)
      GOTO 450
C*
327   CALL TNOU(
     +'Stumbling about in the dark, you have fallen into a deep',56)
      CALL TNOU('hole and broken your neck!',26)
      GOTO 337
C*
C*    *********************************
C*    *                               *
C*    *  End of ground level section  *
C*    *                               *
C*    *********************************
C*
330   IF (Z.GT.0) GOTO 350        !  Below ground
C*
C*    *************
C*    *           *
C*    *  Up tree  *
C*    *           *
C*    *************
C*
      WAYS(DOWN)=.TRUE.
      IF (VALUES(TIME).EQ.1) FLAGS(DARK)=.FALSE.
      IF (FLAGS(DARK)) GOTO 325
      IF (FLAGS(NIGHT)) GOTO 345
      IF (IFLAG.GT.0) CALL TONL
      CALL TNOU(
     +'You are at the top of the tree, from here you can see miles',59)
      CALL TNOU(
     +'in every direction. The mountains are still a long way off',58)
      CALL TNOU(
     +'and there is no chance of reaching them before nightfall.',57)
C*
335   IF (.NOT.FLAGS(CARNIV)) GOTO 450
      IF (.NOT.FLAGS(SEEN)) GOTO 450
      CALL TONL
      IF (CAT.NE.1) GOTO 340      !  Not leopard
      CALL TNOU(
     +'A leopard climbs up the tree after you. You try to escape',57)
      CALL TNOU(
     +'by crawling out along a branch but it is rotten and snaps.',58)
      CALL TNOU(
     +'You fall to the ground and break your neck!',43)
337   CALL DEAD(FLAGS,VALUES(LUCK))
      GOTO 130
C*
340   FLAGS(CARNIV)=.FALSE.
      SCORE=SCORE+1     !  For avoiding CAT
      CALL TNOUA('The ',4)
      CALL TNOUA(CATS(1,CAT),LENGTH(CATS(1,CAT),10))
      CALL TNOU(
     +' prowls by the foot of the tree without',39)
      CALL TNOU(
     +'seeing you. It eventually wanders off.',38)
      GOTO 450
C*
C*    Up tree at night with lamp on.
C*
345   CALL TNOU(
     +'You are at the top of the tree but your lamp is not powerful',60)
      CALL TNOUA(
     +'enough for you to see anything except your immediate ',53)
      CALL TNOU(
     +'surroundings.',13)
      CHARS=0
      GOTO 335
C*
C*    End of the up the tree section.
C*
C*    **************************************
C*    *                                    *
C*    *      We are now below ground       *
C*    *                                    *
C*    **************************************
C*
C*    Get the possible move directions
C*
C*
350   WAYNUM=0
      WAYTOT=0
      DO 360 I=1,10
         XNEXT=X+XINC(I)
         YNEXT=Y+YINC(I)
         ZNEXT=Z+ZINC(I)
         CALL LOCATE(XNEXT,YNEXT,ZNEXT,THERE)
         OK=WAYBIT(I)
         IF (AND(HERE,THERE,OK).EQ.OK) WAYS(I)=.TRUE.
         IF (WAYS(I)) WAYTOT=WAYTOT+1
         IF (I.GT.8) GOTO 360
         IF (WAYS(I)) WAYNUM=WAYNUM+1
360   CONTINUE
C*
C*    Now get the word pointers
C*
      IF (WAYS(DOWN).AND.FLAGS(DARK)) GOTO 327        !  Fallen down pit.
      IF (FLAGS(DARK)) GOTO 325        !  Can't see anything
      IF (Z.GT.1.AND.MOD(HERE,200).LE.10) FLAGS(DRIP)=.TRUE.    ! Water drips
      PROD=X*Y*Z
      TENBIT=AND(PROD,1023)
      WORD2=INT(SQRT(FLOAT(TENBIT/4.0)))+1
      WORD3=INT(SQRT(FLOAT(TENBIT/8.0)))+1
      WORD4=INT(SQRT(FLOAT(AND(PROD,255))))+1
C*
      FLAGS(SMALL)=.FALSE.
      FLAGS(LARGE)=.FALSE.
      FLAGS(SWITCH)=.FALSE.
C*
      IF (AND(HERE,512).EQ.512) FLAGS(SWITCH)=.TRUE.
      IF (WORD2.EQ.5.OR.WORD2.EQ.7.OR.WORD2.EQ.9.OR.
     +WORD2.EQ.11.OR.WORD2.EQ.13.OR.WORD2.EQ.15) FLAGS(SMALL)=.TRUE.
      IF (WORD2.EQ.6.OR.WORD2.EQ.8.OR.WORD2.EQ.10.OR.
     +WORD2.EQ.12.OR.WORD2.EQ.14.OR.WORD2.EQ.16) FLAGS(LARGE)=.TRUE.
C*
      WORD1=1           ! Default value of WORD1
C*
      FLAGS(SHAFT)=.FALSE.
      IF (WAYS(UP).AND.WAYS(DOWN)) FLAGS(SHAFT)=.TRUE.     ! Shafts
      IF (FLAGS(SHAFT)) GOTO 410
      IF (WORD4.LT.5) GOTO 370    ! Cells
      IF (WORD4.EQ.9.OR.WORD4.EQ.11.OR.WORD4.EQ.13.OR.
     +WORD4.EQ.15) GOTO 380       ! Passages
      IF (WORD4.EQ.14.OR.WORD4.EQ.12.OR.WORD4.EQ.14.OR.
     +WORD4.EQ.16) GOTO 390       ! Rooms
      IF (WORD4.EQ.5) GOTO 400    ! Alcove
C*
C*    Caves
C*
      IF (FLAGS(SMALL).AND.FLAGS(SWITCH)) WORD1=2
      IF (FLAGS(SMALL).AND.(.NOT.FLAGS(SWITCH))) WORD1=6
      IF (FLAGS(LARGE).AND.FLAGS(SWITCH)) WORD1=3
      GOTO 420
C*
C*    Cells
C*
370   IF (FLAGS(SMALL).AND.FLAGS(SWITCH)) WORD1=2
      IF (FLAGS(SMALL).AND.(.NOT.FLAGS(SWITCH))) WORD1=5
      GOTO 420
C*
C*    Passages
C*
380   WORD1=3
      IF (WAYTOT.LT.2) WORD4=WORD4+1
      IF (FLAGS(SMALL).AND.FLAGS(SWITCH)) WORD1=5
      IF (FLAGS(SMALL).AND.(.NOT.FLAGS(SWITCH))) WORD1=6
      IF (FLAGS(LARGE).AND.FLAGS(SWITCH)) WORD1=4
      GOTO 420
C*
C*    Rooms
C*
390   IF (FLAGS(SMALL).AND.FLAGS(SWITCH)) WORD1=5
      IF (FLAGS(LARGE).AND.FLAGS(SWITCH)) WORD1=3
      GOTO 420
C*
C*    Alcove
C*
400   IF (FLAGS(SMALL)) WORD1=2
      GOTO 420
C*
C*    Shafts
C*
410   WORD2=MOD(TENBIT,6)+11
C***      WORD3=MOD(TENBIT,5)+8
      WORD4=17
      IF (FLAGS(SWITCH)) WORD4=18
      CALL TNOUA('You are climbing ',17)
      LEN=LENGTH(ADJONE(1,WORD2),20)
      CALL TNOUA(ADJONE(1,WORD2),LEN)
      CALL T1OU(160)
      CHARS=18+LEN
      LEN=LENGTH(NOUNS(1,WORD4),20)
      CALL TNOUA(NOUNS(1,WORD4),LEN)
      CHARS=CHARS+LEN
      IF (FLAGS(SWITCH)) CALL TNOUA('. Stairs ',9)
      IF (.NOT.FLAGS(SWITCH)) CALL TNOUA('. Steps ',8)
      IF (FLAGS(SMALL)) CALL TNOUA('go ',3)
      IF (.NOT.FLAGS(SMALL)) CALL TNOUA('lead ',5)
      CHARS=CHARS+LEN+14
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('up and ',7)
      CHARS=CHARS+7
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('down from ',10)
      CHARS=CHARS+10
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('here. ',6)
      CHARS=CHARS+6
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      GOTO 425
C*
420   SUMXYZ=X+Y+Z
      IF (MOD(SUMXYZ,100).LE.10) FLAGS(GLOW)=.TRUE.
      IF (FLAGS(SHAFT)) FLAGS(GLOW)=.FALSE.      ! No glow in shafts
      FLAGS(WAVER)=FLAGS(GLOW).AND.(FLAGS(WAVER).OR.RANDOM(4).EQ.2)
      FLAGS(DARK)=.TRUE.
      IF (FLAGS(GLOW)) FLAGS(DARK)=.FALSE.
      IF (FLAGS(LAMPON)) FLAGS(DARK)=.FALSE.
      IF (FLAGS(UNDEAD)) FLAGS(DARK)=.FALSE.     ! Undead can always see
      IF (FLAGS(GLOW)) CALL TNOUA(
     +'The whole scene is bathed in an eerie',37)
      IF (FLAGS(WAVER)) CALL TNOUA(', wavering',10)
      IF (FLAGS(GLOW)) CALL TNOU(' glow.',6)
C*
      CALL TNOUA('You are ',8)
      CHARS=8
C*
      LEN=LENGTH(VERBS(1,WORD1),20)
      CALL TNOUA(VERBS(1,WORD1),LEN)
      CALL T1OU(160)
      CHARS=CHARS+LEN
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
      LEN=LENGTH(ADJONE(1,WORD2),20)
      CALL TNOUA(ADJONE(1,WORD2),LEN)
      CHARS=CHARS+LEN
      LEN=LENGTH(ADJTWO(1,WORD3),20)
      CALL TNOUA(ADJTWO(1,WORD3),LEN)
      CALL T1OU(160)
      CHARS=CHARS+LEN+1
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
      LEN=LENGTH(NOUNS(1,WORD4),20)
      CALL TNOUA(NOUNS(1,WORD4),LEN)
      CALL TNOUA('. ',2)
      CHARS=CHARS+LEN+2
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
C*    Now calculate and display where you can go next.
C*    On the level first, stairs later.
C*
425   COUNT=1
      DO 430 I=1,8
         IF (.NOT.WAYS(I)) GOTO 430    ! Not this way
         XNEXT=IABS(X+XINC(I))
         YNEXT=IABS(Y+YINC(I))
         ZNEXT=IABS(Z+ZINC(I))
         CALL LOCATE(XNEXT,YNEXT,ZNEXT,THERE)
C*
         PROD=XNEXT*YNEXT*ZNEXT
         TENBIT=AND(PROD,1023)
         WORD5=INT(SQRT(FLOAT(TENBIT/4.0)))+1
         WORD6=INT(SQRT(FLOAT(AND(PROD,255))))+1
         IF (COUNT.GT.1) GOTO 427
C*
         CALL TNOUA('There is ',9)
         CHARS=CHARS+9
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
427      LEN=LENGTH(ADJONE(1,WORD5),20)
         CALL TNOUA(ADJONE(1,WORD5),LEN)
         CALL T1OU(160)
         CHARS=CHARS+LEN+1
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
         LEN=LENGTH(NOUNS(1,WORD6),20)
         CALL TNOUA(NOUNS(1,WORD6),LEN)
         CHARS=CHARS+LEN
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
         IF (CHARS.GT.0) CALL T1OU(160)
         CALL TNOUA('to the ',7)
         CHARS=CHARS+8
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
         LEN=LENGTH(ROUTES(1,I),10)
         CALL TNOUA(ROUTES(1,I),LEN)
         CHARS=CHARS+LEN
C*
         IF (COUNT.EQ.WAYNUM) GOTO 440
         WAYLES=WAYNUM-1
         IF (COUNT.LT.WAYLES) CALL TNOUA(', ',2)
         IF (COUNT.LT.WAYLES) CHARS=CHARS+2
         IF (COUNT.EQ.WAYLES.AND.CHARS.GT.0) CALL T1OU(160)
         IF (COUNT.EQ.WAYLES) CALL TNOUA('and ',4)
         IF (COUNT.EQ.WAYLES) CHARS=CHARS+5
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
         COUNT=COUNT+1
430   CONTINUE
C*
440   IF (WAYNUM.GT.0) CALL TNOUA('. ',2)
      IF (FLAGS(SHAFT)) GOTO 450       ! In shaft, text done.
C*
C*    Now for stairs and suchlike.
C*
      IF (WAYS(UP)) GOTO 441      !  Stairs up.
      IF (WAYS(DOWN)) GOTO 441         !  Stairs down.
      GOTO 450          ! No way up or down.
C*
441   IF (CHARS.GE.58) CALL LNFEED(CHARS)
      IF (FLAGS(SMALL).OR.FLAGS(LARGE)) GOTO 442
      CALL TNOUA('A steep ladder',14)
      FLAGS(SWITCH)=.TRUE.
      CHARS=CHARS+14
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      GOTO 445
C*
442   IF (CHARS.GE.58) CALL LNFEED(CHARS)
      IF (FLAGS(SMALL).AND.FLAGS(SWITCH)) CALL TNOUA('A winding stair'
     +,15)
      IF (FLAGS(SMALL).AND.(.NOT.FLAGS(SWITCH))) CALL TNOUA(
     +'Rickety steps',13)
      IF (FLAGS(LARGE).AND.FLAGS(SWITCH)) CALL TNOUA('A spiral stair',
     +14)
      IF (FLAGS(LARGE).AND.(.NOT.FLAGS(SWITCH))) CALL TNOUA(
     +'Broad stairs',12)
      IF (FLAGS(SWITCH).AND.AND(PROD,1024).EQ.1024) CALL TNOUA('case',
     +4)
      CHARS=CHARS+19
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
445   FLAGS(LEAD)=.FALSE.
      IF (AND(TENBIT,256).EQ.256) FLAGS(LEAD)=.TRUE.
      IF (CHARS.GT.0) CALL T1OU(160)
      IF (FLAGS(LEAD)) CALL TNOUA('lead',4)
      IF (.NOT.FLAGS(LEAD)) CALL TNOUA('disappear',9)
      IF (FLAGS(SWITCH)) CALL TNOUA('s',1)
      CHARS=CHARS+11
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      IF (CHARS.NE.0) CALL T1OU(160)
      IF (WAYS(UP)) CALL TNOUA('up',2)
      IF (WAYS(DOWN)) CALL TNOUA('down',4)
      IF (AND(PROD,2048).EQ.2048) CALL TNOUA('wards',5)
      CALL TNOUA('. ',2)
      CHARS=CHARS+12
C*
C*    Display water drips
C*
450   IF (FLAGS(DARK)) GOTO 4500
      IF (.NOT.FLAGS(DRIP)) GOTO 454
      VALUES(POOL)=MOD(X,3)
      IF (FLAGS(SHAFT)) VALUES(POOL)=0
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('Water drips ',12)
      CHARS=CHARS+12
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
      CALL TNOUA('from above',10)
      CHARS=CHARS+10
C*
      IF (VALUES(POOL).EQ.1) GOTO 4050
      IF (VALUES(POOL).EQ.2) GOTO 4055
      CALL TNOUA('. ',2)
      CHARS=CHARS+2
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      GOTO 454
C*
4050  CALL TNOUA(', ',2)
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
      CALL TNOUA('disappearing ',13)
      CHARS=CHARS+15
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
      CALL TNOUA('into small fissures ',20)
      CHARS=CHARS+20
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
      CALL TNOUA('in the floor. ',14)
      CHARS=CHARS+14
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      GOTO 454
C*
4055  CALL TNOUA(', ',2)
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
      CALL TNOUA('collecting ',11)
      CHARS=CHARS+13
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
      CALL TNOUA('in a small pool ',16)
      CHARS=CHARS+16
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
      CALL TNOUA('at your feet. ',14)
      CHARS=CHARS+14
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
C*    *************************************
C*    *                                   *
C*    *   Show objects at this location.  *
C*    *                                   *
C*    *************************************
C*
C*    Scan all objects to see which are HERE and convert any
C*    such virtual objects (except plants) into real objects.
C*    Set them to OBHERE(n) and count them.
C*
454   IF (FLAGS(INTENT)) GOTO 4550
      IF (FLAGS(SHAFT)) GOTO 4580      ! No objects in shafts.
      SEEIT=0
      MOD20=MOD(HERE,20)
      MOD60=MOD(HERE,60)
      DO 4520 I=1,H$OBJ
         OBHERE(I)=.FALSE.
         IF (OBJLOC(I).EQ.0) GOTO 4520      ! Carried by player
         IF (OBJLOC(I).EQ.HERE) GOTO 4515   ! It was put here by player
         IF (I.GT.10.AND.Z.LE.1) GOTO 4520  ! Can't see it here
         POSLOC=-OBJLOC(I)
         IF (I.GT.10) GOTO 4510   ! Objects 11-H$OBJ seen 601
         IF (Z.GT.1) GOTO 4510    ! All seen 601 below ground
         IF (MOD20.EQ.POSLOC) GOTO 4515     ! It is here (201)
         GOTO 4520      ! Not here
4510     IF (MOD60.EQ.POSLOC) GOTO 4515          !  It is here (601)
         GOTO 4520           ! Not here
C*
4515     SEEIT=SEEIT+1
         OBHERE(I)=.TRUE.
         IF (I.EQ.4) GOTO 4520    ! Plant remains virtual until picked
         OBJLOC(I)=HERE
         ALLDUN(I)=.FALSE.
4520  CONTINUE
C*
C*    If any objects are here set up an array of pseudo random
C*    unique pointers, seeded with HERE.
C*
      IF (SEEIT.EQ.0) GOTO 4580   ! Nothing here
      CALL POINTR(HERE,IPT,H$OBJ)
C*
C*    Display the objects in pseudo random order.
C*
4522  KOUNT=0
      DO 4540 I=1,H$OBJ
         PTR=IPT(I)
         IF (.NOT.OBHERE(PTR)) GOTO 4540    ! Not here
         KOUNT=KOUNT+1
         IF (KOUNT.GT.1) GOTO 4525
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
         CALL TNOUA('Here you can see ',17)
         CHARS=CHARS+17
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
4525     LEN=LENGTH(THINGS(1,PTR),20)
         CALL TNOUA(THINGS(1,PTR),LEN)
         CALL T1OU(160)
         CHARS=CHARS+LEN+1
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
         LEN=LENGTH(THINGS(11,PTR),20)
         CALL TNOUA(THINGS(11,PTR),LEN)
         CHARS=CHARS+LEN
C*
         IF (KOUNT.EQ.SEEIT) GOTO 4545      ! All done
         IF ((KOUNT+1).EQ.SEEIT) GOTO 4530       ! 'and'
         CALL TNOUA(', ',2)
         CHARS=CHARS+2
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
         GOTO 4540
C*
4530     IF (CHARS.GT.0) CALL T1OU(160)
         CALL TNOUA('and ',4)
         CHARS=CHARS+4
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
4540  CONTINUE
4545  CALL TNOUA('. ',2)
      GOTO 4580
C*
C*    *******************************************
C*    *                                         *
C*    *  In tent, display objects differently.  *
C*    *                                         *
C*    *******************************************
C*
4550  CHARS=44
      DO 4570 K=1,H$OBJ
         OBHERE(K)=.FALSE.
         IF (.NOT.ALLDUN(K)) GOTO 4570
         CALL TNOUA(', ',2)
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
         IF (K.EQ.9) GOTO 4560    ! Garlic, no 'a'.
         IF (K.EQ.5.OR.K.EQ.13) GOTO 4560   ! Keys, coins.
         IF (K.EQ.25) GOTO 4560   ! Violets
         IF (K.EQ.34) GOTO 4560   ! Treasure
         IF (K.EQ.38) GOTO 4560   ! Oyster
         IF (THINGS(1,K).EQ.'a ') CALL TNOUA('a',1)
         IF (THINGS(1,K).EQ.'an') CALL TNOUA('a',1)
         IF (THINGS(1,K).EQ.'so') CALL TNOUA('some',4)
4560     IF (K.EQ.38) CALL TNOUA('an',2)    ! Oyster
         IF (K.EQ.5.OR.K.EQ.13) CALL TNOUA('some',4)  ! Keys,coins
         IF (K.EQ.25) CALL TNOUA('some',4)  ! Violets
         IF (K.EQ.40) CALL TNOUA('the',3)
         CALL T1OU(160)     ! ' '
         LEN=LENGTH(THINGS(11,K),20)
         CALL TNOUA(THINGS(11,K),LEN)
         CHARS=CHARS+LEN+5
         OBHERE(K)=.TRUE.
4570  CONTINUE
      CALL TNOUA(', etc. ',7)
C*
C*    Dim lamp.
C*
4580  IF (FLAGS(UNDEAD)) GOTO 4584
      IF (.NOT.FLAGS(LAMPON)) GOTO 4584
      IFLAG=0
      IF (VALUES(LIGHT).LT.20.AND.MOD(VALUES(LIGHT),4).EQ.3)
     +IFLAG=1
      IF (IFLAG.EQ.1) CALL TONL
      IF (IFLAG.EQ.1) CALL TNOUA('Your lamp is getting dim. ',26)
      IF (VALUES(LIGHT).GT.0) GOTO 4582
      FLAGS(LAMPON)=.FALSE.
      CALL TONL
      CALL TNOUA(
     +'Your lamp flickers and goes out! ',33)
      VALUES(FLICK)=VALUES(FLICK)+1
C*
C*    PLUGH
C*
4582  IF (Z.LE.1) GOTO 4584
      IF (MOD(HERE,50).NE.23.OR.RANDOM(3).NE.1) GOTO 4584
      CALL TONL
      CALL TNOU(
     +'A hollow voice says "Plugh!"',28)
C*
4584  IF (VALUES(BRKBOT).EQ.HERE) GOTO 4590
      IF (VALUES(BRKVAS).EQ.HERE) GOTO 4590
      IF (VALUES(BRKGOB).EQ.HERE) GOTO 4590
      IF (VALUES(BRKMIR).EQ.HERE) GOTO 4590
      GOTO 4591
4590  CALL TONL
      CALL TNOU(
     +'The ground is littered with tiny fragments of glass.',52)
C*
C*    Display THIRST message as appropriate
C*
4591  IF (VALUES(THIRST).LT.100) GOTO 8000
      IF (FLAGS(UNDEAD)) GOTO 8000
      IF (RANDOM(6).NE.4) GOTO 8000
      CALL TONL
      CALL TNOUA('You feel very thirsty',21)
      IF (RANDOM(2).EQ.0) CALL TNOUA(
     +', maybe a little drink will bring you luck',42)
      CALL TNOU('. ',2)
C*
C*    *******************************************
C*    *                                         *
C*    *   See if any monsters start this time.  *
C*    *                                         *
C*    *******************************************
C*
8000  IF (FLAGS(UNDEAD)) GOTO 4500     ! No monsters if undead
      IF (Z.LE.1) GOTO 8100  ! Above ground, none start.
      IF (FLAGS(DARK)) GOTO 8100  ! None start in the dark
      IF (VALUES(MONCNT).GT.2) GOTO 8100    ! Not more than 3 at once.
C*
8010  IF (.NOT.FLAGS(NIGHT).OR.MONSTR(VAMPIR).GE.0) GOTO 8020
      IF (RANDOM(50).EQ.23) MONSTR(VAMPIR)=IABS(MONSTR(VAMPIR))
      IF (MONSTR(VAMPIR).GT.0) GOTO 8090
C*
8020  IF (.NOT.FLAGS(LARGE).OR.MONSTR(DRAGON).GE.0) GOTO 8030
      IF (RANDOM(10).EQ.6) MONSTR(DRAGON)=IABS(MONSTR(DRAGON))
      IF (MONSTR(DRAGON).EQ.1.AND.RANDOM(3).NE.1)
     +MONSTR(DRAGON)=-1      ! Gives 1/30 chance of first encounter
      IF (MONSTR(DRAGON).GT.0) GOTO 8090
C*
8030  IF (.NOT.FLAGS(GLOW).OR.MONSTR(TROLL).GE.0) GOTO 8040
      IF (RANDOM(40).EQ.23) MONSTR(TROLL)=IABS(MONSTR(TROLL))
      IF (MONSTR(TROLL).GT.0) GOTO 8090
C*
8040  IF (MONSTR(ELF).GE.0) GOTO 8050
      IF (RANDOM(20).EQ.7) MONSTR(ELF)=IABS(MONSTR(ELF))
      IF (MONSTR(ELF).GT.0) GOTO 8090
C*
8050  IF (MONSTR(GORGON).GT.0) MONSTR(GORGON)=-MONSTR(GORGON)
      IF (MONSTR(GORGON).EQ.0) GOTO 8060    ! Gorgon dead
      IF (FLAGS(LARGE)) GOTO 8060      ! Not here
      IF (VALUES(GORLOC).GT.0) GOTO 8055    ! Got location already
      IF (MOD(HERE,100).EQ.87) VALUES(GORLOC)=HERE
8055  IF (VALUES(GORLOC).NE.HERE) GOTO 8060      ! Not here
      MONSTR(GORGON)=IABS(MONSTR(GORGON))
      GOTO 8090
C*
8060  IF (MONSTR(SNAKE).GE.0) GOTO 8070
      IF (RANDOM(50).EQ.42) MONSTR(SNAKE)=IABS(MONSTR(SNAKE))
      IF (MONSTR(SNAKE).GT.0) GOTO 8090
C*
8070  IF (VALUES(DWFNUM).EQ.0) GOTO 8080    ! No dwarves left
      IF (RANDOM(20).EQ.9) MONSTR(DWARF)=IABS(MONSTR(DWARF))
      IF (MONSTR(DWARF).GT.0) GOTO 8090
C*
8080  IF (FLAGS(SMALL).OR.FLAGS(GLOW).OR.MONSTR(BATS).GE.0) GOTO 8100
      IF (RANDOM(20).EQ.7) MONSTR(BATS)=IABS(MONSTR(BATS))
      IF (MONSTR(BATS).LT.0) GOTO 8100
C*
8090  VALUES(MONCNT)=VALUES(MONCNT)+1
C*
C*
C*    *********************************
C*    *                               *
C*    *  Display all active monsters  *
C*    *                               *
C*    *********************************
C*
C*    Vampire
C*
8100  IF (MONSTR(VAMPIR).LE.0) GOTO 8200
      IF (.NOT.FLAGS(NIGHT)) MONSTR(VAMPIR)=-IABS(MONSTR(VAMPIR))
      IF (MONSTR(VAMPIR).GT.0) GOTO 8110
      VALUES(MONCNT)=VALUES(MONCNT)-1
      GOTO 8200
C*
8110  CALL TONL
      IF (MONSTR(VAMPIR).EQ.2) GOTO 8120
      IF (MONSTR(VAMPIR).EQ.3) GOTO 8130
      IF (MONSTR(VAMPIR).EQ.4) GOTO 8140
      IF (MONSTR(VAMPIR).EQ.5) GOTO 8150
      IF (MONSTR(VAMPIR).GT.6) GOTO 8160
      CALL TNOU(
     +'A cadaverous man with red eyes sidles up behind you.',52)
      MONSTR(VAMPIR)=2
      GOTO 8190
C*
8120  CALL TNOU(
     +'The red-eyed man is close behind you.',37)
      IF (RANDOM(4).EQ.2) MONSTR(VAMPIR)=3
      GOTO 8190
C*
8130  CALL TNOU(
     +'You turn to confront your follower, his red eyes gaze',53)
      CALL TNOU(
     +'into yours and your head swims. With a great effort of',54)
      CALL TNOU(
     +'will you recover your senses.',29)
      MONSTR(VAMPIR)=4-RANDOM(3)
      GOTO 8190
C*
8140  CALL TNOU(
     +'The cadaverous man leaps from behind and tries to bite',54)
      CALL TNOUA('your throat',11)
      IF (GONE(O$GARL)) GOTO 8180 ! Eaten garlic
      RAND=RANDOM(3)
      IF (RAND.EQ.2) GOTO 8170    ! He gets you
8145  CALL TNOU(', you break free just in time.',30)
      IF (RAND.EQ.0) MONSTR(VAMPIR)=5
      GOTO 8190
C*
8150  CALL TNOU(
     +'The corpse-like man has disappeared but a large bat',51)
      CALL TNOU('hovers nearby.',14)
      IF (RANDOM(4).EQ.2) MONSTR(VAMPIR)=6
      GOTO 8190
C*
8160  CALL TNOUA('A large bat swoops down',23)
      IF (GONE(O$GARL)) GOTO 8180      ! Eaten garlic
      RAND=RANDOM(3)
      IF (RAND.EQ.0) GOTO 8145    !  Break free
      IF (RAND.EQ.1) GOTO 8165    ! Bites you
      CALL TNOU('.',1)
      MONSTR(VAMPIR)=5+RANDOM(2)
      GOTO 8190
C*
8165  CALL TNOU(' and bites your throat.',23)
      GOTO 8175
C*
8170  CALL TNOU('.',1)
8175  CALL TONL
      CALL TNOU(
     +'Cold talons grasp you and you feel his sweet, foul breath',57)
      CALL TNOU(
     +'as he draws the life-blood from you.',36)
      CALL TONL
      CALL SLEEP$(DELAY)
      CALL TNOU(
     +'You pass out for a while, when you recover you see things',57)
      CALL TNOU(
     +'but dimly and you have a raging thirst which cannot be',54)
      CALL TNOU(
     +'satisfied. Material things no longer have any interest',54)
      CALL TNOU('for you.',8)
      SCORE=0
      FLAGS(UNDEAD)=.TRUE.
      DO 8176 I=1,H$MON
         MONSTR(I)=0    ! Deactivate all monsters
8176  CONTINUE
      DO 8177 I=1,H$OBJ
         IF (OBJLOC(I).EQ.0) OBJLOC(I)=HERE ! Drop everything
8177  CONTINUE
      VALUES(HELD)=0
      VALUES(DWFNUM)=0
      VALUES(DWFNOW)=0
      VALUES(MONCNT)=0
      GOTO 4500
C*
8180  CALL TNOU(' but is repelled by the smell of',32)
      CALL TNOU(
     +'your breath and vanishes in a cloud of dust specks which',56)
      CALL TNOU(
     +'dance before your eyes and slowly disappear.',44)
C*
C*    Kill the vampire off and award the player 50 points.
C*
      MONSTR(VAMPIR)=0
      SCORE=SCORE+50
      VALUES(MONCNT)=VALUES(MONCNT)-1
      GOTO 8200
C*
8190  IF (RANDOM(20).EQ.13) MONSTR(VAMPIR)=-MONSTR(VAMPIR)
      IF (MONSTR(VAMPIR).EQ.-3) MONSTR(VAMPIR)=-2
      IF (MONSTR(VAMPIR).LT.0) VALUES(MONCNT)=VALUES(MONCNT)-1
C*
C*    Dragon
C*
8200  IF (MONSTR(DRAGON).LE.0) GOTO 8300    ! Inactive
      RAND=RANDOM(1)+2       !  Range 2 - 3
      IF (Z.LE.1) MONSTR(DRAGON)=-RAND      ! Not above ground
      IF (.NOT.FLAGS(LARGE)) MONSTR(DRAGON)=-RAND     ! Not here
      IF (FLAGS(SHAFT)) MONSTR(DRAGON)=-RAND          ! Not in shaft
      IF (MONSTR(DRAGON).GT.0) GOTO 8202
      VALUES(MONCNT)=VALUES(MONCNT)-1
      GOTO 8300
C*
8202  CALL TONL
      IF (MONSTR(DRAGON).EQ.2) GOTO 8204
      IF (MONSTR(DRAGON).EQ.3) GOTO 8206
      IF (MONSTR(DRAGON).GT.3) GOTO 8210
      CALL TNOU(
     +'A huge dragon lies before you. Feeling your presence',52)
      CALL TNOU(
     +'it heaves itself to its feet and prepares to attack.',52)
      GOTO 8230
8204  CALL TNOU(
     +'The dragon confronts you breathing fire and smoke.',50)
      CALL TNOU(
     +'You had better get out of here fast!',36)
      GOTO 8230
8206  CALL TNOU(
     +'The dragon thunders after you!',30)
      GOTO 8230
C*
8210  RAND=RANDOM(5)
      CALL TNOUA('The dragon breathes a blast of fire',35)
      IF (RAND.LE.1) GOTO 8220
      CALL TNOU(', you dodge just in time.',25)
      GOTO 8230
C*
8220  IF (RAND.EQ.1) GOTO 8224
      CALL TNOU(', you leap aside but are',24)
      CALL TNOU(
     +'laid low by its lashing tail. It devours you',44)
      CALL TNOU('in an instant!',14)
      GOTO 8226
8224  CALL TNOU(' which burns you to a cinder!',29)
8226  CALL DEAD(FLAGS,VALUES(LUCK))
      GOTO 130
C*
8230  MONSTR(DRAGON)=RANDOM(3)+2
C*
C*    Troll
C*
8300  IF (MONSTR(TROLL).LE.0) GOTO 8400     !  Inactive
      IF (.NOT.FLAGS(NIGHT).AND.Z.LE.1) GOTO 8390     ! Turn it to stone
      CALL TONL
      IF (MONSTR(TROLL).EQ.2) GOTO 8314
      IF (MONSTR(TROLL).EQ.3) GOTO 8316
      IF (MONSTR(TROLL).GT.3) GOTO 8320
      CALL TNOU(
     +'A large troll steps out of the shadows and lumbers',50)
      CALL TNOU('menacingly after you.',21)
      GOTO 8380
8314  CALL TNOU('The troll lumbers after you.',28)
      GOTO 8380
8316  CALL TNOU(
     +'The troll tries to corner you, but you manage',45)
      CALL TNOU('to avoid him.',13)
      GOTO 8380
C*
8320  IF (MONSTR(DWARF).GT.0) GOTO 8330
      CALL TNOU(
     +'The troll corners you, you try to escape but cannot',51)
      CALL TNOU(
     +'squeeze past him. Slowly he crushes you to a pulp!',50)
      GOTO 8340
8330  CALL TNOU(
     +'The troll lumbers towards you, you try to dodge but',51)
      CALL TNOU(
     +'trip over a little dwarf. The troll stamps both of',50)
      CALL TNOU('you to death!',13)
8340  CALL DEAD(FLAGS,VALUES(LUCK))
      GOTO 130
C*
8380  MONSTR(TROLL)=MONSTR(TROLL)+RANDOM(2)
      IF (RANDOM(4).EQ.2) MONSTR(TROLL)=2
      IF (RANDOM(15).EQ.6) MONSTR(TROLL)=1
      IF (RANDOM(6).EQ.4) MONSTR(TROLL)=-MONSTR(TROLL)
      IF (MONSTR(TROLL).LT.0) VALUES(MONCNT)=VALUES(MONCNT)-1
      GOTO 8400
C*
8390  CALL TONL
      CALL TNOU(
     +'The sunlight catches the troll, it gives a piercing',51)
      CALL TNOU(
     +'scream and tumbles to the ground as an inert lump',49)
      CALL TNOU('of rock.',8)
      MONSTR(TROLL)=0
      VALUES(MONCNT)=VALUES(MONCNT)-1
      SCORE=SCORE+50
C*
C*    Elf
C*
8400  IF (Z.LE.1) GOTO 8500  ! Elf underground only
      IF (MONSTR(ELF).LE.0) GOTO 8500  ! Inactive
      CALL TONL
      IF (MONSTR(ELF).GT.1) GOTO 8410
      CALL TNOU(
     +'A slender elf strolls past as if looking for something',54)
      CALL TNOU(
     +'and disappears round a corner.',30)
      GOTO 8440
C*
8410  DO 8420 K=1,H$OBJ
         IF (OBHERE(K)) GOTO 8430
8420  CONTINUE
      CALL TNOU(
     +'The elf appears, looks about and seeing nothing',47)
      CALL TNOU(
     +'to interest him wanders off.',28)
      GOTO 8440
C*
8430  CALL TNOU(
     +'The elf appears, says "Ha, just what I wanted!" and',51)
      CALL TNOUA('runs out, taking the ',21)
      CALL TNOUA(THINGS(11,K),LENGTH(THINGS(11,K),20))
      CALL TNOU(' with him.',10)
      OBHERE(K)=.FALSE.
8435  OBJLOC(K)=-RANDOM(60)
      POSLOC=-OBJLOC(K)
      IF (MOD(HERE,60).EQ.POSLOC) GOTO 8435 ! Not put down here!
C*
8440  MONSTR(ELF)=-2
      VALUES(MONCNT)=VALUES(MONCNT)-1
C*
C*    Gorgon
C*
8500  IF (MONSTR(GORGON).LE.0) GOTO 8600    !  Inactive
      IF (VALUES(GORLOC).NE.HERE) GOTO 8600 ! Not here
      CALL TONL
      IF (MONSTR(GORGON).GT.1) GOTO 8510
      CALL TNOU(
     +'A sleeping woman lies chained to a rock, her hair is a',54)
      CALL TNOU(
     +'seething mass of snakes. She begins to awake and slowly',55)
      CALL TNOU('turns towards you.',18)
      MONSTR(GORGON)=2
      GOTO 8600
C*
8510  IF (MONSTR(BATS).GT.0) GOTO 8520
      IF (MONSTR(DWARF).GT.0) GOTO 8530
      IF (MONSTR(SNAKE).GT.0) GOTO 8540
      IF (OBJLOC(O$MIRR).EQ.0) GOTO 8550    !  Got mirror
8515  CALL TNOU(
     +'The gorgon glares at you and you are immediately',48)
      CALL TNOU('turned into stone!!',19)
      CALL DEAD(FLAGS,VALUES(LUCK))
      GOTO 130
C*
8520  CALL TNOU(
     +'The gorgon glares at the bats wheeling overhead, they',53)
      CALL TNOU(
     +'instantly fall to the ground as a shower of brickbats.',54)
      MONSTR(BATS)=0
      OBJLOC(O$BRIC)=HERE
      ALLDUN(O$BRIC)=.FALSE.
      GONE(O$BRIC)=.FALSE.
      GOTO 8590
C*
8530  IF (OBJLOC(O$GNOM).GT.-2000) GOTO 8515
      IF (VALUES(DWFNOW).GT.1) GOTO 8534
      CALL TNOUA(
     +'The gorgon glares at the dwarf,',31)
      GOTO 8536
8534  CALL TNOU(
     +'The gorgon glares at one of the dwarves,',40)
8536  CALL TNOU(
     +' turning it into a pottery gnome.',33)
      VALUES(DWFNOW)=VALUES(DWFNOW)-1
      OBJLOC(O$GNOM)=HERE
      ALLDUN(O$GNOM)=.FALSE.
      GONE(O$GNOM)=.FALSE.
      IF (VALUES(DWFNOW).GT.0) GOTO 8600
      MONSTR(DWARF)=-1
      GOTO 8590
C*
8540  CALL TNOU(
     +'The gorgon stares hard at the snake, which slowly',49)
      CALL TNOU('turns into a block of serpentine.',33)
      MONSTR(SNAKE)=0
      OBJLOC(O$SERP)=HERE
      ALLDUN(O$SERP)=.FALSE.
      GONE(O$SERP)=.FALSE.
      GOTO 8590
C*
8550  CALL TNOU(
     +'The gorgon catches her own gaze in the mirror and',49)
      CALL TNOU(
     +'is instantly transformed into a statue of the',45)
      CALL TNOU('Venus de Milo!',14)
      VALUES(GORLOC)=-1
      MONSTR(GORGON)=0
      SCORE=SCORE+100
      OBJLOC(O$VENU)=HERE
      ALLDUN(O$VENU)=.FALSE.
      GONE(O$VENU)=.FALSE.
8590  VALUES(MONCNT)=VALUES(MONCNT)-1
C*
C*    Snake
C*
8600  IF (MONSTR(VAMPIR).GT.0) GOTO 8900    !  Snake, dwarves &
      IF (MONSTR(DRAGON).GT.0) GOTO 8900    !  bats avoid these.
      IF (MONSTR(SNAKE).LE.0) GOTO 8700     !  Inactive
      CALL TONL
      IF (MONSTR(SNAKE).EQ.2) GOTO 8610
      IF (MONSTR(SNAKE).EQ.3) GOTO 8620
      IF (MONSTR(SNAKE).EQ.4) GOTO 8630
      IF (MONSTR(SNAKE).GT.4) GOTO 8640
      CALL TNOU(
     +'An enormous snake appears and hisses angrily at you.',52)
      GOTO 8680
C*
8610  CALL TNOU(
     +'You are being followed by a large green snake.',46)
      GOTO 8680
8620  CALL TNOU(
     +'The snake is getting close and is trying to hypnotise you,',58)
      CALL TNOU(
     +'I don''t give much for your chances if it succeeds.',50)
      GOTO 8680
8630  CALL TNOU(
     +'The snake strikes at you but you leap back just in time.',56)
      GOTO 8680
C*
8640  CALL TNOU(
     +'The snake suddenly strikes at you, you spring back but two',58)
      CALL TNOU(
     +'small marks on your arm show where its poison was injected.',59)
      IF (OBJLOC(O$CHAR).NE.0) GOTO 8650    ! Not got charm
      CALL TNOU(
     +'By good luck there seem to be no ill effects - this time!',57)
      IF (.NOT.FLAGS(ELIXIR)) VALUES(LUCK)=VALUES(LUCK)-10
      MONSTR(SNAKE)=-2
      GOTO 8680
C*
8650  CALL SLEEP$(DELAY)
      CALL TNOU(
     +'Your arm swells up and goes black, you feel muzzy from the',58)
      CALL TNOU('effects of the poison!!',23)
      IF (.NOT.FLAGS(ELIXIR)) GOTO 8660
      CALL TONL
      CALL SLEEP$(DELAY)
      CALL TNOU(
     +'After a while your head clears and within a few',47)
      CALL TNOU(
     +'minutes the swelling has gone down and your arm is',50)
      CALL TNOU(
     +'as good as new.',15)
      CALL TNOU('Meanwhile, the snake has disappeared.',37)
      MONSTR(SNAKE)=-2
      GOTO 8685
C*
8660  CALL DEAD(FLAGS,VALUES(LUCK))
      GOTO 130
C*
8680  MONSTR(SNAKE)=MONSTR(SNAKE)+RANDOM(3)
      IF (RANDOM(5).EQ.3) MONSTR(SNAKE)=RANDOM(4)-4
8685  IF (MONSTR(SNAKE).LT.0) VALUES(MONCNT)=VALUES(MONCNT)-1
C*
C*    Dwarves
C*
8700  IF (MONSTR(DWARF).LE.0) GOTO 8800     !  Inactive
      IF (Z.LE.1) GOTO 8800  ! Dwarves must be underground
      CALL TONL
      RAND=RANDOM(5)
      IF (RAND.EQ.3) VALUES(DWFNOW)=VALUES(DWFNOW)+1
      IF (VALUES(DWFNUM).LT.2) GOTO 8710
      IF (VALUES(DWFNOW).GT.5) VALUES(DWFNOW)=5       ! No more than 5
      IF (VALUES(DWFNOW).GT.VALUES(DWFNUM)) VALUES(DWFNOW)=
     +VALUES(DWFNUM)    !  Can't be more than total population
      IF (RAND.EQ.1) VALUES(DWFNOW)=VALUES(DWFNOW)-1
8710  RAND=RANDOM(3)
      IF (MONSTR(DWARF).EQ.1) RAND=0   ! Ensure first time message
      HITYOU=.FALSE.
      IF (RANDOM(3).EQ.2) HITYOU=.TRUE.
      IF (VALUES(DWFNOW).GT.1) GOTO 8730
C*
      IF (RAND.EQ.2) GOTO 8715
      IF (RAND.EQ.0) CALL TNOU(
     +'There is an angry little dwarf in here with you.',48)
      IF (RAND.EQ.1) CALL TNOU('The little dwarf is furious.',28)
      GOTO 8750
8715  CALL TNOUA(
     +'The infuriated dwarf shoots a tiny dart',39)
      IF (HITYOU) GOTO 8720
      CALL TNOU(' at you but misses.',19)
      GOTO 8750
8720  CALL TNOU(' which hits you and smarts',26)
      CALL TNOU('painfully for a while.',22)
      VALUES(LUCK)=VALUES(LUCK)-1
      CALL LUCKY(VALUES(LUCK))
      GOTO 8750
C*
8730  IF (RAND.GT.0) GOTO 8740
      CALL TNOUA('There are ',10)
      NUM=VALUES(DWFNOW)
      CALL TNOUA(NUMBER(NUM),LENGTH(NUMBER(NUM),8))
      CALL TNOU(' angry little dwarves in here with you.',39)
      GOTO 8750
8740  IF (RAND.EQ.2) GOTO 8745
      CALL TNOU('The dwarves are furious.',24)
      GOTO 8750
8745  CALL TNOU(
     +'The infuriated dwarves fire a hail of tiny darts, some',54)
      CALL TNOU(
     +'of them hit you and smart painfully like wasp stings.',53)
      VALUES(LUCK)=VALUES(LUCK)-VALUES(DWFNOW)
      CALL LUCKY(VALUES(LUCK))
C*
8750  IF (RANDOM(6).EQ.2) MONSTR(DWARF)=2
      IF (RANDOM(5).EQ.1) MONSTR(DWARF)=-1
      IF (MONSTR(DWARF).LT.0) VALUES(MONCNT)=VALUES(MONCNT)-1
C*
C*    Bats
C*
8800  IF (.NOT.FLAGS(NIGHT).AND.Z.LE.1) GOTO 8900     ! Not in daylight
      CALL TONL
      IF (MONSTR(BATS).LE.0) GOTO 8900      !  Inactive
      IF (MONSTR(BATS).EQ.2) GOTO 8810
      IF (MONSTR(BATS).EQ.3) GOTO 8820
      CALL TNOU(
     +'You have disturbed hundreds of roosting bats, they',50)
      CALL TNOU('wheel and swoop around you.',27)
      GOTO 8830
8810  CALL TNOU(
     +'Hundreds of large bats are flying about.',40)
      GOTO 8830
8820  CALL TNOU(
     +'A cloud of bats sweeps past you and disappears',46)
      CALL TNOU('into the darkness.',18)
8830  MONSTR(BATS)=RANDOM(2)-3
      VALUES(MONCNT)=VALUES(MONCNT)-1
C*
8900  CONTINUE
C*
C*    ****************************
C*    *                          *
C*    *  End of monster section  *
C*    *                          *
C*    ****************************
C*
C*
C*    ********************************************
C*    *                                          *
C*    * All the text is now on the screen.       *
C*    * Get the next instruction from the user.  *
C*    *                                          *
C*    ********************************************
C*
4500  CALL TONL
4501  CALL TONL
      CALL TIMDAT(TIMBUF,28)
      TIMPAS=TIMBUF(4)
      CALL COMIN$(G$PRNT+G$JUST+G$UPPR,'> ',2,LINE,132,LEN,TOKNO)
      CALL TIMDAT(TIMBUF,28)
      TIMPAS=TIMBUF(4)-TIMPAS
      IF (TIMPAS.GT.1) VALUES(TIME)=VALUES(TIME)+TIMPAS
      IF (TOKNO.EQ.0) GOTO 4501
      CALL TONL
      CALL PARSE
C*
C*    First see if ACTION is a naughty word
C*
4505  DO 469 I=1,H$EXP
         IF (ACTION.EQ.EXPLET(I)) GOTO 471
469   CONTINUE
C*
C*    Examine ACTION for direction or action
C*
      DO 460 I=1,H$ACT
         IF (COMEQV(ACTION,DICACT(I))) GOTO 480
460   CONTINUE
C*
C*    Not an action verb, is there an action pending?
C*
      IF (OBJECT.NE.BLANK.OR.TMPACT.EQ.BLANK) GOTO 463
      OBJECT=ACTION
      ACTION=TMPACT
      GOTO 4505
C*
C*    Let's see if it is actually an object.
C*
463   TMPACT=BLANK
      DO 465 OBJPT=1,H$OBJ
         CALL MOVE$R(21,THINGS(1,OBJPT),1,THING,8)
         CALL UPCASE(THING,8)
         IF (COMEQV(ACTION,THING)) GOTO 472
465   CONTINUE
C*
C*    See if ACTION is a monster name
C*
      OBJPT=0           !  Use this as a flag for now
      DO 467 MONPT=1,H$MON
         CALL MOVE$R(1,BEASTS(1,MONPT),1,THING,8)
         CALL UPCASE(THING,8)
         IF (COMEQV(ACTION,THING)) GOTO 485
467   CONTINUE
C*
C*    Not found
C*
470   MESS=RANDOM(5)
      IF (MESS.EQ.1) GOTO 4710
      IF (MESS.EQ.3) GOTO 4720
      CALL TNOU('Pardon?',7)
      GOTO 4501
4710  CALL TNOU('What?',5)
      GOTO 4501
4720  CALL TNOU('Sorry, I don''t understand.',26)
      GOTO 4501
C*
471   CALL TONL
      CALL TNOU(
     +'That sort of language is not permitted here, you have',53)
      CALL TNOU(
     +'been fined 5 points!',20)
      SCORE=SCORE-5
      GOTO 4501
C*
472   CALL TONL
      CALL TNOUA('What do you want to do with the ',32)
      CALL TNOUA(THINGS(11,OBJPT),LENGTH(THINGS(11,OBJPT),20))
      CALL TNOU('?',1)
      TMPOBJ=ACTION
      GOTO 4501
C*
480   TMPACT=BLANK
      IF (OBJECT.EQ.BLANK) OBJECT=TMPOBJ
      TMPOBJ=BLANK
      APOINT=MOD(I,H$MOD)
      IF (APOINT.GT.12) GOTO 510       ! Not a direction
      IF (APOINT.GT.10) GOTO 500  ! In or out.
      IF (WAYS(APOINT)) GOTO 490
      CALL TNOU('You cannot go in that direction!',32)
      GOTO 4501
C*
C*    ACTION is a monster name
C*
485   CALL TONL
      IF (OBJECT.EQ.BLANK) GOTO 487
      CALL TNOU('Sorry, I don''t understand you.',30)
      GOTO 4501
487   CALL TNOU('What do you want to do with it?',31)
      TMPOBJ=ACTION
      GOTO 4501
C*
C*    Make the move
C*
490   X=IABS(X+XINC(APOINT))
      Y=IABS(Y+YINC(APOINT))
      Z=IABS(Z+ZINC(APOINT))
      FLAGS(GLOW)=.FALSE.
      FLAGS(WAVER)=.FALSE.
      GOTO 120
C*
500   IF (.NOT.FLAGS(HOME)) GOTO 470
      IF (APOINT.EQ.11.AND.FLAGS(HOME)) FLAGS(INTENT)=.TRUE.
      IF (APOINT.EQ.12.AND.FLAGS(HOME)) FLAGS(INTENT)=.FALSE.
      GOTO 120
C*
510   IF (APOINT.EQ.17) GOTO 515       !  LOOK
      IF (APOINT.NE.36.AND.APOINT.NE.39) GOTO 520
      IF (APOINT.EQ.O$CHAR) CALL INVENT(VALUES(HELD),OBJLOC)
      IF (APOINT.EQ.39) CALL QUIT(FLAGS(LIFE),FLAGS(UNDEAD))
      GOTO 4501
C*
515   MOVENO=MOVENO-1   ! Adjust number of moves
      VALUES(TIME)=VALUES(TIME)-1 ! Adjust time of day
      GOTO 120
C*
C*    See if player has stayed in the same place as the dragon
C*    without trying to kill it.
C*
520   IF (APOINT.EQ.15.OR.MONSTR(DRAGON).LE.0) GOTO 5100
C*
C*    He has, kill him off
C*
      CALL TNOU(
     +'The dragon breathes a blast of fire, burning you to',51)
      CALL TNOU('a frazzle!',10)
      GOTO 8226         ! Killed by dragon
C*
C*    Examine OBJECT for a noun.
C*
5100  DO 5110 OBJPT=1,H$OBJ
         CALL MOVE$R(21,THINGS(1,OBJPT),1,THING,8)
         CALL UPCASE(THING,8)
         IF (COMEQV(OBJECT,THING)) GOTO 5120
5110  CONTINUE
      IF (APOINT.EQ.23) GOTO 1710 ! Turn OBJECT into ACTION
C*
C*    Examine OBJECT for a monster
C*
      OBJPT=0           !  Use this as a flag for now
      DO 5115 MONPT=1,H$MON
         CALL MOVE$R(1,BEASTS(1,MONPT),1,THING,8)
         CALL UPCASE(THING,8)
         IF (COMEQV(OBJECT,THING)) GOTO 5120
5115  CONTINUE
C*
C*    See if OBJECT is a pseudo object
C*
      DO 5117 PSEUD=1,H$PSEU
         CALL MOVE$R(1,PSEUDO(PSEUD),1,THING,8)
         CALL UPCASE(THING,8)
         IF (COMEQV(OBJECT,THING)) GOTO 5120
5117  CONTINUE
C*
      GOTO 470          ! What?!
C*
C*    Found an object (or a pseudo object).
C*
5120  IF (.NOT.FLAGS(UNDEAD)) GOTO 5125
C*
      CALL TNOU(
     +'Why bother, you no longer have any interest in',46)
      CALL TNOU('these material things.',22)
      GOTO 4501
C*
5125  IF (APOINT.EQ.14) GOTO 1000      !  GET or TAKE
      IF (APOINT.EQ.27) GOTO 1100      !  PUT
      IF (APOINT.EQ.28) GOTO 1100      !  DROP
      IF (APOINT.EQ.13) GOTO 1200      !  ON or LIGHT
      IF (APOINT.EQ.29) GOTO 1250      !  OFF
      IF (APOINT.EQ.30) GOTO 1300      !  RUB
      IF (APOINT.EQ.25) GOTO 1400      !  WAVE
      IF (APOINT.EQ.35) GOTO 1500      !  Magic anagram One.
      IF (APOINT.EQ.38) GOTO 1550      !  SCORE
      IF (APOINT.EQ.32) GOTO 1600      !  PLAY
      IF (APOINT.EQ.33) GOTO 1650      !  READ
      IF (APOINT.EQ.23) GOTO 1700      !  GO
      IF (APOINT.EQ.37) GOTO 1750      !  HELP
      IF (APOINT.EQ.21) GOTO 1800      !  EAT
      IF (APOINT.EQ.41) GOTO 1900      !  OPEN
      IF (APOINT.EQ.44) GOTO 2000      !  UNLOCK
      IF (APOINT.EQ.24) GOTO 2100      !  THROW
      IF (APOINT.EQ.58) GOTO 2200      !  SAVE
      IF (APOINT.EQ.59) GOTO 2250      !  RESTORE
      IF (APOINT.EQ.15) GOTO 2300      !  KILL or SLAY
      IF (APOINT.EQ.26) GOTO 2400      !  STAMP
      IF (APOINT.EQ.40) GOTO 2500      !  DRINK
      IF (APOINT.EQ.42) GOTO 2600      !  CLOSE
      IF (APOINT.EQ.43) GOTO 2700      !  LOCK
      IF (APOINT.EQ.31) GOTO 2800      !  FILL
      IF (APOINT.EQ.34) GOTO 2900      !  FIND
      IF (APOINT.EQ.45) GOTO 2950      !  CALL
      IF (APOINT.EQ.46) CALL INSTR(START)   ! Get instructions
      IF (APOINT.EQ.47) GOTO 3000      !  PLUGH
      IF (APOINT.EQ.48) GOTO 3050      !  GAMIC
      IF (APOINT.EQ.49) GOTO 3100      !  SLEEP
      IF (APOINT.EQ.50) CALL CLOCK(VALUES(TIME),VALUES(DAYLEN),Z)
      IF (APOINT.EQ.51) GOTO 3200      !  WHERE
      IF (APOINT.EQ.52) GOTO 3250      !  ADVENTURE
      IF (APOINT.EQ.18) GOTO 3300      !  CHASE  FOLLOW
      IF (APOINT.EQ.53) GOTO 3400      !  EXAMINE
      IF (APOINT.EQ.22) GOTO 9000      !  MASTER
      GOTO 4500
C*
C*    GET and TAKE.
C*
1000  IF (OBJECT.EQ.BLANK) GOTO 1030
      IF (OBJPT.EQ.0) GOTO 1005
      IF (OBHERE(OBJPT)) GOTO 1010
      CALL TNOUA(
     +'I see no ',9)
      CALL TNOUA(THINGS(11,OBJPT),LENGTH(THINGS(11,OBJPT),20))
      CALL TNOU(' here!',6)
      GOTO 4501
C*
1005  CALL TNOU('I can''t do that.',16)
      GOTO 4501
C*
1010  IF (OBJPT.EQ.O$BASK) GOTO 1020        ! Can always pick up basket
      IF (VALUES(HELD).LT.7) GOTO 1020
      IF (VALUES(HELD).LT.9.AND.OBJLOC(O$BASK).EQ.0) GOTO 1020  ! Basket
      IF (RANDOM(2).EQ.1) GOTO 1015
      CALL TNOU(
     +'Your hands are full!',20)
      GOTO 4501
1015  CALL TNOU(
     +'You can''t carry any more, you will have',39)
      CALL TNOU(
     +'to drop something first!',24)
      GOTO 4501
C*
1020  VALUES(HELD)=VALUES(HELD)+1
      BASKET=0
      IF (OBJLOC(O$BASK).NE.0) GOTO 1022    ! Not got basket
C*
C*    These object cannot be carried in the basket.
C*
      IF (OBJPT.EQ.O$LAMP.OR.OBJPT.EQ.O$SWOR) GOTO 1022
      IF (OBJPT.EQ.O$STAF.OR.OBJPT.EQ.O$CARP) GOTO 1022
C*
      CALL TNOUA('You put the ',12)
      BASKET=1
      GOTO 1024
1022  CALL TNOUA('You pick up the ',16)
1024  CALL TNOUA(THINGS(11,OBJPT),LENGTH(THINGS(11,OBJPT),20))
      IF (BASKET.EQ.0) GOTO 1026
      CALL TNOUA(' into the basket',16)
1026  CALL TNOU('.',1)
      OBJLOC(OBJPT)=0   !  The object is now held.
      OBHERE(OBJPT)=.FALSE.
      ALLDUN(OBJPT)=.FALSE.
      IF (OBJPT.EQ.O$CHAR) VALUES(LUCK)=VALUES(LUCK)+100   ! Got charm
      GOTO 4501
C*
1030  CALL TNOU('What do you want to get?',24)
      TMPACT=ACTION
      GOTO 4501
C*
C*    PUT or DROP
C*
1100  IF (OBJECT.NE.BLANK) GOTO 1105
      CALL TNOU('What do you want to drop?',25)
      TMPACT=ACTION
      GOTO 4501
C*
1105  IF (OBJLOC(OBJPT).EQ.0) GOTO 1110
      CALL TNOU(
     +'But you haven''t got it!',23)
      GOTO 4501
1110  CALL TNOUA('You ',4)
      IF (APOINT.EQ.27) CALL TNOUA('put down',8)
      IF (APOINT.EQ.28) CALL TNOUA('drop',4)
      IF (APOINT.EQ.24) CALL TNOUA('throw down',10)
      CALL TNOUA(' the ',5)
      CALL TNOUA(THINGS(11,OBJPT),LENGTH(THINGS(11,OBJPT),20))
C*
C*    Cushion
C*
      IF (OBJLOC(O$CUSH).EQ.HERE.AND.APOINT.EQ.27) GOTO 1115
      IF (APOINT.EQ.27.AND.FLAGS(INTENT).AND.ALLDUN(O$CUSH)) GOTO 1115
C*
      IF (OBJPT.EQ.O$BOTT) GOTO 2122   ! Bottle
      IF (OBJPT.EQ.O$VASE) GOTO 2122   ! Vase
      IF (OBJPT.EQ.O$GOBL) GOTO 2122   ! Goblet
      IF (OBJPT.EQ.O$MIRR) GOTO 2122   ! Mirror
1115  VALUES(HELD)=VALUES(HELD)-1
      OBJLOC(OBJPT)=HERE
      IF (OBJPT.EQ.O$CHAR) VALUES(LUCK)=VALUES(LUCK)-100        ! Dropped charm
      IF (FLAGS(INTENT)) ALLDUN(OBJPT)=.TRUE.
      IF (ALLDUN(OBJPT)) OBJLOC(OBJPT)=-1000
      IF (OBJPT.NE.O$BASK) GOTO 1130
      DROPIT=0
      DO 1120 J=1,H$OBJ
         IF (OBJLOC(J).NE.0) GOTO 1120
         IF (J.EQ.3.OR.J.EQ.6) GOTO 1120    ! Not lamp, sword, staff &
         IF (J.EQ.10.OR.J.EQ.19) GOTO 1120  ! carpet in basket.
         IF (J.EQ.11) GOTO 1120   ! Basket dropped already.
         DROPIT=1
         VALUES(HELD)=VALUES(HELD)-1
         OBJLOC(J)=HERE
         IF (J.EQ.36) VALUES(LUCK)=VALUES(LUCK)-100        ! Dropped charm
         IF (FLAGS(INTENT)) ALLDUN(J)=.TRUE.
         IF (ALLDUN(J)) OBJLOC(J)=-1000
1120  CONTINUE
C*
      IF (DROPIT.EQ.1) CALL TNOUA(' and everything in it',21)
1130  CALL TNOU('.',1)
      IF (.NOT.FLAGS(SHAFT)) GOTO 1135
      CALL PLUMB(HERE,X,Y,Z,OBJLOC)
      IF (OBJPT.EQ.O$KEYS.OR.OBJPT.EQ.O$COIN) GOTO 1132
      IF (OBJPT.EQ.O$JEWE.OR.OBJPT.EQ.O$BANK) GOTO 1132
      IF (OBJPT.EQ.O$VIOL.OR.OBJPT.EQ.O$BRIC) GOTO 1132
      CALL TNOUA('It falls ',9)
      GOTO 1134
1132  CALL TNOUA('They fall ',10)
1134  CALL TNOU('out of sight down the shaft.',28)
1135  DO 1140  I=1,H$OBJ
         OBHERE(I)=.FALSE.
         IF (OBJLOC(I).EQ.HERE) OBHERE(I)=.TRUE.
C****         IF (FLAGS(INTENT).AND.ALLDUN(OBJPT)) OBHERE(I)=.TRUE.
         IF (FLAGS(INTENT).AND.ALLDUN(I)) OBHERE(I)=.TRUE.
1140  CONTINUE
      GOTO 4501
C*
C*    Light lamp
C*
1200  IF (OBJLOC(O$LAMP).EQ.0) GOTO 1210
1205  CALL TNOU('You haven''t got it!',19)
      GOTO 4501
1210  IF (.NOT.FLAGS(LAMPON)) GOTO 1220
      CALL TNOU('It is already on!',17)
      GOTO 4501
1220  IF (VALUES(FLICK).GT.3) GOTO 1221
      FLAGS(LAMPON)=.TRUE.
      CALL TNOU('Your lamp is now lit.',21)
      GOTO 4501
1221  CALL TNOU('Sorry, it`s not that easy!',26)
      GOTO 4501
C*
C*    Extinguish lamp
C*
1250  IF (OBJLOC(O$LAMP).NE.0) GOTO 1205
      IF (FLAGS(LAMPON)) GOTO 1260
      CALL TNOU('The lamp is already off!',24)
      GOTO 4501
1260  FLAGS(LAMPON)=.FALSE.
      CALL TNOU('Your lamp is now off.',21)
      GOTO 4501
C*
C*    RUB things
C*
1300  IF (OBJECT.NE.BLANK) GOTO 1302
      CALL TNOU('What do you want to rub?',24)
      TMPACT=ACTION
      GOTO 4501
C*
1302  IF (OBJPT.EQ.O$LAMP) GOTO 1310   !  Lamp
C*    Leave space here for other things to be rubbed.
1305  CALL TNOU('Nothing happens.',16)
      GOTO 4501
C*
1310  IF (OBJLOC(O$LAMP).NE.0) GOTO 1205    !  Not got it.
      IF (VALUES(LIGHT).GT.20) GOTO 1305    !  Not yet
      IF (FLAGS(GENIE)) GOTO 1305 ! Done it already
      CALL TNOU(
     +'You rub the lamp until it gleams. Suddenly a genie',50)
      CALL TNOU(
     +'appears. "Free at last!", he cries, "you shall be"',50)
      CALL TNOU(
     +'rewarded with everlasting light." There is a flash',50)
      CALL TNOU(
     +'and he disappears in a cloud of acrid smoke which',49)
      CALL TNOU(
     +'tarnishes the gleaming surface of the now brightly',50)
      CALL TNOU('burning lamp.',13)
      SCORE=SCORE+10
      FLAGS(GENIE)=.TRUE.
      FLAGS(LAMPON)=.TRUE.
      VALUES(LIGHT)=32000
      VALUES(FLICK)=0
      GOTO 4501
C*
C*    WAVE things
C*
1400  IF (OBJECT.NE.BLANK) GOTO 1405
      CALL TNOU('What do you want to wave?',25)
      TMPACT=ACTION
      GOTO 4501
C*
1405  IF (OBJPT.EQ.O$ROD) GOTO 1410
C*    Leave space for other things to be waved.
      GOTO 1305         !  Nothing happens
C*
1410  IF (OBJLOC(O$ROD).NE.0) GOTO 1205     ! Not got it.
      IF (.NOT.(FLAGS(GLOW).AND.FLAGS(WAVER))) GOTO 1305
      CALL TNOU(
     +'A tall elderly wizard dressed in shimmering white',49)
      CALL TNOU(
     +'robes appears, takes the rod and thanks you politely',52)
      CALL TNOU(
     +'for finding his missing magic wand.',35)
      CALL TNOU(
     +'    "Gandalf''s magic anagram is convenient", he says,',53)
      CALL TNOU(
     +'"but you can only use it seven times."',38)
      CALL TNOU(
     +'He vanishes as suddenly as he came.',35)
      OBJLOC(O$ROD)=-2000
      GONE(O$ROD)=.TRUE.
      SCORE=SCORE+50
      VALUES(HELD)=VALUES(HELD)-1
      GOTO 4501
C*
C*    Magic anagram one, abbreviation not allowed.
C*
1500  IF (ACTION.NE.DICACT(35)) GOTO 470    ! Pardon?
      VALUES(MAGONE)=VALUES(MAGONE)+1
      IF (VALUES(MAGONE).GT.7) GOTO 1305    !  Nothing happens.
      CALL LOCATE(X,Y,Z,WHERE)
      IF (VALUES(BASE).EQ.WHERE) GOTO 1510       ! At base camp.
C*
      VALUES(LASTX)=X             !  Remember
      VALUES(LASTY)=Y             !  where
      VALUES(LASTZ)=Z             !  you were.
C*
      X=VALUES(FIRSTX)            !  and go
      Y=VALUES(FIRSTY)            !  to base
      Z=1               !  camp.
      GOTO 120
C*
1510  X=VALUES(LASTX)             !  At base, go back
      Y=VALUES(LASTY)             !  to where you last
      Z=VALUES(LASTZ)             !  used this word.
      GOTO 120
C*
1550  CALL SCORIT(SCORAL)
      CALL TNOUA('Your current score is ',22)
      CALL TOVFIS(SCORAL,0)
      CALL TNOUA(' in ',4)
      CALL TOVFIS(MOVENO,0)
      CALL TNOU(' moves.',7)
      GOTO 4501
C*
C*    PLAY FLUTE
C*
1600  IF (OBJPT.EQ.O$FLUT.OR.OBJPT.EQ.O$MUSI.OR.OBJECT.EQ.BLANK)
     +GOTO 1605
      CALL TNOU('You can''t play that!',20)
      GOTO  4501
C*
1605  IF (OBJLOC(O$FLUT).EQ.0) GOTO 1610
      CALL TNOU('You have no instrument to play on!',34)   ! No flute
      GOTO 4501
C*
1610  IF (OBJLOC(O$MUSI).EQ.0) GOTO 1620
      CALL TNOU('You have no music!',18)
      GOTO 4501
C*
1620  CALL TNOUA('You play the silver flute very badly, ',38)
C*
C*    Snake
C*
      IF (MONSTR(SNAKE).LE.0) GOTO 1640
      CALL TNOU('the snake is',12)
      CALL TNOU(
     +'alarmed by the noise and slithers out of sight',46)
      CALL TNOU(
     +'through a crack in the floor, never to return.',46)
C*
C*    Remove snake permanently and increment the score
C*
      MONSTR(SNAKE)=0
      SCORE=SCORE+50
      VALUES(MONCNT)=VALUES(MONCNT)-1
      GOTO 4501
C*
1640  CALL TNOU('nothing happens.',16)
      GOTO 4501
C*
C*    READ
C*
1650  IF (OBJPT.EQ.0) GOTO 1655
      IF (OBJLOC(OBJPT).NE.0) GOTO 1205     ! Not got it
      IF (OBJPT.EQ.O$MANU) GOTO 1660   ! Manuscript
      IF (OBJPT.EQ.O$BOOK) GOTO 1670   ! Book
      IF (OBJPT.EQ.O$MUSI) GOTO 1680   ! Music
      GOTO 470
C*
1655  CALL TNOU('Read what?',10)
      TMPACT=ACTION
      GOTO 4501
C*
1660  CALL TNOU(
     +'It is written in old English, apparently by some monk named',59)
      CALL TNOU(
     +'Bede, but you can''t make much of it. You turn it over and',57)
      CALL TNOU(
     +'find that someone has written a glowing description of',54)
      CALL TNOU(
     +'the Indian rope trick on the back.',34)
      GOTO 4501
C*
1670  CALL TNOU(
     +'Unfortunately the book is written in the Gamic script',53)
      CALL TNOU(
     +'used by the trolls and you can''t understand a word of',53)
      CALL TNOU(
     +'it. Someone has scrawled "Stamp them out!" across',49)
      CALL TNOU('the flyleaf.',12)
      GOTO 4501
C*
1680  CALL TNOU(
     +'How clever of you to be able to read music! Perhaps',51)
      CALL TNOU(
     +'you should try playing it too?',30)
      GOTO 4501
C*
C*    GO
C*
1700  IF (OBJECT.NE.BLANK) GOTO 1710
      CALL TNOU('Go where?',9)
      GOTO 4501
C*
1710  ACTION=OBJECT
      OBJECT=BLANK
      DO 1720 I=1,H$ACT
         IF (COMEQV(ACTION,DICACT(I))) GOTO 480
1720  CONTINUE
      GOTO 470
C*
C*    HELP
C*
1750  IF (FLAGS(HOME)) GOTO 1780
      IF (HELPNO.EQ.1) GOTO 1760
      CALL TNOU(
     +'I can get you out of here, at a cost of 20 points,',50)
      CALL TNOU('are you sure you need help that much?',37)
      CALL YESNO(YES)
      IF (.NOT.YES) GOTO 4501
      SCORE=SCORE-20
      HELPNO=1
      GOTO 1770
C*
1760  CALL TNOU(
     +'I can rescue you once more, but the price has gone up',53)
      CALL TNOU('to 30 points, can you afford it?',32)
      CALL YESNO(YES)
      IF (.NOT.YES) GOTO 4501
      SCORE=SCORE-30
      DICACT(37)=BLANK  !  Delete HELP command
C*
1770  CALL TONL
      CALL TNOU(
     +'A thick mist forms above you, from which a giant hand slowly',60)
      CALL TNOUA(
     +'descends. It lifts you ',23)
      IF (VALUES(HELD).GT.0) CALL TNOUA(
     +'and all your possessions ',25)
      CALL TNOU(
     +'into the air.',13)
      CALL TONL
      CALL TNOU(
     +'You lose all sense of time and space. After a while the',55)
      CALL TNOU(
     +'mists clear and ....',20)
      CALL TONL
      CALL SLEEP$(DELAY)
      VALUES(TIME)=1
      FLAGS(DARK)=.FALSE.
C*
C*    Initialise any monsters that still exist.
C*
      DO 1775 I=1,H$MON
           IF (MONSTR(I).NE.0) MONSTR(I)=-1
1775  CONTINUE
      VALUES(DWFNOW)=0       ! Remove any current dwarves.
C*
      X=VALUES(FIRSTX)            !  And go
      Y=VALUES(FIRSTY)            !  to base
      Z=1                         !  camp.
      CALL TONL
      GOTO 120
C*
C*    At HOME, help by displaying the instructions
C*
1780  CALL TNOU('Would you like instructions?',28)
      CALL YESNO(YES)
      IF (YES) CALL TONL
      IF (YES) CALL INSTR(START)
      GOTO 4501
C*
C*    EAT
C*
1800  IF (OBJECT.NE.BLANK) GOTO 1810
      CALL TNOU('Eat what?',9)
      TMPACT=ACTION
      GOTO 4501
C*
1810  IF (OBJLOC(OBJPT).NE.0) GOTO 1205     !  Not got it
      IF (OBJPT.EQ.O$FOOD)  GOTO 1820  !  Food
      IF (OBJPT.EQ.O$PLAN)  GOTO 1830  !  Plant
      IF (OBJPT.EQ.O$GARL)  GOTO 1840  !  Garlic
      IF (OBJPT.EQ.O$VIOL)  GOTO 1850  !  Violets
      IF (OBJPT.EQ.O$OYST)  GOTO 1860  !  Oyster
      IF (OBJPT.EQ.O$CLAM)  GOTO 1870  !  Clam
      MESS=RANDOM(3)
      CALL TNOUA('Not ',4)
      IF (MESS.EQ.1) CALL TNOUA('bl**dy ',7)
      IF (MESS.EQ.2) CALL TNOUA('Pygmalion ',10)
      CALL TNOU('likely!',7)
      GOTO 4501
C*
1820  CALL TNOU('Thank you, that was delicious!',30)
      SCORE=SCORE+5     ! Give him a few points.
1825  OBJLOC(OBJPT)=-2000
      GONE(OBJPT)=.TRUE.
      VALUES(HELD)=VALUES(HELD)-1
      GOTO 4501
C*
1830  IF (RANDOM(10).EQ.5) GOTO 1835
      CALL TNOU(
     +'It is sustaining but not particularly tasty. You',48)
      CALL TNOU(
     +'notice a slight euphoria afterwards, possibly due',49)
      CALL TNOU(
     +'to some alkaloid in the leaves. However, there seem',51)
      CALL TNOU('to be no permanent ill effects.',31)
      OBJLOC(O$PLAN)=-RANDOM(20)
      SCORE=SCORE+2
      IF (RANDOM(10).EQ.8) GOTO 1825   ! Get rid of the plant permanently
      VALUES(HELD)=VALUES(HELD)-1
      GOTO 4501
C*
1835  CALL TNOU(
     +'You eat the plant but are horrified to find the label',53)
      CALL TNOU(
     +'"Aconitum napellus (Monkshood)" tied to the stem. You',53)
      CALL TNOU(
     +'spit out what remains in your mouth but it is too late,',55)
      CALL TNOU(
     +'gradually a numbness spreads over you, followed by a',52)
      CALL TNOU(
     +'creeping paralysis beginning in your legs. Breathing',52)
      CALL TNOU(
     +'is difficult and your pulse becomes slow, irregular and',55)
      CALL TNOU(
     +'weak, although your mind remains perfectly clear.',49)
      IF (FLAGS(ELIXIR)) GOTO 1837
      CALL TNOU('Suddenly you collapse!',22)
      CALL TONL
      OBJLOC(O$PLAN)=-2000
      GONE(O$PLAN)=.TRUE.
      CALL DEAD(FLAGS,VALUES(LUCK))
      GOTO 130
C*
C*    He has drunk the elixir, revive him.
C*
1837  CALL TONL
      CALL SLEEP$(DELAY*2)   ! Keep him in suspense for a while.
      CALL TNOU('After a while you miraculously recover.',39)
      CALL TONL
      SCORE=SCORE+2
      IF (RANDOM(5).EQ.1) GOTO 1825    ! Get rid of the plant permanently
      VALUES(HELD)=VALUES(HELD)-1
      GOTO 4501
C*
1840  CALL TNOUA(
     +'With some distaste you eat the garlic. Your breath ',51)
      CALL TNOU('smells terrible.',16)
      SCORE=SCORE+15
      GOTO 1825
C*
1850  CALL TNOU(
     +'You nibble at the flowers and are delighted to find that',56)
      CALL TNOU(
     +'they are crystallised violets preserved in sugar. You eat',57)
      CALL TNOU(
     +'the lot and the world seems a better and happier place, for',59)
      CALL TNOU(
     +'a time at least.',16)
      SCORE=SCORE+10
      GOTO 1825
C*
1860  IF (MONTH.LE.8) GOTO 1864
1862  CALL TNOU(
     +'Sorry, there is no "r" in the month and shellfish',49)
      CALL TNOU('are not in season.',18)
      GOTO 4501
C*
1864  IF (OBJLOC(O$DAGG).EQ.0) GOTO 1868
1866  CALL TNOU(
     +'You have nothing to open it with!',33)
      GOTO 4501
C*
1868  CALL TNOU(
     +'You use the dagger to prise open the oyster, there',50)
      CALL TNOU(
     +'is no pearl inside and you swallow the contents whole.',54)
      CALL TNOU(
     +'A penguin dressed as a waiter (how else?) waddles in, takes',59)
      CALL TNOU(
     +'the empty shell from you very politely and waddles out.',55)
      SCORE=SCORE+15
      GOTO 1825
C*
1870  IF (MONTH.GT.8) GOTO 1862
C*
      IF (OBJLOC(O$SWOR).NE.0) GOTO 1866    ! No sword
C*
      CALL TNOU(
     +'You force the sword blade between the two halves of the',55)
      CALL TNOU(
     +'clam''s shell and with a great effort prise it open. An',54)
      CALL TNOU(
     +'evil looking little dwarf leaps out of the shell, curses',56)
      CALL TNOU(
     +'angrily and vanishes out of sight before you can do',51)
      CALL TNOU(
     +'anything to stop him. With a clang the shell snaps shut!',56)
      VALUES(DWFNUM)=VALUES(DWFNUM)+1
      GOTO 4501
C*
C*    OPEN
C*
1900  IF (OBJECT.EQ.BLANK) GOTO 1910
      IF (OBJLOC(OBJPT).NE.0) GOTO 1205
      IF (OBJPT.EQ.O$BOOK)  GOTO 1670       !  Book
      IF (OBJPT.EQ.O$BOX)   GOTO 1920       !  Box
      IF (OBJPT.EQ.O$OYST)  GOTO 1860       !  Oyster
      IF (OBJPT.EQ.O$CLAM)  GOTO 1870       !  Clam
      GOTO 470
C*
1910  CALL TNOU('Open what?',10)
      TMPACT=ACTION
      GOTO 4501
C*
1920  IF (.NOT.FLAGS(BOXLOK)) GOTO 1925
      CALL TNOU('The box is locked!',18)
      GOTO 4501
C*
1925  CALL TNOU(
     +'You open the box, inside the lid is a label addressed :',55)
      CALL TNOU(
     +'    "Pandora, c/o Zeus, Mount Olympus".',39)
      CALL TNOU(
     +'Fortunately for you the box is otherwise empty!',47)
      CALL TNOU(
     +'You quickly close it again.',27)
      GOTO 4501
C*
C*    UNLOCK
C*
2000  IF (OBJLOC(O$KEYS).EQ.0) GOTO 2010
      CALL TNOU('You haven''t any keys!',21)
      GOTO 4501
C*
2010  IF (OBJLOC(OBJPT).NE.0) GOTO 1205     !  Not got it
      IF (OBJPT.EQ.O$BOX) GOTO 2020
C*    Space here for DOOR coding
      GOTO 470
C*
2020  IF (FLAGS(BOXLOK)) GOTO 2030
      CALL TNOU('Why bother, it isn''t locked.',28)
      GOTO 4501
C*
2030  CALL TNOU('The box is now unlocked.',24)
      FLAGS(BOXLOK)=.FALSE.
      GOTO 4501
C*
C*    THROW
C*
2100  IF (OBJLOC(OBJPT).NE.0) GOTO 1205     !  Not got it
      IF (OBJPT.EQ.O$BOTT)  GOTO 2120  !  Bottle
      IF (OBJPT.EQ.O$SWOR)  GOTO 2130  !  Sword
      IF (OBJPT.EQ.O$ROD)   GOTO 2140  !  Rod
      IF (OBJPT.EQ.O$ROPE)  GOTO 2150  !  Rope
      IF (OBJPT.EQ.O$STAF)  GOTO 2140  !  Staff
      IF (OBJPT.EQ.O$COIN)  GOTO 2160  !  Coins
      IF (OBJPT.EQ.O$VASE)  GOTO 2120  !  Vase
      IF (OBJPT.EQ.O$BOOK)  GOTO 2170  !  Book
      IF (OBJPT.EQ.O$DAGG)  GOTO 2130  !  Dagger
      IF (OBJPT.EQ.O$GOBL)  GOTO 2120  !  Goblet
      GOTO 1100         ! Not one of the above
C*
2120  CALL TNOUA('You throw the ',14)
      CALL TNOUA(THINGS(11,OBJPT),LENGTH(THINGS(11,OBJPT),20))
2122  CALL TNOU(
     +', it smashes into a thousand fragments.',39)
      IF (HERE.NE.VALUES(BASE)) GOTO 2124
      CALL TNOU(
     +'A native servant rushes in, sweeps up the pieces and',52)
      CALL TNOU(
     +'dashes out again.',17)
      GOTO 2125
2124  IF (OBJPT.EQ.O$BOTT)  VALUES(BRKBOT)=HERE  ! Bottle
      IF (OBJPT.EQ.O$VASE)  VALUES(BRKVAS)=HERE  ! Vase
      IF (OBJPT.EQ.O$GOBL)  VALUES(BRKGOB)=HERE  ! Goblet
      IF (OBJPT.EQ.O$MIRR)  VALUES(BRKMIR)=HERE  ! Mirror
2125  VALUES(HELD)=VALUES(HELD)-1
      SCORE=SCORE-10
      OBJLOC(OBJPT)=-2000
      GONE(OBJPT)=.TRUE.
      ALLDUN(OBJPT)=.FALSE.
      IF (OBJPT.EQ.O$MIRR) GOTO 2127
      GOTO 4501
C*
C*    Broken the mirror!
C*
2127  CALL TONL
      CALL TNOU(
     +'Oh dear! You have broken the mirror, be prepared for',52)
      CALL TNOU(
     +'seven year''s bad luck!!',23)
      VALUES(LUCK)=4
      GOTO 4501
C*
2130  CALL TNOUA('You throw the ',14)
      CALL TNOUA(THINGS(11,OBJPT),LENGTH(THINGS(11,OBJPT),20))
      IF (OBJPT.EQ.O$SWOR.AND.Z.LE.1) GOTO 2138
      CALL TNOU(', it hits the ground and',24)
      CALL TNOU('disappears in a shower of sparks.',33)
2135  VALUES(HELD)=VALUES(HELD)-1
      OBJLOC(OBJPT)=-RANDOM(H$OBJ)-15
      GOTO 4501
C*
2138  CALL TNOU(
     +', as if guided by some unseen power it flies',44)
      CALL TNOU(
     +'straight as an arrow towards a shimmering lake which has',56)
      CALL TNOU(
     +'suddenly appeared nearby. A graceful hand reaches up,',53)
      CALL TNOU(
     +'catches it, waves it thrice and disappears again. The',53)
      CALL TNOU('lake vanishes.',14)
      GOTO 2135
C*
2140  CALL TNOU(
     +'It sticks into the ground and immediately sprouts leaves',56)
      CALL TNOU(
     +'and tiny, scented flowers. However, these rapidly shrivel',57)
      CALL TNOUA('away to nothing',15)
      GOTO 1115
C*
2150  IF (Z.LE.1) GOTO 1110
C***      IF (MOD(HERE,30).EQ.19) GOTO 1110
      IF (.NOT.FLAGS(GLOW)) GOTO 1110
      CALL TNOU(
     +'The rope stands straight up, unsupported!! A fakir scrambles',60)
      CALL TNOU(
     +'nimbly down, gathers it up and disappears into the shadows.',59)
      CALL TNOU(
     +'As he goes a small, brilliant object falls from his loincloth',
     +61)
      CALL TNOU('and rolls away out of sight.',28)
      OBJLOC(O$DIAM)=-RANDOM(H$OBJ)-15      !  Diamond now exists
      GONE(O$DIAM)=.FALSE.
      OBJLOC(O$ROPE)=-2000   ! Get rid of rope
      GONE(O$ROPE)=.TRUE.
      SCORE=SCORE+50
      VALUES(HELD)=VALUES(HELD)-1
      GOTO 4501
C*
2160  IF (VALUES(DWFNUM).EQ.0) GOTO 1110
      CALL TNOU(
     +'A little dwarf scurries up, grabs the coins and',47)
      CALL TNOU('disappears before you can catch him.',36)
      GOTO 2125
C*
2170  IF (MONSTR(TROLL).GT.0) GOTO 2175
      CALL TNOU(
     +'There is no-one here to throw the book at - except,',51)
      CALL TNOU('perhaps, you!',13)
      GOTO 4501
C*
2175  CALL TNOU(
     +'The troll catches the book and retires to a corner to',53)
      CALL TNOU(
     +'read it. Suddenly he chortles "At last I have found',51)
      CALL TNOU(
     +'how to get rid of them pesky dwarves!" He lumbers',49)
      CALL TNOU(
     +'over, shakes you roughly by the hand and vanishes',49)
      CALL TNOU(
     +'forever.',8)
      MONSTR(TROLL)=0
      VALUES(HELD)=VALUES(HELD)+1
      OBJLOC(O$BOOK)=-2000
      ALLDUN(O$BOOK)=.FALSE.
      GONE(O$BOOK)=.TRUE.
      SCORE=SCORE+50
      GOTO 4501
C*
C*    SAVE
C*
2200  VALUES(1)=MOVENO
      VALUES(2)=SCORE
      VALUES(3)=X
      VALUES(4)=Y
      VALUES(5)=Z
      CALL SAVE(VALUES,FLAGS,OBJLOC,MONSTR)
      GOTO 4500
C*
C*    RESTORE
C*
2250  CALL RESTOR(MOVENO,SCORE,X,Y,Z,VALUES,FLAGS,OBJLOC,
     +MONSTR)
      IF (MOVENO.EQ.-999) RETURN
      DO 2260 I=1,H$OBJ
         ALLDUN(I)=.FALSE.
         GONE(I)=.FALSE.
         IF (OBJLOC(I).EQ.-1000) ALLDUN(I)=.TRUE.
         IF (OBJLOC(I).EQ.-2000) GONE(I)=.TRUE.
2260  CONTINUE
      MOVENO=MOVENO-1
      GOTO 120
C*
C*    KILL or SLAY
C*
2300  IF (OBJECT.NE.BLANK) GOTO 2305
      CALL TNOU('Kill what?',10)
      TMPACT=ACTION
      GOTO 4501
C*
2305  IF (OBJPT.EQ.0) GOTO 2310
      CALL TNOU('What''s the point?',17)
      GOTO 4501
C*
2310  IF (MONSTR(MONPT).GT.0) GOTO 2312
      CALL TNOUA('What ',5)
      CALL TNOUA(BEASTS(1,MONPT),LENGTH(BEASTS(1,MONPT),8))
      CALL TNOU('?',1)
      GOTO 4501
C*
2312  IF (MONPT.EQ.8) GOTO 2315   ! Vampire
      IF (OBJLOC(O$STAF).EQ.0) GOTO 2340    ! Staff
      IF (OBJLOC(O$DAGG).EQ.0) GOTO 2330    !  Dagger
      IF (OBJLOC(O$SWOR).EQ.0) GOTO 2320    ! Sword
2315  CALL TNOU(
     +'You have nothing which could possibly hurt it!',46)
      GOTO 4501
C*
2320  IF (MONPT.EQ.6) GOTO 2328   ! Troll
      IF (MONPT.EQ.7) GOTO 2324   ! Dragon
2322  CALL TNOU(
     +'You lash out but it easily avoids your blow.',44)
      GOTO 4501
C*
2324  IF (RANDOM(4).EQ.2) GOTO 2326
      CALL TNOU(
     +'You strike at the dragon with your sword but it',47)
      CALL TNOU(
     +'parries your blow with its iron claws.',38)
      GOTO 4501
C*
2326  CALL TNOU(
     +'As the dragon rears up you plunge your sword into its',53)
      CALL TNOU(
     +'breast. It disappears up to the hilt and there is a',51)
      CALL TNOU(
     +'tremendous explosion which knocks you senseless. When',53)
      CALL TNOU(
     +'you recover the dragon and sword are nowhere to be seen.',56)
      CALL TNOU(
     +'Wisps of smoke drift about but gradually disperse.',50)
      SCORE=SCORE+50
      MONSTR(DRAGON)=0
      VALUES(HELD)=VALUES(HELD)-1
      OBJLOC(O$SWOR)=-2000
      ALLDUN(O$SWOR)=.FALSE.
      GONE(O$SWOR)=.TRUE.
      GOTO 4501
C*
2328  CALL TNOU(
     +'You strike the troll with the sword but the blade',49)
      CALL TNOU(
     +'bounces back from its stony hide, severely jarring',50)
      CALL TNOU(
     +'your arm. The troll does not appear to have noticed',51)
      CALL TNOU('the blow.',9)
      GOTO 4501
C*
2330  IF (MONPT.NE.TROLL) GOTO 2322
      CALL TNOU(
     +'You stab the troll with the dagger, shattering the',50)
      CALL TNOU(
     +'blade on its rock hard hide. The troll snatches the',51)
      CALL TNOU(
     +'haft from you and grinds it to dust.',36)
      VALUES(HELD)=VALUES(HELD)-1
      OBJLOC(O$DAGG)=-2000
      ALLDUN(O$DAGG)=.FALSE.
      GONE(O$DAGG)=.TRUE.
      GOTO 4501
C*
2340  CALL TNOU(
     +'You flail about you with the staff but hit nothing.',51)
      GOTO 4501
C*
C*    STAMP
C*
2400  IF (MONSTR(DWARF).GE.1) GOTO 2410
2405  CALL TNOU(
     +'You stamp on the ground, nothing happens.',41)
      GOTO 4501
C*
2410  IF (OBJPT.NE.0) GOTO 2405
      IF (MONPT.NE.DWARF) GOTO 2405
      IF (RANDOM(3).EQ.0) GOTO 2420
      CALL TNOUA('You try to stamp on ',20)
      IF (VALUES(DWFNOW).EQ.1) CALL TNOUA('the',3)
      IF (VALUES(DWFNOW).GT.1) CALL TNOUA('a',1)
      CALL TNOU(' little dwarf but he gets',25)
      CALL TNOU('out of the way in time.',23)
      GOTO 8000
2420  CALL TNOUA('You stamp on ',13)
      IF (VALUES(DWFNOW).LT.1) GOTO 2425
      IF (VALUES(DWFNOW).EQ.1) CALL TNOUA('the',3)
      IF (VALUES(DWFNOW).GT.1) CALL TNOUA('a',1)
      CALL TNOU(' little dwarf and squash him flat.',34)
      SCORE=SCORE+10
      VALUES(DWFNOW)=VALUES(DWFNOW)-1
      VALUES(DWFNUM)=VALUES(DWFNUM)-1
      IF (VALUES(DWFNOW).GT.0) GOTO 4501
      MONSTR(DWARF)=-MONSTR(DWARF)
      IF (VALUES(DWFNUM).EQ.0) MONSTR(DWARF)=0
      GOTO 4501
2425  CALL TNOU(
     +'the ground but the little dwarf has gone.',41)
      GOTO 4501
C*
C*    DRINK
C*
2500  IF (FLAGS(DRIP).AND.VALUES(POOL).EQ.2) GOTO 2540
      IF (FLAGS(DRIP)) GOTO 2550
      IF (OBJLOC(O$BOTT).EQ.0.OR.OBJLOC(O$ELIX).EQ.0) GOTO 2510
      CALL TNOU('You have nothing to drink.',26)
      GOTO 4501
C*
2510  IF (OBJECT.EQ.BLANK) GOTO 2515
      IF (PSEUD.EQ.4) GOTO 2520
      IF (OBJPT.EQ.O$ELIX) GOTO 2530
2515  CALL TNOU('Drink what?',11)
      TMPACT=ACTION
      GOTO 4501
C*
2520  IF (.NOT.FLAGS(EMPTY)) GOTO 2525
      CALL TNOU('Your bottle is empty.',21)
      GOTO 4501
C*
2525  CALL TNOUA(
     +'You drink the contents of the bottle and feel',45)
      CALL TNOU(' much refreshed.',16)
      FLAGS(EMPTY)=.TRUE.
      VALUES(THIRST)=0
      VALUES(LUCK)=VALUES(LUCK)+10
      GOTO 4501
C*
2530  IF (OBJLOC(O$ELIX).NE.0) GOTO 1205    ! Not got it
      CALL TNOU(
     +'You place the phial containing the elixir to your',49)
      CALL TNOU(
     +'lips and drain every drop. A feeling of renewed',47)
      CALL TNOU(
     +'strength courses through your veins. The empty',46)
      CALL TNOU(
     +'phial falls to dust in your grasp.',34)
      FLAGS(ELIXIR)=.TRUE.
      VALUES(HELD)=VALUES(HELD)-1
      OBJLOC(O$ELIX)=-2000
      ALLDUN(O$ELIX)=.FALSE.
      GONE(O$ELIX)=.TRUE.
      SCORE=SCORE+10
      VALUES(THIRST)=0
      VALUES(LUCK)=VALUES(LUCK)+50
      GOTO 4501
C*
C*    Drink from pool
C*
2540  CALL TNOU(
     +'You drink cool, clear water from the pool and feel',50)
      CALL TNOU('much refreshed.',15)
      VALUES(THIRST)=0
      VALUES(LUCK)=VALUES(LUCK)+10
      GOTO 4501
C*
2550  CALL TNOU(
     +'The water drip is too slow to quench your thirst but',52)
      CALL TNOU(
     +'at least you can moisten your parched lips.',43)
      VALUES(THIRST)=VALUES(THIRST)-10
      VALUES(LUCK)=VALUES(LUCK)+2
      GOTO 4501
C*
C*    CLOSE
C*
2600  CALL TNOU('NOT IMPLEMENTED YET.',20)
      GOTO 4501
C*
C*    LOCK
C*
2700  CALL TNOU('NOT IMPLEMENTED YET.',20)
      GOTO 4501
C*
C*    FILL
C*
2800  IF (OBJECT.NE.BLANK) GOTO 2810
2805  CALL TNOU('Fill what?',10)
      TMPACT=ACTION
      GOTO 4501
C*
2810  IF (OBJLOC(OBJPT).NE.0) GOTO 2805
      IF (OBJPT.EQ.O$BOTT)  GOTO 2820  ! Bottle
      IF (OBJPT.EQ.O$BASK)  GOTO 2830  ! Basket
      IF (OBJPT.EQ.O$VASE)  GOTO 2840  ! Vase
      IF (OBJPT.EQ.O$BOX)   GOTO 2850  ! Box
      IF (OBJPT.EQ.O$GOBL)  GOTO 2860  ! Goblet
      CALL TNOU('But it can''t hold anything!',27)
      GOTO 4501
C*
2820  IF (FLAGS(EMPTY)) GOTO 2822
      CALL TNOU('It is already full.',19)
      GOTO 4501
C*
2822  IF (.NOT.FLAGS(DRIP)) GOTO 2824
      IF (VALUES(POOL).EQ.2) GOTO 2826
      IF (VALUES(POOL).GE.0) GOTO 2828
2824  CALL TNOU('You have nothing to fill it with!',33)
      GOTO 4501
C*
2826  CALL TNOU('Your bottle is now full of water.',33)
      FLAGS(EMPTY)=.FALSE.
      GOTO 4501
C*
C*    Water drip but no pool
C*
2828  CALL TNOU(
     +'The water drip is very slow, it would take for ever to',54)
      CALL TNOU(
     +'fill anything from it and you quickly give up the attempt.',58)
      GOTO 4501
C*
2830  IF (VALUES(HELD).GT.8) GOTO 1015 ! Can't carry more
      DO 2832 K=1,H$OBJ
         IF (OBHERE(K)) GOTO 2834
2832   CONTINUE
      CALL TNOU('There is nothing here to put in it.',35)
      GOTO 4501
C*
2834  CALL TNOUA('You put the ',12)
      CALL TNOUA(THINGS(11,K),LENGTH(THINGS(11,K),20))
      CALL TNOU(' into the basket.',17)
      OBHERE(K)=.FALSE.
      OBJLOC(K)=0
      VALUES(HELD)=VALUES(HELD)+1
      GOTO 4501
C*
2840  IF (.NOT.FLAGS(DRIP)) GOTO 2824
      IF (VALUES(POOL).NE.2) GOTO 2828
      CALL TNOU(
     +'You attempt to fill the vase with water from the',48)
      CALL TNOU(
     +'pool, but it pours out of a hole in the bottom.',47)
      GOTO 4501
C*
2850  CONTINUE
      IF (FLAGS(BOXLOK)) GOTO 1920
      CALL TNOUA('Would that you could put all the ills ',38)
      CALL TNOU('of mankind back into it.',24)
      GOTO 4501
C*
2860  IF (.NOT.FLAGS(DRIP)) GOTO 2824
      IF (VALUES(POOL).NE.2) GOTO 2824
      CALL TNOU(
     +'You dip the goblet into the pool but it shatters on',51)
      CALL TNOU(
     +'contact with the ice-cold water.',32)
      VALUES(HELD)=VALUES(HELD)-1
      OBJLOC(O$GOBL)=-2000
      ALLDUN(O$GOBL)=.FALSE.
      GONE(O$GOBL)=.TRUE.
      SCORE=SCORE-5
      GOTO 4501
C*
C*    FIND
C*
2900  IF (OBJECT.NE.BLANK) GOTO 2910
      CALL TNOU('Find what?',10)
      GOTO 4501
C*
2910  CALL TNOU(
     +'If you can''t find it I''m sure that I can''t!',43)
      GOTO 4501
C*
C*    CALL
C*
2950  CALL TNOUA('No one comes',12)
      IF (RANDOM(3).EQ.0) CALL TNOUA(
     +' - servants are so difficult to find these days.',48)
      CALL TNOU('.',1)
      GOTO 4501
C*
C*    PLUGH
C*
3000  IF (COMP$R(5,ACTION,'PLUGH').NE.0) GOTO 470     ! Pardon?
      IF (RANDOM(3).EQ.0) GOTO 3010
      CALL PLUGH
      GOTO 4501
C*
3010  CALL TNOU(
     +'I think you are in the wrong game, try ADVENTURE.',49)
      GOTO 4501
C*
C*    GAMIC
C*
3050  IF (ACTION.NE.'GAMIC   ') GOTO 470    ! Pardon?
      CALL TNOUA(
     +'Argg, ywyll slagwyll Gaimykk, ipf wazglytt apfglyjll ',53)
      CALL TNOU('sqydd ''swalguut.',16)
      CALL TNOU(
     +'Hyperrd makargyulaitt zligwik puddhamerr!',41)
3060  CALL TONL
      CALL COMIN$(G$PRNT+G$JUST+G$UPPR,'> ',2,LINE,132,LEN,TOKENS)
      CALL TONL
      IF (LEN.EQ.0) GOTO 3060
      IF (COMEQV(LINE,'QUIT')) GOTO 3070
      IF (LINE(1).EQ.'EN') GOTO 3080
      IF (RANDOM(5).EQ.3) GOTO 3065
      CALL TNOU(
     +'Ifg doanutt unstlangg, plygickk splagwyll Gaimykk.',50)
      GOTO 3060
3065  CALL TNOU(
     +'Ipf ywyll wallogg quit, sayligg soww.',37)
      GOTO 3060
C*
3070  CALL QUIT(FLAGS(LIFE),FLAGS(UNDEAD))
      GOTO 3060
C*
3080  CALL MOVE$R(3,LINE,1,BUFFER,6)
      IF (COMEQV(BUFFER,'GLISH')) GOTO 3085
      GOTO 3060
3085  CALL TNOUA(
     +'OK, if you don''t want to practise your Gamic we will ',53)
      CALL TNOU('communicate in English.',23)
      GOTO 4501
C*
C*    SLEEP
C*
3100  IF (FLAGS(INTENT)) GOTO 3120
      IF (FLAGS(HOME)) GOTO 3130
      IF (Z.LT.1) GOTO 3140       ! Up tree
      IF (Z.EQ.1) GOTO 3150       ! On the plain
C*
C*    Underground
C*
      IF (.NOT.FLAGS(SHAFT)) GOTO 3105
      CALL TNOUA(
     +'You are certain to ''drop off'' if you fall asleep here,',54)
      CALL TNOU(' try somewhere else.',20)
      GOTO 4501
C*
3105  CALL TNOU(
     +'You prop yourself against a wall and fall into a deep sleep.',60)
      CALL TNOU(
     +'The elves steal all your belongings but then take pity on you',
     +61)
      CALL TNOU(
     +'and carry you back to ground level.',35)
      VALUES(TIME)=1
      FLAGS(DARK)=.FALSE.
      VALUES(HELD)=0
      DO 3110 I=1,H$OBJ      ! Scatter his belongings about
         IF (OBJLOC(I).NE.0) GOTO 3110      ! but not too far.
      OBJLOC(I)=-RANDOM(10)
3110  CONTINUE
      X=VALUES(FIRSTX)
      Y=VALUES(FIRSTY)
      Z=1
      GOTO 120
C*
3120  CALL TNOUA('Yawning wearily, you',20)
3125  CALL TNOU(
     +' lie down on the bed and fall into a deep, refreshing',53)
      CALL TNOU(
     +'sleep. When you awake it is daybreak.',37)
      VALUES(TIME)=1
      FLAGS(NIGHT)=.FALSE.
      FLAGS(DARK)=.FALSE.
      GOTO 4501
C*
3130  CALL TNOUA('You go into the tent,',21)
      FLAGS(INTENT)=.TRUE.
      GOTO 3125
C*
3140  CALL TNOU(
     +'Wedging yourself against the base of a large, forked branch',59)
      CALL TNOU(
     +'you fall into an uneasy sleep. Fortunately the leopard',54)
      CALL TNOU(
     +'doesn''t visit your tree during the night. The birds',51)
      CALL TNOU(
     +'arose you at dawn.',18)
      VALUES(TIME)=1
      FLAGS(NIGHT)=.FALSE.
      FLAGS(DARK)=.FALSE.
      GOTO 4501
C*
3150  CALL TNOU(
     +'You burrow into some long grass nearby and fall asleep,',55)
      CALL TNOU(
     +'hoping that none of the large cats find you. Whilst you',55)
      CALL TNOU(
     +'slumber a deadly scorpion crawls down the neck of your shirt.',
     +61)
      CALL TNOU(
     +'Feeling something scratching you suddenly start up and the',58)
      CALL TNOU(
     +'scorpion stings you before you can remove it. You have no',57)
      CALL TNOU(
     +'antidote to the poison and rapidly succumb to it.',49)
      CALL DEAD(FLAGS,VALUES(LUCK))
      GOTO 130
C*
C*    WHERE (Approximate location of player with respect to his
C*    tent, only applicable when above ground.
C*
3200  IF (HERE.EQ.VALUES(BASE)) GOTO 515
      IF (Z.LE.1) GOTO 3210
      CALL TNOU(
     +'You are in a complex system of underground chambers, passages',
     +61)
      CALL TNOU(
     +'and shafts.',11)
      GOTO 4500
C*
3210  IDISTX=X-VALUES(FIRSTX)
      IDISTY=Y-VALUES(FIRSTY)
      FLAGXY=IABS(IDISTX*IDISTY)  ! If this is 0 then one of them is 0.
      DISTX=FLOAT(IDISTX)
      DISTY=FLOAT(IDISTY)
      IDIST=INT(SQRT(SQRT(DISTX*DISTX+DISTY*DISTY)))
      CALL TNOUA('You are ',8)
      IF (IDIST.EQ.1) CALL TNOUA(
     +'quite close to ',15)
      IF (IDIST.EQ.2) CALL TNOUA(
     +'some distance from ',19)
      IF (IDIST.EQ.3) CALL TNOUA(
     +'several miles away from ',24)
      IF (IDIST.GT.3) CALL TNOUA(
     +'many miles away from ',21)
      CALL TNOUA(
     +'your base camp, which ',22)
C*
C*    North, south, east or west.
C*
      IF (FLAGXY.NE.0) GOTO 3220
      CALL TNOUA('lies due ',9)
      IF (IDISTY.GT.0) CALL TNOU('south.',6)
      IF (IDISTY.LT.0) CALL TNOU('north.',6)
      IF (IDISTX.GT.0) CALL TNOU('west.',5)
      IF (IDISTX.LT.0) CALL TNOU('east.',5)
      GOTO 4500
C*
C*    Other directions.
C*
3220  CALL TNOUA('lies to the ',12)
      IF (IDISTY.GT.0) CALL TNOUA('south',5)
      IF (IDISTY.LT.0) CALL TNOUA('north',5)
      IF (IABS(IDISTX).NE.IABS(IDISTY)) CALL TNOU(' and',4)
      IF (IDISTX.GT.0) CALL TNOU('west.',5)
      IF (IDISTX.LT.0) CALL TNOU('east.',5)
      GOTO 4500
C*
C*    ADVENTURE
C*
3250  IF (COMP$R(4,ACTION,'ADVE').NE.0) GOTO 470      ! Pardon?
      CALL ADVENT
      CALL SLEEP$(DELAY)
C*
C*    Drop everything HERE
C*
      DO 3260 I=1,H$OBJ
         IF (OBJLOC(I).NE.0) GOTO 3260
         OBJLOC(I)=HERE
3260  CONTINUE
C*
C*    If he had the lamp put it outside the tent.
C*
      IF (OBJLOC(O$LAMP).EQ.HERE) OBJLOC(O$LAMP)=VALUES(BASE)
C*
C*    Move player into tent and set daybreak
C*
      FLAGS(HOME)=.TRUE.
      FLAGS(INTENT)=.TRUE.
      X=VALUES(FIRSTX)
      Y=VALUES(FIRSTY)
      Z=1
      VALUES(TIME)=1
      VALUES(LIGHT)=100
C*
      CALL TONL
      CALL TNOU(
     +'You awake as from a bad dream to find yourself in your tent.',60)
      IF (VALUES(HELD).GT.0) CALL TNOU(
     +'Everthing you were carrying has vanished!',41)
      VALUES(HELD)=0
      CALL TONL
      GOTO 120
C*
C*    CHASE or FOLLOW
C*
3300  MESS=RANDOM(5)
      GOTO (3310,3320,3330,3340,3350), MESS+1
C*
3310  CALL TNOU(
     +'You set off in pursuit but miss your footing and fall',53)
      CALL TNOUA(
     +'heavily',7)
      IF (VALUES(HELD).GT.0) CALL TNOUA(
     +', scattering your possessions about you',39)
      CALL TNOU('. ',2)
C*
      VALUES(HELD)=0
      DO 3315 I=1,H$OBJ
         IF (OBJLOC(I).EQ.0) OBJLOC(I)=HERE
3315  CONTINUE
      GOTO 4501
C*
3320  CALL TNOU(
     +'You step forward but are suddenly overcome with weariness',57)
3325  CALL TNOU(
     +'and fall to the ground in a dead faint. After a time you',56)
      CALL TNOU(
     +'recover your senses and your strength rapidly returns.',54)
      VALUES(TIME)=VALUES(TIME)+RANDOM(10)+1
      GOTO 4501
C*
3330  CALL TNOUA(
     +'You leap forward but some mysterious and irresistable ',54)
      CALL TNOU('power holds',11)
      CALL TNOU(
     +'you back. You struggle for a while but rapidly succumb',54)
      GOTO 3325
C*
3340  CALL TNOU(
     +'A ghastly figure rises before you. "Don''t do it; I did and',58)
      CALL TNOU(
     +'this is the result!" it moans as it slowly crumbles into',56)
      CALL TNOUA(
     +'dust. You decide that discretion is the better part ',52)
      CALL TNOU(
     +'of valour.',10)
      GOTO 4501
C*
3350  CALL TNOU(
     +'You give chase. After a while you pause for breath and',54)
      CALL TNOU(
     +'look about you. Realising that you have run round in',52)
      CALL TNOU(
     +'circles and are back where you started from you give up.',56)
      VALUES(TIME)=VALUES(TIME)+RANDOM(4)+1
      GOTO 4501
C*
C*    EXAMINE
C*
3400  IF (COMP$R(4,ACTION,'EXAM').NE.0) GOTO 470      ! Pardon?
      CALL TNOUA(
     +'Please refer to the Cambridge Local Examinations ',49)
      CALL TNOU('Syndicate',9)
      CALL TNOU(
     +'for information; we cannot provide any assistance',49)
      CALL TNOU(
     +'with examinations here.',23)
      GOTO 4501
C*
C*    MASTER (No abbrevation of command allowed)
C*    Allows masters to pick up anything, anywhere, anytime.
C*
9000  IF (ACTION.NE.DICACT(22)) GOTO 470    ! No abbrev.
      CALL STATUS(FLAGS(MASTER),SCORE)      ! Is he a Master?
      IF (.NOT.FLAGS(MASTER)) GOTO 4500     ! Masters only!
9010  CALL COMIN$(G$PRNT+G$JUST,'Object > ',9,LINE,132,LEN,TOKNO)
      IF (TOKNO.EQ.0) GOTO 9020
      READ(UNIT=LINE,ERR=4500) GOT
      IF (GOT.LT.1.OR.GOT.GT.H$OBJ) GOTO 4500
      VALUES(HELD)=VALUES(HELD)+1
      OBJLOC(GOT)=0
      GONE(GOT)=.FALSE.
      ALLDUN(GOT)=.FALSE.
      OBHERE(GOT)=.FALSE.
      GOTO 9010
C*
9020  CALL COMIN$(G$PRNT+G$JUST,'Monster > ',10,LINE,132,LEN,TOKNO)
      IF (TOKNO.EQ.0) GOTO 4500
      READ(UNIT=LINE,ERR=4500) GOT
      IF (GOT.LT.1.OR.GOT.GT.H$MON) GOTO 4500
      IF (MONSTR(GOT).EQ.0) MONSTR(GOT)=-1
      MONSTR(GOT)=-MONSTR(GOT)
      IF (MONSTR(GOT).GT.0) VALUES(MONCNT)=VALUES(MONCNT)+1
      GOTO 9020
C*
      END
C*
C***********************************************************************
C*
      SUBROUTINE STATUS(FLAG,SCORE)
C*
      LOGICAL   FLAG              ! Master status flag
      INTEGER*2 SCORE             ! His score so far
C*
      INTEGER*2 MTRX(8)           ! Data for password construction
      INTEGER*2 TMDATA(4)         ! Buffer for TIMDAT
      INTEGER*2 PWRD(3)           ! Buffer for user-supplied password
C*
      DATA MTRX/'ACODUSENHRRDAUUD'/
C*
      IF (FLAG) RETURN            ! We already know that he is a master
      CALL TNOUA('Are you in fact a Perfect Master? ',34)
      CALL YESNO(FLAG)            ! Does he claim to be one?
      IF (.NOT.FLAG) RETURN
      CALL PASSWD('Prove it! Say the magic word',28,PWRD,6)
      IF (PWRD(3).NE.'  ') GOTO 20     ! Ha!
      CALL TIMDAT(TMDATA,4)       ! Get current date
      TMDATA(4)='  '                   !
      N=NUFRD(TMDATA)                  ! These 3 lines calculate day of week (+
      N=NINDW(N,TMDATA(3))+1           !
      TMDATA(1)=LT(TMDATA(2),8)+RS(MTRX(N),8)    ! Construct today's
      TMDATA(2)=LS(TMDATA(2),8)+RT(MTRX(N),8)    !   password
      IF (TMDATA(1).NE.PWRD(1)) GOTO 20
      IF (TMDATA(2).NE.PWRD(2)) GOTO 20
      GOTO 30                     ! He is. (Flag already true)
C*
20    FLAG=.FALSE.                ! Trying to cheat eh?
      CALL TNOU('Foo, you are just an imposter!',30)
      CALL TNOU('That little piece of deception will cost you 10 points.
     +..',57)
      SCORE=SCORE-10
C*
30    TMDATA(1)=0
      TMDATA(2)=0
      TMDATA(3)=0
      RETURN
      END
C*
C***********************************************************************
C*
      SUBROUTINE ADVENT
C*    Simulates the beginning of ADVENTURE
#include "G_KEYS.INS.f"
      INTEGER*2 LINE(8)
      REAL*8 WORD(2),PLUG
      EQUIVALENCE (LINE(1),WORD(1))
      PLUG='PLUGH   '
C*
      CALL TNOUA('You are standing at the end of a road ',38)
      CALL TNOU('before a small brick building.',30)
      CALL TNOUA('Around you is a forest. A small stream ',39)
      CALL TNOU('flows out of the building and',29)
      CALL TNOU('down a gully and a wide path leads northwest.',45)
10    CALL TONL
      CALL COMIN$(G$BLCM+G$PRNT+G$UPPR+G$JUST,'? ',2,
     +LINE,10,LEN,ITOKEN)
      IF (LEN.EQ.0) GOTO 10
      IF (COMEQV(LINE,'IN')) GOTO 30
      IF (COMEQV(LINE,'ENTER')) GOTO 30
20    CALL TONL
      CALL TNOUA('A large cloud of green smoke appears in ',40)
      CALL TNOU('front of you. It clears away',28)
      CALL TNOUA('to reveal a tall wizard, clothed in grey. ',42)
      CALL TNOU('He fixes you with a steely',26)
      CALL TNOUA('glare and declares, "This adventure has lasted ',47)
      CALL TNOU('too long." With that he',23)
      CALL TNOUA('makes a single pass over you with his hands, ',45)
      CALL TNOU('and everything around you',25)
      CALL TNOU('fades away into a grey nothingness.',35)
      CALL TONL
      RETURN
C*
30    CALL TONL
      CALL TNOU(
     +'You are inside the building, a well house for a large spring.',
     +61)
      CALL TONL
      CALL TNOU('There are some keys on the ground here.',39)
      CALL TNOU('There is a shiny brass lamp nearby.',35)
C*
40    CALL TONL
      CALL COMIN$(G$BLCM+G$PRNT+G$UPPR+G$JUST,'? ',2,
     +LINE,10,LEN,ITOKEN)
      IF (LEN.EQ.0) GOTO 40
      IF (WORD(1).EQ.PLUG) CALL PLUGH
      GOTO 20
      END
C*
C********************************************************************
C*
      SUBROUTINE CLOCK(TIME,DAYLEN,Z)
C*
      INTEGER*2 TIMES(4,48),TIME,DAYLEN,NOON,HOUR,Z
C*
      DATA TIMES /'Midnight00.30   01.00   01.30   02.00   02.30   ',
     +            '03.00   03.30   04.00   04.30   05.00   05.30   ',
     +            '06.00   06.30   07.00   07.30   08.00   08.30   ',
     +            '09.00   09.30   10.00   10.30   11.00   11.30   ',
     +            'Midday  12.30   13.00   13.30   14.00   14.30   ',
     +            '15.00   15.30   16.00   16.30   17.00   17.30   ',
     +            '18.00   18.30   19.00   19.30   20.00   20.30   ',
     +            '21.00   21.30   22.00   22.30   23.00   23.30   '/
C*
      NOON=(DAYLEN+1)/2
      HOUR=MOD((TIME-NOON+24),48)+1
      CALL TNOUA('It is ',6)
      CALL TNOUA(TIMES(1,HOUR),LENGTH(TIMES(1,HOUR),8))
      CALL TNOU('.',1)
      IF (HOUR.NE.25) GOTO 10
      IF (Z.GT.1) GOTO 10
      CALL TONL
      CALL TNOU(
     +'Mad dogs and Englishmen go out in the midday sun!',49)
10     RETURN
      END
C*
C***********************************************************************
C*
      SUBROUTINE DEAD(FLAGS,LUCK)

      IMPLICIT INTEGER*2 (A-Z)

#include "KEYS.INS.f"
#include "ERRD.INS.f"
#include "G_KEYS.INS.f"
#include "CHIMAERA.INS.f"
C*
C*    The player has been killed, offer him reincarnation.
C*
      INTEGER*4 DELAY   !  Delay for SLEEP$
      LOGICAL FLAGS(1),YES
      PARAMETER ( LIFE=9,UNDEAD=28
C*
     +)
      DELAY=2000
      CALL TONL
      CALL TNOUA('Sorry, you are ',15)
      IF (LUCK.LT.1) CALL TNOUA('permanently ',12)
      CALL TNOU('dead.',5)
      IF (LUCK.LT.1) GOTO 5
      CALL TONL
      CALL TNOU(
     +'I may be able to reincarnate you, shall I try?',46)
      CALL YESNO(YES)
      IF (YES) GOTO 10
5     FLAGS(LIFE)=.FALSE.
      CALL QUIT(FLAGS(LIFE),FLAGS(UNDEAD))
10    CALL TONL
      CALL TNOU(
     +'O.K., this may hurt a little and will cost',42)
      CALL TNOU(
     +'you 50 points!',14)
      CALL SLEEP$(DELAY)     !  Delay
      LUCK=LUCK-5
      RETURN
      END
C*
C***********************************************************************
C*
      SUBROUTINE INSTR(FIRST)     !  User instructions
      LOGICAL FIRST
      IF (.NOT.FIRST) GOTO 10
      CALL TONL
      FIRST=.FALSE.
10    CALL TNOU(
     +'  Use simple commands with one or two words to direct me.',57)
      CALL TNOU(
     +'Thus,  NORTH or GO NORTH or N will move you to the North,',57)
      CALL TNOU(
     +'assuming that you can go that way. Similarly, GET or TAKE',57)
      CALL TNOU(
     +'OBJECT will enable you to pick up OBJECT if it can be',53)
      CALL TNOU(
     +'carried provided that you have the strength to lift it. Some',60)
      CALL TNOU(
     +'other useful commands are INVENTORY, SCORE, TIME, QUIT, LOOK,',
     +61)
      CALL TNOU(
     +'WHERE, SAVE and RESTORE. Most commands can be abbreviated',57)
      CALL TNOU(
     +'to one or two letters, thus SW will do for SOUTHWEST, etc.',58)
      CALL TONL
      CALL TNOU(
     +'Try to get all the treasure you find into the tent at',53)
      CALL TNOU(
     +'your base camp.',15)
      CALL TONL
      CALL TNOU(
     +'Be careful, there may be some nasty surprises for you.',54)
      CALL TONL
      CALL TNOU(
     +'See how quickly you can become a Supreme Champion, there',56)
      CALL TNOU('is no maximum score.',20)
      CALL TONL
      CALL TNOU(
     +'Good luck, you''ll need it!!!',28)
      RETURN
      END
C*
C***********************************************************************
C*
      SUBROUTINE INVENT(HELD,OBJLOC)        !  Objects carried

      IMPLICIT INTEGER*2 (A-Z)

#include "CHIMAERA.INS.f"
C*
C*
      INTEGER*2 THINGS(20,H$OBJ),OBJLOC(1)
C*
      INTEGER*2 NTBASK(4)    ! These do not go in the basket
C*
      REAL*8 PSEUDO(H$PSEU)
C*
      COMMON/DESCRP/THINGS,PSEUDO
C*
      DATA NTBASK/3,6,10,19/
C*
      IF (HELD.GT.0) GOTO 10
      CALL TNOU(
     +'You are holding nothing!',24)
      RETURN
10    COUNT=0
      PENULT=HELD-1
      CALL TNOUA('You are carrying ',17)
      CHARS=17
      IF (OBJLOC(O$BASK).EQ.0) GOTO 100     ! He has the basket
C*
C*    He doesn't have the basket, list the objects
C*    in their normal order.
      DO 30 I=1,H$OBJ
         IF (OBJLOC(I).NE.0) GOTO 30        !  Not held
         COUNT=COUNT+1
         LEN=LENGTH(THINGS(1,I),20)
         CALL TNOUA(THINGS(1,I),LEN)
         CALL T1OU(160)
         CHARS=CHARS+LEN+1
C*
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
         LEN=LENGTH(THINGS(11,I),20)
         CALL TNOUA(THINGS(11,I),LEN)
         IF (COUNT.EQ.HELD) GOTO 200        ! All done
         IF (COUNT.EQ.PENULT) CALL TNOUA(' and ',5)
         IF (COUNT.LT.PENULT) CALL TNOUA(', ',2)
         CHARS=CHARS+LEN+6
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
30    CONTINUE
C*
C*
C*    He has the basket, list objects in the following order
C*    lamp, staff, sword, carpet, basket, others in order.
C*
100   DO 110 I=1,4
         PTR=NTBASK(I)
         IF (OBJLOC(PTR).NE.0) GOTO 110     ! Not got it
         COUNT=COUNT+1
         LEN=LENGTH(THINGS(1,PTR),20)
         CALL TNOUA(THINGS(1,PTR),LEN)
         CALL T1OU(160)
         CHARS=CHARS+LEN+1
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
         LEN=LENGTH(THINGS(11,PTR),20)
         CALL TNOUA(THINGS(11,PTR),LEN)
         IF (COUNT.EQ.HELD) GOTO 200
         IF (COUNT.LT.PENULT) CALL TNOUA(', ',2)
         CHARS=CHARS+LEN+2
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
110   CONTINUE
C*
      IF (COUNT.GT.0) CALL TNOUA(' and ',5)
      CHARS=CHARS+5
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('a small wicker ',15)
      CHARS=CHARS+15
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      CALL TNOUA('basket',6)
      COUNT=COUNT+1
      IF (COUNT.EQ.HELD) GOTO 200
      CHARS=CHARS+6
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
      IF (CHARS.GT.0) CALL T1OU(160)
      CALL TNOUA('containing ',11)
      CHARS=CHARS+11
      IF (CHARS.GE.58) CALL LNFEED(CHARS)
C*
      DO 120 I=1,H$OBJ
         IF (OBJLOC(I).NE.0) GOTO 120
         IF (I.EQ.3.OR.I.EQ.6.OR.I.EQ.10.OR.I.EQ.11.OR.I.EQ.19)
     +   GOTO 120            ! Already described
         COUNT=COUNT+1
         LEN=LENGTH(THINGS(1,I),20)
         CALL TNOUA(THINGS(1,I),LEN)
         CALL T1OU(160)
         CHARS=CHARS+LEN+1
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
         LEN=LENGTH(THINGS(11,I),20)
         CALL TNOUA(THINGS(11,I),LEN)
         IF (COUNT.EQ.HELD) GOTO 200
         IF (COUNT.LT.PENULT) CALL TNOUA(', ',2)
         IF (COUNT.EQ.PENULT) CALL TNOUA(' and ',5)
         CHARS=CHARS+LEN+5
         IF (CHARS.GE.58) CALL LNFEED(CHARS)
120   CONTINUE
C*
C*
200   CALL TNOU('.',1)
      RETURN
      END
C*
C***********************************************************************
C*
      SUBROUTINE LEXICN      !  Main dictionary for calculated text.

      IMPLICIT INTEGER*2 (A-Z)

#include "KEYS.INS.f"
#include "ERRD.INS.f"
#include "G_KEYS.INS.f"
#include "CHIMAERA.INS.f"
C*
C*
      REAL*8    DICACT(H$ACT),         !  Action verbs
     +          PSEUDO(H$PSEU)              ! Pseudo objects
C*
      INTEGER*2  VERBS(10,7),          !  Text verbs
     +           NOUNS(10,18),         !  Text nouns
     +           ADJONE(10,16),        !  First adjective
     +           ADJTWO(10,12),        !  Second adjective
     +           THINGS(20,H$OBJ)           !  Object descriptions
C*
      COMMON/DICTON/DICACT,VERBS,NOUNS,ADJONE,ADJTWO
      COMMON/DESCRP/THINGS,PSEUDO
C*
      DATA DICACT/'NORTH   SOUTH   EAST    WEST    NE      ',
     +            'NW      SE      SW      UP      DOWN    ',   ! 10
     +            'IN      OUT     ON      TAKE    KILL    ',
     +            'FREE    LOOK    CHASE                   ',   ! 20
     +            'EAT     MASTER  GO      THROW   WAVE    ',
     +            'STAMP   PUT     DROP    OFF     RUB     ',   ! 30
     +            'FILL    PLAY    READ    FIND    1$$$$   ',
     +            'INVENTORHELP    SCORE   QUIT    DRINK   ',   ! 40
     +            'OPEN    close   lock    UNLOCK  CALL    ',
     +            'INSTRUCTPLUGH   GAMIC   SLEEP   TIME    ',   ! 50
     +            'WHERE   ADVENTUREXAMINE                 ',
     +            '                SAVE    RESTORE ********',   ! 60
     +            'NORTH   SOUTH   EAST    WEST    NORTHEAS',
     +            'NORTHWESSOUTHEASSOUTHWESCLIMB   DESCEND ',   ! 70
     +            'ENTER   EXIT    LIGHT   GET     SLAY    ',
     +            'RELEASE         FOLLOW                  '/   ! 80
C*
C*    DICACT words in lower case have not been implemented yet
C*
      DATA VERBS/'standing in         ',
     +           'lying in            ',
     +           'walking through     ',
     +           'walking along       ',
     +           'crawling through    ',
     +           'crawling along      ',
     +           'climbing            '/
C*
      DATA NOUNS/'oubliette           ',
     +           'crypt               ',
     +           'dungeon             ',
     +           'cell                ',
     +           'alcove              ',
     +           'cavern              ',
     +           'grotto              ',
     +           'cave                ',
     +           'tunnel              ',
     +           'antechamber         ',
     +           'passageway          ',
     +           'hall                ',
     +           'corridor            ',
     +           'chamber             ',
     +           'passage             ',
     +           'room                ',
     +           'well                ',
     +           'shaft               '/
C*
      DATA ADJONE/'an impressive       ',
     +            'an ornate           ',
     +            'an arched           ',
     +            'an imposing         ',
     +            'a cramped           ',
     +            'a gigantic          ',
     +            'a tiny              ',
     +            'an enormous         ',
     +            'a low               ',
     +            'a high              ',
     +            'a narrow            ',
     +            'a wide              ',
     +            'a little            ',
     +            'a big               ',
     +            'a small             ',
     +            'a large             '/
C*
      DATA ADJTWO/' evil               ',
     +            ' smelly             ',
     +            '                    ',
     +            ' ominous            ',
     +            '                    ',
     +            ' dank               ',
     +            '                    ',
     +            ' sandy              ',
     +            '                    ',
     +            ' damp               ',
     +            '                    ',
     +            ' dry                '/
C*
      DATA THINGS/
     +'some tasty          food                ',     !  1
     +'a green glass       bottle              ',
     +'a tarnished brass   lamp                ',
     +'a little            plant               ',
     +'a bunch of          keys                ',     !  5
     +'a rusty bloodstainedsword               ',
     +'a polished black    rod                 ',
     +'a coil of           rope                ',
     +'a clove of          garlic              ',
     +'a long wooden       staff               ',     ! 10
     +'a small wicker      basket              ',
     +'a glistening        pearl               ',
     +'a handful of        coins               ',
     +'a golden            nugget              ',
     +'some bars of        silver              ',     ! 15
     +'a brilliant         diamond             ',
     +'some precious       jewels              ',
     +'a delicate Ming     vase                ',
     +'an ornate Persian   carpet              ',
     +'some bundles of     banknotes           ',     ! 20
     +'an illuminated      manuscript          ',
     +'a beautiful silver  flute               ',
     +'a dog-eared         book                ',
     +'some sheets of      music               ',
     +'a bunch of          violets             ',     ! 25
     +'a gem encrusted     box                 ',
     +'a platinum          pyramid             ',
     +'a richly embroideredcushion             ',
     +'a jewelled          dagger              ',
     +'a crystal           goblet              ',     ! 30
     +'a priceless         sapphire            ',
     +'a glittering        necklace            ',
     +'a princely          sceptre             ',
     +'a small chest of    treasure            ',
     +'a blood-red         ruby                ',     ! 35
     +'a good luck         charm               ',
     +'an emerald          bracelet            ',
     +'an enormous         oyster              ',
     +'a giant             clam                ',
     +'the                 elixir of life      ',     ! 40
     +'a highly polished   mirror              ',
     +'some scattered      brickbats           ',
     +'a pottery           gnome               ',
     +'a block of          serpentine          ',
     +'a statue of the     Venus de Milo       '/     ! 45
C*
      DATA PSEUDO/
     +'camp    tent    base    water   servant ',
     +'                                        '/
      RETURN
      END
C*
C***********************************************************************
C*
      SUBROUTINE LNFEED(CHARS)
      INTEGER*2 CHARS
      CALL TONL
      CHARS=0
      RETURN
      END
C*
C***********************************************************************
C*
      SUBROUTINE LOCATE(X,Y,Z,HERE)
#include "KEYS.INS.f"
#include "ERRD.INS.f"
#include "G_KEYS.INS.f"
#include "CHIMAERA.INS.f"
C*
C*    This routine calculates an integer*2 number from the X,Y and Z
C*    co-ordinates.
C*
      INTEGER*2 X,Y,Z,HERE
      INTEGER*4 XL,YL,ZL
C*
      XL=X
      YL=Y
      ZL=Z
      HERE=RT(XL*YL*ZL+XL*17+YL*19+ZL*23,15)
      RETURN
      END
C*
C***********************************************************************
C*
      SUBROUTINE LOCWUN(OBJLOC)

      IMPLICIT INTEGER*2 (A-Z)

#include "CHIMAERA.INS.f"
C*    Put each object into a unique initial positions, different
C*    in each game. N.B. Only objects 11 - H$OBJ cannot be seen at
C*    ground level until they have been placed by the player.
C*
C*
      INTEGER*2 OBJLOC(1)
C*
C*    Fill the array with negative numbers appropriate to the
C*    frequency with which the objects will be found. The first
C*    ten of them may be found on the surface, probability
C*    20 to 1, all in different positions.
C*
      DO 10 I=1,10
         OBJLOC(I)=I-15 !  Range -5 to -14
10    CONTINUE
C*
C*    Now scramble them
C*
      DO 20 I=1,100
         A=RANDOM(10)+1
         B=RANDOM(10)+1
         C=OBJLOC(A)
         OBJLOC(A)=OBJLOC(B)
         OBJLOC(B)=C
20    CONTINUE
C*
C*    The rest are only found underground at 60 to 1.
C*
      DO 30 I=11,H$OBJ
         OBJLOC(I)=I-65 !  Range - 54 to (H$OBJ-65)
30    CONTINUE
C*
C*    Scramble some of these (about 20).
C*
      POINT=H$OBJ-10
      DO 40 I=1,20
         A=RANDOM(POINT)+11
         B=RANDOM(POINT)+11
         C=OBJLOC(A)
         OBJLOC(A)=OBJLOC(B)
         OBJLOC(B)=C
40    CONTINUE
C*
C*    Special objects which do not exist yet
C*
      OBJLOC(O$DIAM)=-2000   !  Diamond
      OBJLOC(O$BRIC)=-2000   !  Brickbats
      OBJLOC(O$GNOM)=-2000   !  Gnome
      OBJLOC(O$SERP)=-2000   !  Serpentine
      OBJLOC(O$VENU)=-2000   !  Venus-de-Milo
C*
      RETURN
      END
C*
C***********************************************************************
C*
      SUBROUTINE LUCKY(LUCK)
C*
      INTEGER*2 LUCK
C*
      IF (LUCK.GE.30) RETURN
C*
      CALL TONL
      IF (LUCK.LE.5)  GOTO 10
      IF (LUCK.LT.10) GOTO 20
      IF (LUCK.LT.20) GOTO 30
      IF (LUCK.LT.30) GOTO 40
C*
10    CALL TNOU('You are almost out of luck!',27)
      RETURN
20    CALL TNOU('Your luck won''t hold out for ever.',34)
      RETURN
30    CALL TNOU('You are pushing your luck.',26)
      RETURN
40    CALL TNOU('It must be your lucky day.',26)
      RETURN
      END
C*
C***********************************************************************
C*
      SUBROUTINE PARSE       !  Input parsing routine

      IMPLICIT INTEGER*2(A-Z)

#include "KEYS.INS.f"
#include "ERRD.INS.f"
#include "G_KEYS.INS.f"
#include "CHIMAERA.INS.f"
C*
C*
      INTEGER*2 LINE(66),    !  Input buffer
     +          LEN,         !  LINE length
     +          TOKNO,       !  Tokens on line
     +          BUFFER(8),   !  TOKEN$ buffer
     +          LITACT(8),        !  Literal form of action verb
     +          LITOBJ(8)         !  Literal form of object noun
C*
      REAL*8 ACTION,OBJECT
C*
      COMMON/INPUT/LINE,LEN,TOKNO,ACTION,LITACT,OBJECT,LITOBJ
C*
      CALL SFIL$R(LITACT,8)
      CALL SFIL$R(LITOBJ,8)
      CALL SFIL$R(ACTION,4)
      CALL SFIL$R(OBJECT,4)
C*
      POINT=0
      LOOP=1
      IF (TOKNO.GE.2) LOOP=2
      DO 10 I=1,LOOP
         CALL TOKEN$(G$DFLT,LINE,LEN,BUFFER,16,POINT,TRULEN)
         IF (TRULEN.GT.16) TRULEN=16
         IF (I.EQ.1) CALL MOVE$R(1,BUFFER,1,LITACT,TRULEN)
         IF (I.EQ.2) CALL MOVE$R(1,BUFFER,1,LITOBJ,TRULEN)
10    CONTINUE
      CALL MOVE$R(1,LITACT,1,ACTION,8)
      CALL MOVE$R(1,LITOBJ,1,OBJECT,8)
C*    CALL UPCASE(ACTION,8)
C*    CALL UPCASE(OBJECT,8)
C*
      RETURN
      END
C*
C************************************************************************
C*
      SUBROUTINE PLUGH
C*    Simulates PLUGH from ADVENTURE
#include "G_KEYS.INS.f"
      INTEGER*2 LINE(8),RANDOM,RAND
      INTEGER*2 KOUNT
      REAL*8 WORD(2),PLUG
      EQUIVALENCE (LINE(1),WORD(1))
      PLUG='PLUGH   '
      KOUNT=0
C*
      CALL TONL
      CALL TNOUA('There is a brilliant flash of light ',36)
      CALL TNOU('and a sudden fanfare of trumpets!',33)
      CALL TNOUA('When your eyes recover from the flash, ',39)
      CALL TNOU('you find that:',14)
      CALL TNOUA('You are in a large room, with a passage ',40)
      CALL TNOU('to the south, a passage to the',30)
      CALL TNOUA('west, and a wall of broken rock to the east. ',45)
      CALL TNOU('There is a large "Y2" on',24)
      CALL TNOU('a rock in the room''s centre.',28)
C*
10    KOUNT=KOUNT+1     ! Increment counter.
      IF (KOUNT.GT.20) GOTO 40
      CALL TONL
      CALL COMIN$(G$BLCM+G$PRNT+G$JUST+G$UPPR,'? ',2,
     +LINE,10,LEN,ITOKEN)
      IF (LEN.EQ.0) GOTO 10
      IF (WORD(1).EQ.PLUG) GOTO 30
      IF (LINE(1).EQ.'Y2') GOTO 20
      CALL TONL
      RAND=RANDOM(8)
      IF (RAND.EQ.3) GOTO 12
      IF (RAND.EQ.5) GOTO 14
      IF (RAND.EQ.7) GOTO 16
      CALL TNOU('Pardon?',7)
      GOTO 10
C*
12    CALL TNOU('Sorry, I don''t understand that here.',36)
      GOTO 10
C*
14    CALL TNOUA('I''m afraid that I am temporarily ',33)
      CALL TNOU('suffering from partial amnesia.',31)
      GOTO 10
C*
16    CALL TNOUA('If at first you don''t succeed, ',31)
      CALL TNOU('try, try, try again!',20)
      GOTO 10
C*
20    CALL TONL
      CALL TNOU('That''s where you are now!',25)
      GOTO 10
C*
30    CALL TONL
      CALL TNOU('OK!',3)
      RETURN
C*
C*    The player has been here for more than 20 more and is
C*    obviously in trouble, give him a hint.
C*
40    CALL TONL
      CALL TNOU(
     +'You seem to be stuck here, but keep plugging away at',52)
      CALL TNOU(
     +'it and you will eventually work out how to escape.',50)
      KOUNT=KOUNT-5
      GOTO 10
C*
      END
C*
C********************************************************************
C*
      SUBROUTINE PLUMB(HERE,X,Y,Z,OBJLOC)

      IMPLICIT INTEGER*2 (A-Z)

#include "CHIMAERA.INS.f"
C*    Try to find the bottom of a shaft
      INTEGER*2 OBJLOC(1)
C*
      ZNEXT=Z
      THIS=HERE
C*
10    ZNEXT=ZNEXT+1
      IF (ZNEXT.EQ.100) GOTO 20   ! Bottomless pit, give up.
      CALL LOCATE(X,Y,ZNEXT,DOWN)
      IF (AND(THIS,DOWN,128).NE.128) GOTO 20     ! Found bottom
      THIS=DOWN
      GOTO 10
C*
C*    Found bottom (or given up at level 100), locate
C*    dropped objects there.
C*
20    DO 30 I=1,H$OBJ
         IF (OBJLOC(I).EQ.HERE) OBJLOC(I)=THIS
30    CONTINUE
      RETURN
      END
C*
C*******************************************************************
C*
      SUBROUTINE POINTR(SEED,IPT,SIZE)

      IMPLICIT INTEGER*2 (A-Z)

C*    This routine produces a random series of unique pointers from
C*    1 to SIZE in IPT using the SEED provided.
C*
      INTEGER*2 IPT(SIZE)
C*     Preload array
      DO 10 I=1,SIZE
         IPT(I)=I
10    CONTINUE
C*    Seed random number generator
      CALL RND(SEED)
C*    Scramble them
      TOP=SIZE
      DO 20 I=2,SIZE
         RAND=RANDOM(TOP)+1
         TEMP=IPT(TOP)
         IPT(TOP)=IPT(RAND)
         IPT(RAND)=TEMP
         TOP=TOP-1
20    CONTINUE
      RETURN
      END
C*
C***********************************************************************
C*
      SUBROUTINE QUIT(LIFE,UNDEAD)          !  Terminates program

      IMPLICIT INTEGER*2 (A-Z)

#include "KEYS.INS.f"
#include "ERRD.INS.f"
#include "G_KEYS.INS.f"
#include "CHIMAERA.INS.f"
C*
C*
      INTEGER*2 SCORE,MOVENO
      INTEGER*2 IWORD(10,10),
     +          POINTS(H$OBJ)     !  Object scores
C*
      LOGICAL LIFE,UNDEAD,YES
      LOGICAL ALLDUN(H$OBJ)
C*
      COMMON/SCORES/SCORE,MOVENO,ALLDUN,POINTS
C*
      DATA IWORD/'a novice            an apprentice       ',
     +           'a student           a graduate          ',
     +           'a third class       a second class      ',
     +           'a first class       a master            ',
     +           'a grand master      a supreme champion  '/
C*
      IF (.NOT.LIFE) GOTO 10
      CALL TONL
      CALL TNOU(
     +'Are you sure that you want to quit now?',39)
      CALL YESNO(YES)
      IF (.NOT.YES) RETURN
      CALL TONL
      CALL TNOU('Very well.',10)
10    CALL TONL
      CALL SCORIT(SCORAL)
      IF (UNDEAD) SCORAL=0
      CALL TNOUA('You scored ',11)
      CALL TOVFD$(SCORAL)
      CALL TNOUA(' points in ',11)
      CALL TOVFD$(MOVENO)
      CALL TNOU(' moves.',7)
      CALL TONL
      IF (SCORAL.LT.100) GOTO 20
      CALL TNOUA(
     +'Your score qualifies you as ',28)
      IPT=SCORAL/100
      CALL TNOUA(IWORD(1,IPT),LENGTH(IWORD(1,IPT),20))
      CALL TNOU(' adventurer.',12)
      CALL TONL
      IF (IPT.EQ.10) CALL TNOU('Congratulations!!',17)
      GOTO 40
20    IF (UNDEAD) GOTO 30
      CALL TNOU(
     +'You are obviously a rank amateur!',33)
      GOTO 40
30    CALL TNOU('(N.B. Vampires don''t score!)',28)
40    CALL TONL
      CALL EXIT
      END
C*
C***********************************************************************
C*
      INTEGER*2 FUNCTION RANDOM(N)
      INTEGER*2 N
      REAL*4 RND
C*    Returns a random number in the range 0 - (N-1).
      RANDOM=INTS(RND(0)*N)
      RETURN
      END
C*
C***********************************************************************
C*
      SUBROUTINE RESTOR(MOVENO,SCORE,X,Y,Z,VALUES,FLAGS,OBJLOC,
     +MONSTR)
C*    Subroutine to restore a game of CHIMAERA.
C*
#include "KEYS.INS.f"
#include "ERRD.INS.f"
#include "G_KEYS.INS.f"
#include "CHIMAERA.INS.f"
C*
      IMPLICIT INTEGER*2 (A-Z)
C*
      LOGICAL   YES
C*
      INTEGER*2 VALUES(H$VAL),
     +          OBJLOC(H$OBJ),
     +          MONSTR(H$MON),
     +          DATE(5)
C*
      COMMON/DMPCOM/PRFX(2),DLMTR(2),PWRD(3),TREE(33)
C*
      CALL DMPOPN(K$READ,PRFX,10,DLMTR,PWRD,TREE,UNIT,ICD)
      IF (ICD.EQ.0) GOTO 10
      CALL TNOU('Sorry... restore not possible.',30)
      CALL ZFIL$R(TREE,33)
      RETURN
C*
10    CALL PRWF$$(K$READ,UNIT,LOC(DATE),5,000000,NWD,ICD)       ! Skip date sav
      CALL TIMDAT(DATE,5)
      CALL PRWF$$(K$WRIT,UNIT,LOC(DATE),5,000000,NWD,ICD)       ! Save date rea
      CALL PRWF$$(K$READ,UNIT,LOC(VALUES),H$VAL,000000,NWD,ICD)
      CALL PRWF$$(K$READ,UNIT,LOC(OBJLOC),H$OBJ,000000,NWD,ICD)
      CALL PRWF$$(K$READ,UNIT,LOC(MONSTR),H$MON,000000,NWD,ICD)
      CALL PRWF$$(K$READ,UNIT,LOC(FLAGS),H$FLAG,000000,NWD,ICD)
      CALL SRCH$$(K$CLOS,0,0,UNIT,0,JCD)
      IF (ICD.EQ.0) GOTO 20
      CALL TNOU('Oops!.. That game is corrupt.',29)
      CALL ZFIL$R(TREE,33)
      MOVENO=-999
      RETURN
C*
20    MOVENO=VALUES(1)
      SCORE=VALUES(2)
      X=VALUES(3)
      Y=VALUES(4)
      Z=VALUES(5)
      CALL TNOUA('Do you want to keep this saved game? ',37)
      CALL YESNO(YES)
      IF (YES) GOTO 30
      DATE(1)=0
      DATE(2)=TREE(1)
      CALL TSRC$$(K$DELE,TREE(2),UNIT,DATE,ITP,ICD)
      CALL TONL
C*
30    CALL ZFIL$R(TREE,33)
      RETURN
C*
      END
C*
C***********************************************************************
C*
      SUBROUTINE SAVE(VALUES,FLAGS,OBJLOC,MONSTR)

      IMPLICIT INTEGER*2 (A-Z)

C*    Subroutine to save a game of CHIMAERA.
C*
#include "KEYS.INS.f"
#include "ERRD.INS.f"
#include "G_KEYS.INS.f"
#include "CHIMAERA.INS.f"
C*
C*
      INTEGER*2 DATE(5),          ! TIMDAT buffer
     +          VALUES(H$VAL),
     +          OBJLOC(H$OBJ),
     +          MONSTR(H$MON)
C*
      LOGICAL FLAGS(H$FLAG)
C*
      COMMON/DMPCOM/PRFX(2),DLMTR(2),PWRD(3),TREE(33)
C*
      CALL DMPOPN(K$RDWR,PRFX,10,DLMTR,PWRD,TREE,UNIT,ICD)
      IF (ICD.EQ.0) GOTO 10
      CALL TNOU('Sorry... ',9)
      RETURN
C*
10    CALL TIMDAT(DATE,5)
      CALL PRWF$$(K$WRIT,UNIT,LOC(DATE),5,000000,NWD,ICD)
      CALL ZFIL$R(DATE,5)
      CALL PRWF$$(K$WRIT,UNIT,LOC(DATE),5,000000,NWD,ICD)
      CALL PRWF$$(K$WRIT,UNIT,LOC(VALUES),H$VAL,000000,NWD,ICD)
      CALL PRWF$$(K$WRIT,UNIT,LOC(OBJLOC),H$OBJ,000000,NWD,ICD)
      CALL PRWF$$(K$WRIT,UNIT,LOC(MONSTR),H$MON,000000,NWD,ICD)
      CALL PRWF$$(K$WRIT,UNIT,LOC(FLAGS),H$FLAG,000000,NWD,ICD)
      CALL SRCH$$(K$CLOS,0,0,UNIT,0,ICD)
      CALL TONL
      CALL TNOU('Done!',5)
      CALL TONL
      CALL QUIT(.FALSE.,FLAGS(28))
      END
C*
C***********************************************************************
C*
      SUBROUTINE SCORIT(SCORAL)   !  Current score.

      IMPLICIT INTEGER*2 (A-Z)

#include "CHIMAERA.INS.f"
C*
      INTEGER*2 SCORE,MOVENO
      INTEGER*2 POINTS(H$OBJ)     ! Object scores
C*
      LOGICAL ALLDUN(H$OBJ)
C*
      COMMON/SCORES/SCORE,MOVENO,ALLDUN,POINTS
C*
      SCORAL=SCORE
      DO 10 I=1,H$OBJ
         IF (ALLDUN(I)) SCORAL=SCORAL+POINTS(I)
10    CONTINUE
C*
      RETURN
      END
C*
C***********************************************************************
C*
      SUBROUTINE WORTH       ! Load object value array
#include "CHIMAERA.INS.f"
C*
      INTEGER*2 SCORE,MOVENO
      INTEGER*2 POINTS(H$OBJ)     ! Object scores
C*
      LOGICAL ALLDUN(H$OBJ)
C*
      COMMON/SCORES/SCORE,MOVENO,ALLDUN,POINTS
C*
C*    Common objects (1-10) all score 5 points
C*
      DO 10 I=1,11
         POINTS(I)=5
10    CONTINUE
C*
C*    Uncommon objects (11-H$OBJ) score 20 points
C*
      DO 20 I=12,H$OBJ
         POINTS(I)=20
20    CONTINUE
C*
C*    Certain valuable objects score more
C*
      POINTS(14)=25     ! Gold nugget
      POINTS(16)=30     ! Diamond
      POINTS(17)=40     ! Jewels
      POINTS(27)=45     ! Platinum pyramid
      POINTS(31)=50     ! Priceless sapphire
      POINTS(34)=50     ! Treasure
      POINTS(45)=40     ! Venus-de-Milo
C*
C*    and some are virtually worthless (for points)
C*
      POINTS(9)=2       ! Garlic
      POINTS(23)=2      ! Book
      POINTS(25)=2      ! Violets
      POINTS(36)=10     ! Good luck charm
      POINTS(38)=2      ! Oyster
      POINTS(39)=2      ! Clam
      POINTS(40)=2      ! Elixir of life
      POINTS(42)=0      ! Brickbats
      POINTS(43)=0      ! Gnome
C*
      RETURN
      END
C*
C***********************************************************************
C*
      SUBROUTINE YESNO(YES)  !  YES or NO response

      IMPLICIT INTEGER*2 (A-Z)

#include "KEYS.INS.f"
#include "ERRD.INS.f"
#include "G_KEYS.INS.f"
C*
      LOGICAL YES,COMEQV
      REAL*8 REPLY
      YES=.FALSE.
10    CALL TONL
      CALL COMIN$(G$PRNT+G$JUST+G$UPPR,'> ',2,REPLY,8,LEN,TOKNO)
      IF (TOKNO.EQ.0) GOTO 15
      IF (COMEQV(REPLY,'YES')) GOTO 20
      IF (COMEQV(REPLY,'NO')) GOTO 30
15    CALL TONL
      CALL TNOU('Please answer YES or NO!',24)
      GOTO 10
20    YES=.TRUE.
30    RETURN
      END
C*
C**************************************************************************
C*
      SUBROUTINE DMPOPN(KEY,PRFX,NAMLEN,DLMTR,PWRD,TREE,FUNIT,CODE)
C*
      INTEGER*2 KEY               ! Action key
      INTEGER*2 PRFX(2)           ! Game specific name prefix (PL1 format)
      INTEGER*2 NAMLEN            ! Upper limit for login name length
      INTEGER*2 DLMTR(2)          ! Login name/dump name delimiter (PL1)
      INTEGER*2 PWRD(2)           ! Password for GMS*>SAVED (PL1)
      INTEGER*2 TREE(33)          ! Treename to be constructed
      INTEGER*2 FUNIT             ! Returned file unit
      INTEGER*2 CODE              ! Returned error code
C*
#include "KEYS.INS.f"
#include "ERRD.INS.f"
#include "G_KEYS.INS.f"
C*
      INTEGER*2 BUF(28)           ! General buffer
      INTEGER*2 LEN               ! Length
      INTEGER*2 CNT               ! Token count
      INTEGER*2 ITP               ! File type
      INTEGER*2 POS               ! Character pointer
      INTEGER*4 CHRPOS            ! Character position
C*
      POS=3
C*      CALL MOVE$R(1,'GMS*>SAVED ',3,TREE,11)
C*      CALL MOVE$R(3,PWRD,14,TREE,PWRD)
C*      POS=14+PWRD(1)
C*      CALL MOVE$R(1,'>',POS,TREE,1)
C*      POS=POS+1
      CALL MOVE$R(3,PRFX,POS,TREE,PRFX)
      POS=POS+PRFX(1)
      CALL TIMDAT(BUF,28)
      LEN=LENGTH(BUF(13),16)
      IF (LEN.GT.NAMLEN) LEN=NAMLEN
      CALL MOVE$R(25,BUF,POS,TREE,LEN)
      POS=POS+LEN
      CALL MOVE$R(3,DLMTR,POS,TREE,DLMTR)
      POS=POS+DLMTR(1)
      CALL COMIN$(G$UPPR+G$JUST+G$PRNT,'Name of saved game: ',20,
     +     BUF,12,LEN,CNT)
      CODE=E$BNAM
      IF (LEN.LT.1.OR.LEN.GT.23.OR.CNT.GT.1) GOTO 10
      CALL MOVE$R(1,BUF,POS,TREE,LEN)
      POS=LEN+POS-3
C*
      CHRPOS=POS
      IF (KEY.NE.K$READ) GOTO 5
      CALL TSRC$$(K$EXST,TREE(2),FUNIT,CHRPOS,ITP,ICD)
      CHRPOS=POS
      IF (ICD.EQ.0) GOTO 5
      CALL TNOU('You have no such saved game.',28)
      RETURN
5     CALL TSRC$$(K$RDWR+K$GETU,TREE(2),FUNIT,CHRPOS,ITP,CODE)
10    CALL ERRPR$(K$IRTN,CODE,TREE(2),CHRPOS,0,0)
      IF (KEY.EQ.K$READ) TREE(1)=POS
      IF (KEY.NE.K$READ) CALL ZFIL$R(TREE,33)
      RETURN
      END
C*
