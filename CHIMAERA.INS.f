C*    CHIMERA.INS.f  Definitions and keys for CHIMERA
C*
C*
      INTEGER*2 H$ACT,H$MOD,H$OBJ,H$EXP,H$FLAG,H$VAL,H$MON,
     +          H$PSEU
C*
      INTEGER*2 O$FOOD,O$BOTT,O$LAMP,O$PLAN,O$KEYS,O$SWOR,
     +          O$ROD ,O$ROPE,O$GARL,O$STAF,O$BASK,O$PEAR,
     +          O$COIN,O$NUGG,O$SILV,O$DIAM,O$JEWE,O$VASE,
     +          O$CARP,O$BANK,O$MANU,O$FLUT,O$BOOK,O$MUSI,
     +          O$VIOL,O$BOX ,O$PYRA,O$CUSH,O$DAGG,O$GOBL,
     +          O$SAPP,O$NECK,O$SEPT,O$TREA,O$RUBY,O$CHAR,
     +          O$BRAC,O$OYST,O$CLAM,O$ELIX,O$MIRR,O$BRIC,
     +          O$GNOM,O$SERP,O$VENU
C*
      PARAMETER (
     +  H$ACT=80,     !  Current number of actions
     +  H$MOD=60,     !  Current value of action MOD  (H$ACT-20)
     +  H$OBJ=45,     !  Number of objects in database
     +  H$EXP=15,     !  Number of expletives
     +  H$FLAG=50,    !  Dimension of FLAGS array
     +  H$VAL=50,     !  Dimension of VALUES array
     +  H$MON=8,      !  Number of monsters
     +  H$PSEU=10     !  Number of pseudo objects
C*
C*
     +)
      PARAMETER ( O$FOOD= 1,
     +          O$BOTT= 2,
     +          O$LAMP= 3,
     +          O$PLAN= 4,
     +          O$KEYS= 5,
     +          O$SWOR= 6,
     +          O$ROD = 7,
     +          O$ROPE= 8,
     +          O$GARL= 9,
     +          O$STAF=10,
     +          O$BASK=11,
     +          O$PEAR=12,
     +          O$COIN=13,
     +          O$NUGG=14,
     +          O$SILV=15,
     +          O$DIAM=16,
     +          O$JEWE=17,
     +          O$VASE=18,
     +          O$CARP=19,
     +          O$BANK=20,
     +          O$MANU=21,
     +          O$FLUT=22,
     +          O$BOOK=23,
     +          O$MUSI=24,
     +          O$VIOL=25,
     +          O$BOX =26,
     +          O$PYRA=27,
     +          O$CUSH=28,
     +          O$DAGG=29,
     +          O$GOBL=30,
     +          O$SAPP=31,
     +          O$NECK=32,
     +          O$SEPT=33,
     +          O$TREA=34,
     +          O$RUBY=35,
     +          O$CHAR=36,
     +          O$BRAC=37,
     +          O$OYST=38,
     +          O$CLAM=39,
     +          O$ELIX=40,
     +          O$MIRR=41,
     +          O$BRIC=42,
     +          O$GNOM=43,
     +          O$SERP=44,
     +          O$VENU=45
     +)
