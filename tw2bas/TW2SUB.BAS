	REM **/ TW 2 SUB /** MADE SPECIFICALLY FOR TW 2 (500T) TO PASS QB3'S
						  'SMALLER' SEGMENT LIMIT

	'$INCLUDE: 'TWVAR.BAS'

   DEF FNTI! = CSNG(FIX((VAL(MID$(TIME$,1,2))*60*60) + _
			   (VAL(MID$(TIME$,4,2))*60)+(VAL(MID$(TIME$,7,2))*1)))

   REM **/ Adjust a name A$ to make it right with caps /**
   SUB NAM.ADJ(NAME$) STATIC
   AX = 0
   FOR LX = 1 TO LEN(NAME$)
	 BX = ASC(MID$(NAME$,LX,1))
	 IF AX = 0 AND BX > 96 AND BX < 123 THEN
		   MID$(NAME$,LX,1) = CHR$(BX - 32)
	 ELSEIF AX = 1 AND BX > 64 AND BX < 91 THEN
		   MID$(NAME$,LX,1) = CHR$(BX + 32)
	 END IF
	 AX = 1
	 IF BX < 65 OR (BX > 90 AND BX < 96) OR BX > 123 THEN _
		AX = 0
   NEXT
   END SUB

   REM **/ Add message to TWOPENG.DAT /**
   SUB TWOPENG.ADD(MESS$) STATIC
   OPEN "TWOPENG.DAT" FOR OUTPUT AS #5
   PRINT #5, ""
   PRINT #5, MESS$
   CLOSE 5
   END SUB

   REM **/ Add message (MESS$) to messages to SysOp /**
   SUB SYSOP.MESSAGE(MESS$) STATIC
   GET 1,1
   IF CVI(FP$) = 1 THEN _
	  EXIT SUB
   OPEN "TWMESG.DAT" FOR APPEND AS #5
   PRINT #5, MESS$
   CLOSE 5
   END SUB

   REM **/ V = RND(A) /**
   SUB RAND(V,A) STATIC
   V = INT(RND(1)*A) + 1
   END SUB

   REM **/ Add blanks to make string STRNG$ length LENGTH /**
   SUB ADD.SPACE(STRNG$,LENGTH) STATIC
   IF LEN(STRNG$) < LENGTH THEN _
	  STRNG$ = SPACE$(LENGTH - LEN(STRNG$)) + STRNG$
   END SUB

   REM **/ Print a string X.LN$ to user /**
   SUB X.LNPRINT(X.LN$,SNOOP,L.OCAL) STATIC
   IF SNOOP OR L.OCAL = 1 THEN _
	  PRINT X.LN$
   IF L.OCAL <> 1 THEN
	  PRINT #3, X.LN$
	  PRINT #3, LF$;
   END IF
   END SUB

   REM **/ Print a string S.LN$ to user (no C/R)/**
   SUB S.LNPRINT(S.LN$,SNOOP,L.OCAL) STATIC
   IF SNOOP OR L.OCAL = 1 THEN _
	  PRINT S.LN$;
   IF L.OCAL <> 1 THEN _
	  PRINT #3, S.LN$;
   END SUB

   REM **/ Capitalize everything /**
   SUB UPLOW(ARG$) STATIC
   CONV$ = ARG$
   FOR Z = 1 TO LEN(CONV$)
	  IF MID$(CONV$,Z,1) > "@" THEN
		 MID$(CONV$,Z,1) = CHR$(ASC(MID$(CONV$,Z,1)) AND 223)
	  END IF
   NEXT
   ARG$ = CONV$
   END SUB

   REM **/ Print info on line 25 /**
   SUB LINE25 STATIC
   COLOR 11,1
   LOCATE 25,1
   PRINT FIRST$ + " " + LAST$; _
		 TAB(32);DOOR.NAME$; _
		 TAB(55)"Node: " + RD$; _
		 TAB(70);ON.AT$ + " "
   COLOR 7,0
   END SUB

   REM **/ create POINTS.DOR file /**
   SUB OUTDOOR(MON.SUB$,RD$,ORIG.POINTS#,POINTS#,NO.MONITOR) STATIC
   IF NO.MONITOR = 1 THEN _    ' EXIT IF RUNNING WITHOUT DOOR MONITOR
	  EXIT SUB
   OPEN MON.SUB$ + POINTS.FILE$ FOR OUTPUT AS #2
   MIDSEC! = FNTI!
   NEW.POINTS# = POINTS# - ORIG.POINTS#
   PRINT #2, MIDSEC!
   PRINT #2, NEW.POINTS#
   CLOSE 2
   END SUB

   SUB OPENDATA STATIC
   CLOSE 1
   OPEN"R",1,"TWDATA.DAT",143
   CALL ASSIGN.FIELDS
   END SUB

   SUB ASSIGN.FIELDS STATIC
   FIELD 1,41 AS FA$, _
			2 AS FB$, _
			2 AS FC$, _
			2 AS FD$, _
			2 AS FE$, _
			2 AS FF$, _
			2 AS FG$, _
			2 AS FH$, _
			2 AS FI$, _
			2 AS FJ$, _
			2 AS FK$, _
			2 AS FL$, _
			2 AS FM$, _
			2 AS FR$, _
			2 AS FO$, _
			2 AS FP$, _
			2 AS FQ$, _
			2 AS FT$, _
			4 AS FV$
   GET 1,1
   AY = CVI(FC$)
   TT = CVI(FD$)
   LP = CVI(FE$)
   LS = CVI(FF$)
   LT = CVI(FG$)
   LL = CVI(FO$)
   END SUB

   SUB Read.command STATIC
	 DEF.NAME$ = "RBBS-PC.DEF"
	 RD$ = COMMAND$
	 IF LEN(RD$) = 0 THEN _
		RD$ = "-"
	 NODE = INSTR("-1234567890",LEFT$(RD$,1))
	 IF NODE < 2 THEN NODE = 2
	 NODE.ID$ = STR$(NODE - 1)
	 IF LEN(RD$) > 2 AND MID$(RD$,2,1) = " " THEN
		DEF.NAME$ = MID$(RD$,3)
		GOTO Read.end
	 END IF
	 MID$(DEF.NAME$,5,1) = RD$
	 TIME.OFF.FILE$ = "TIMEOFF" + RD$ + ".DOR"
	 POINTS.FILE$ = "POINTS" + RD$ + ".DOR"
Read.end:
	 IF RD$ = "-" THEN                'RIG IT FOR THE HOKEY WAY MONI WORKS
		TIME.OFF.FILE$ = "TIMEOFF0.DOR"
		POINTS.FILE$ = "POINTS0.DOR"
		RD$ = "1"
	 END IF
	 END SUB

SUB Forceoff(TIME.OFF,WARNING,TIME.SAVE) STATIC                       '*** F-9 FORCE OFF ***
	 TEMP.STRING$ = STRING$(15,"*")
	 PRINT "SysOp, how many minutes till user is forced off ["; _
		   INT((TIME.OFF-FNTI!)/60);"] ";
	 INPUT MIN.F.O$
	 IF MIN.F.O$ = "" THEN _
		EXIT SUB
	 FORCE.OFF = VAL(MIN.F.O$)
	 X.LN$ = "     " + TEMP.STRING$ + _
		   "> IMPORTANT MESSAGE <" + _
		   TEMP.STRING$
	 CALL X.lnprint(X.LN$,SNOOP,L.OCAL)
	 X.LN$ = SYSOP.FIRST$ + _
		   " " + _
		   SYSOP.LAST$ + _
		   " needs to use " + _
		   RBBS.NAME$ + _
		   "!  It will be closing down"
	 CALL X.lnprint(X.LN$,SNOOP,L.OCAL)
	 X.LN$ = "in" + _
		   STR$(FORCE.OFF) + _
		   " minutes!  Please complete what you are doing within that time"
	 CALL X.lnprint(X.LN$,SNOOP,L.OCAL)
	 X.LN$ = "     " + TEMP.STRING$ + _
		   ">   END OF MESSAGE  <" + _
		   TEMP.STRING$
	 CALL X.lnprint(X.LN$,SNOOP,L.OCAL)
	 TIME.OFF = FNTI! + (FORCE.OFF*60)
	 WARNING = TIME.OFF - (3*60)
	 IF TIME.SAVE < 5 THEN _
		TIME.SAVE = 5
	 END SUB

	 SUB Read.def(SYSOP.FIRST$,SYSOP.LAST$,MESSAGE.FILE$,COM.PORT$,RBBS.NAME$,MCR,MSR) STATIC
	  OPEN DEF.NAME$ FOR INPUT AS #1
	  INPUT #1, Q$,Q$,Q$,SYSOP.FIRST$,SYSOP.LAST$,Q,Q,Q,Q!,Q,Q,Q$,Q,Q,Q,Q,Q,Q$,Q$,Q$,Q$,Q$,Q$,Q,Q,Q,Q,Q,Q,Q$
	  INPUT #1, MESSAGE.FILE$,Q$,Q$,Q$,Q$,Q$,Q$,Q$,COM.PORT$,Q,Q$,Q$,Q,Q,Q,Q,Q$,Q$,Q
	  INPUT #1, Q$,Q$,Q$,Q$,Q$,Q$,Q$,Q$,Q$,Q,Q,Q,MAX.NODES,NET.TYPE,Q,Q,Q,Q$
	  INPUT #1, Q,Q,Q,Q$,Q,Q$,Q,Q,Q,Q,Q,Q,Q,Q,Q$,Q,Q,Q,Q,Q
	  INPUT #1, Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q$
	  INPUT #1, Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q!,Q,Q,RBBS.NAME$
	  CLOSE 1                              '** Q, Q$, Q!   dummy variables **
	  IF COM.PORT$ = "COM1" THEN
		 MCR = &H3FC
		 MSR = &H3FE
		 EXIT SUB
	  END IF
	  MCR = &H2FC
	  MSR = &H2FE
	  END SUB

	 SUB GET.D(D,AY) STATIC
	 Z$ = DATE$
	 Y = VAL(RIGHT$(Z$,2))                     'Year
	 M = VAL(LEFT$(Z$,2))                      'Month
	 D = VAL(MID$(Z$,4,2))                     'Day
	 IF Y/4 = INT(Y/4) AND M > 2 THEN _
		D = D + 1                              'Leap Year, and past Feb. 29
	 IF M = 2 THEN
		  D = D + 31
		ELSEIF M = 3 THEN
		  D = D + 59
		ELSEIF M = 4 THEN
		  D = D + 90                           'add days for what month it is
		ELSEIF M = 5 THEN
		  D = D + 120
		ELSEIF M = 6 THEN
		  D = D + 151
		ELSEIF M = 7 THEN
		  D = D + 181
		ELSEIF M = 8 THEN
		  D = D + 212
		ELSEIF M = 9 THEN
		  D = D + 243
		ELSEIF M = 10 THEN
		  D = D + 273
		ELSEIF M = 11 THEN
		  D = D + 304
		ELSEIF M = 12 THEN
		  D = D + 334
	 END IF
	 IF Y < AY THEN _
		Y = Y + 100                            'Past the end of the decade
	 IF Y <> AY THEN
		FOR A = AY TO Y - 1
			D = D + 365
			IF A/4 = INT(A/4) THEN _
			   D = D + 1                       'Add leap years past.
			IF Y <> AY THEN _
			   EXIT FOR
		NEXT A
	 END IF
	 END SUB

	 REM **/ Rank players with FT$ and FV$.  P = starting person /**
	 SUB RANK.PLAYERS(P) STATIC
	 DOT = 0
2500 FOR L = 2 TO LP
	   DOT = DOT + 1
	   GET 1,L
	   IF CVI(FM$) = 0 THEN
		  LSET FV$ = MKS$(-1)
		  PUT 1,L
		  GOTO 2530
	   END IF
	   IF CVI(FC$) <> 0 THEN
		  LSET FV$ = MKS$(0)
		  PUT 1,L
		  GOTO 2530
	   END IF
	   G0! = CVI(FG$)
	   H0! = CVI(FH$)
	   F0! = CVI(FI$)
	   J0! = CVI(FJ$)
	   K0! = CVI(FK$)
	   L0! = CVI(FL$)
	   V! = G0!*100 + _
			H0!*500 + _
			F0!*10 + _
			J0!*20 + _
			K0!*35 + _
			L0!
	   LSET FV$ = MKS$(V!)
	   PUT 1,L
2530   IF (DOT MOD 20) = 0 AND (RANK <> 1) THEN _
		  CALL S.LNPRINT(".",SNOOP,L.OCAL)
	 NEXT
	 DOT = 0
	 FOR L = LP + 1 TO LS
	   DOT = DOT + 1
	   GET 1,L
	   IF CVI(FL$) = 0 OR CVI(FM$) < 2 THEN _
		  2560
	   A = CVI(FL$)
	   P = CVI(FM$)
	   GET 1,P
	   LSET FV$ = MKS$(CVS(FV$) + A*100)
	   PUT 1,P
2560 IF (DOT MOD 20) = 0 AND (RANK <> 1) THEN _
		CALL S.LNPRINT(".",SNOOP,L.OCAL)
	 NEXT
	 DOT = 0
	 P = 0
	 FOR L = 2 TO LP
	   DOT = DOT + 1
	   GET 1,L
	   V! = CVS(FV$)
	   IF V! = -1 THEN _
		  2620
	   N = P
	   O = 0
	   IF P = 0 THEN
		  P = L
		  LSET FT$ = MKI$(-1)
		  PUT 1,L
		  GOTO 2620
	   END IF
2580   GET 1,N
	   IF V! > CVS(FV$) AND O = 0 THEN
		  GET 1,L
		  LSET FT$ = MKI$(P)
		  PUT 1,L
		  P = L
		  GOTO 2620
	   END IF
	   IF V! > CVS(FV$) THEN
		  GET 1,O
		  C = CVI(FT$)
		  LSET FT$ = MKI$(L)
		  PUT 1,O
		  GET 1,L
		  LSET FT$ = MKI$(C)
		  PUT 1,L
		  GOTO 2620
	   END IF
	   IF CVI(FT$) = -1 THEN
		  GET 1,N
		  LSET FT$ = MKI$(L)
		  PUT 1,N
		  GET 1,L
		  LSET FT$ = MKI$(-1)
		  PUT 1,L
		  GOTO 2620
	   END IF
	   O = N
	   N = CVI(FT$)
	   GOTO 2580
2620 IF (DOT MOD 20) = 0 AND (RANK <> 1) THEN _
		CALL S.LNPRINT(".",SNOOP,L.OCAL)
	 NEXT
	 END SUB

	 REM **/ Rank Teams and put in two arrays for processing /**
	 SUB TEAM.RANK STATIC
	 FOR TEAM = 1 TO CVI(FT$)
		 TEAM.NUM(TEAM) = TEAM
		 FOR TIX = 2 TO LP
			 GET 1, TIX
			 IF CVI(FE$) = 1 THEN
				IF CVI(FR$) = TEAM THEN
				   TEAM.TOTAL#(TEAM) = TEAM.TOTAL#(TEAM) + CVS(FV$)
				END IF
			 END IF
		 NEXT
	 NEXT
	 GET 1,1
	 FOR TEAM = 1 TO CVI(FT$)
		 FOR TIX = 1 TO CVI(FT$)
			IF TEAM.TOTAL#(TIX) < TEAM.TOTAL#(TEAM) THEN
			   TEMP# = TEAM.TOTAL#(TIX)
			   TEAM.TOTAL#(TIX) = TEAM.TOTAL#(TEAM)
			   TEAM.TOTAL#(TEAM) = TEMP#
			   TEAM.TEMP = TEAM.NUM(TIX)
			   TEAM.NUM(TIX) = TEAM.NUM(TEAM)
			   TEAM.NUM(TEAM) = TEAM.TEMP
			END IF
		 NEXT
	 NEXT
	 END SUB

	 SUB TWANSIO(O.PENFILE$,L.OCAL) STATIC
	 OPEN O.PENFILE$ FOR INPUT AS #1
	 OPEN "CON" FOR OUTPUT AS #5
	 WHILE (NOFSX$ <> "Q") AND (NOT EOF(1))
		LINE INPUT #1,A$
		PRINT #5, A$
		IF L.OCAL <> 1 THEN
		   PRINT #3, A$
		   PRINT #3, LF$
		END IF
	 WEND
	 CLOSE #1
	 CLOSE #5
	 END SUB

	 REM **/ Add a message to player P: Player PN destroyed N fighters /**
	 SUB MESS.TO.PLAYER(N,P,PN) STATIC
6000 OPEN "R",5,"TWPMSG.DAT",8
	 FIELD 5,2 AS GA$, _
			 2 AS GB$, _
			 2 AS GC$, _
			 2 AS GD$
	 GET 5,1
	 A = CVI(GA$)
	 B = CVI(GB$)
	 IF B > 1 THEN _
		GET 5,B
	 IF CVI(GA$) = P AND CVI(GB$) = 1 AND CVI(GC$) = PN THEN
		LSET GD$ = MKI$(CVI(GD$) + N)
		PUT 5,B
		GOTO 6050
	 END IF
	 L = A + 1
	 GET 5,1
	 LSET GA$ = MKI$(L)
	 PUT 5,1
	 LSET GA$ = MKI$(P)
	 LSET GB$ = MKI$(1)
	 LSET GC$ = MKI$(PN)
	 LSET GD$ = MKI$(N)
	 PUT 5,L
	 GET 5,1
	 LSET GB$ = MKI$(L)
	 PUT 5,1
6050 CLOSE 5
	 END SUB

	 SUB RADIO.MESSAGE(P,PN,R.MSG$) STATIC
	 OPEN "TWRMSG.DAT" FOR RANDOM AS #5 LEN = 80
	 FIELD 5, 2 AS HA$, _
			  2 AS HB$, _
			  2 AS HC$, _
			 74 AS HD$
	 GET 5,1
	 LRM = CVI(HA$)
	 ER = LRM + 1
	 FOR B = 2 TO LRM
		 GET 5,B
		 IF CVI(HA$) = 0 THEN
			ER = B
			EXIT FOR
		 END IF
	 NEXT
	 LSET HA$ = MKI$(1)
	 LSET HB$ = MKI$(P)
	 LSET HC$ = MKI$(PN)
	 LSET HD$ = R.MSG$
	 PUT 5,ER
	 IF ER > LRM THEN
		GET 5,1
		LSET HA$ = MKI$(ER)
		PUT 5,1
	 END IF
	 CLOSE 5
	 END SUB

	 REM **/ Read Radio Messages (if any) /**
	 SUB READ.RADIO(PN,SNOOP,L.OCAL) STATIC
	 MSG = 0
	 A$ = ""
	 CALL X.LNPRINT(A$,SNOOP,L.OCAL)
	 A$ = "Checking for Radio Messages sent to you."
	 CALL X.LNPRINT(A$,SNOOP,L.OCAL)
	 OPEN "TWRMSG.DAT" FOR RANDOM AS #5 LEN = 80
	 FIELD 5, 2 AS HA$, _
			  2 AS HB$, _
			  2 AS HC$, _
			 74 AS HD$
	 GET 5,1
	 ER = CVI(HA$)
	 FOR RM = 2 TO ER
		 GET 5,RM
		 IF (CVI(HA$) = 1) AND (CVI(HB$) = PN) THEN
			MSG = 1
			A$ = ""
			CALL X.LNPRINT(A$,SNOOP,L.OCAL)
			GET 1,CVI(HC$)
			A$ = "Message From " + LEFT$(FA$,CVI(FM$)) + ":"
			CALL X.LNPRINT(A$,SNOOP,L.OCAL)
			A$ = HD$
			CALL X.LNPRINT(A$,SNOOP,L.OCAL)
			LSET HA$ = MKI$(0)
			PUT 5,RM
		 END IF
	 NEXT
	 IF MSG = 0 THEN
		A$ = "None Received."
		CALL X.LNPRINT(A$,SNOOP,L.OCAL)
	 END IF
	 CLOSE 5
	 END SUB

	  REM **/ Find shortest route from A to B in S(500,1) /**
	  SUB FIND.WARP(A,B,M.AINT) STATIC
	  N = 1
	  C = B
	  IF A = B THEN
		 S(0,0) = A
		 S(0,1) = 0
		 S(A,1) = 0
		 EXIT SUB
	  END IF
	  FOR L = 1 TO (LS - LP)
		FOR M = 0 TO 1
		  S(L,M) = 0
		NEXT
	  NEXT
	  S(A,1) = 1
	  DOT = 0
7530  FOR L = 1 TO (LS - LP)
		DOT = DOT + 1
		IF S(L,1) <> N THEN _
		   7580
		GET 1,L + LP
		E(1) = CVI(FB$)
		E(2) = CVI(FC$)
		E(3) = CVI(FD$)
		E(4) = CVI(FE$)
		E(5) = CVI(FF$)
		E(6) = CVI(FG$)
		FOR M = 1 TO 6
		  IF E(M) <> 0 THEN
			 IF S(E(M),1) = 0 THEN
				S(E(M),1) = N + 1
				S(E(M),0) = L
			 END IF
			 IF E(M) = B THEN _
				7590
		  END IF
		NEXT
		IF ((DOT MOD 15) = 0 AND M.AINT = 0) THEN _
		   CALL S.LNPRINT(".",SNOOP,L.OCAL)
7580  NEXT
	  N = N + 1
	  IF N < 9999 THEN _
		 7530
	  A$ = "*** Error - Sector path not found - from sector" + _
		   STR$(A) + _
		   " to sector" + _
		   STR$(B)
	  CALL SYSOP.MESSAGE(A$)
	  CALL X.LNPRINT(A$,SNOOP,L.OCAL)
	  S(A,1) = 0
	  EXIT SUB
7590  S(S(C,0),1) = C
	  C = S(C,0)
	  IF S(C,0) <> 0 THEN _
		 7590
	  S(B,1) = 0
	  END SUB

	  REM **/ Compress Player/Radio Data Base /**
	  SUB MSG.COMPRESS STATIC
	  CALL X.LNPRINT("",SNOOP,L.OCAL)
	  CALL X.LNPRINT("Compressing Message Base's",SNOOP,L.OCAL)
	  OPEN "TWPMSG.DAT" FOR RANDOM AS #5 LEN = 8
	  FIELD 5,2 AS GA$, _
			  2 AS GB$, _
			  2 AS GC$, _
			  2 AS GD$
	  GET 5,1
	  P = 2
	  FOR L = 2 TO CVI(GA$)
		GET 5,L
		IF CVI(GB$) <> 0 THEN
		   PUT 5,P
		   P = P + 1
		END IF
	  NEXT
	  P = P - 1
	  GET 5,1
	  LSET GA$ = MKI$(P)
	  LSET GB$ = MKI$(0)
	  PUT 5,1
	  CLOSE 5
	  OPEN "TWRMSG.DAT" FOR RANDOM AS #5 LEN = 80
	  FIELD 5, 2 AS HA$, _
			   2 AS HB$, _
			   2 AS HC$, _
			  74 AS HD$
	  GET 5,1
	  P = 2
	  R = CVI(HA$)
	  FOR M = 2 TO R
		  GET 5,M
		  IF CVI(HA$) <> 0 THEN
			 PUT 5,P
			 P = P + 1
		  END IF
	  NEXT
	  P = P - 1
	  GET 5,1
	  LSET HA$ = MKI$(P)
	  PUT 5,1
	  CLOSE 5
	  END SUB

	  REM **/ Add ship P person-in-sector chain /**
	  SUB ADD.P(P,END.NOW) STATIC
15000 GET 1,P
	  R = CVI(FF$)
	  IF R = 0 THEN _
		 EXIT SUB
	  FOR L = 2 TO LP
		GET 1,L
		IF CVI(FP$) = 0 THEN _
		   GOTO 15050
	  NEXT
	  A$ = "ERROR IN PLACING PEOPLE IN SECTOR - NOT ENOUGH ROOM!, ADD.P SUB"
	  CALL SYSOP.MESSAGE(A$)
	  CALL X.LNPRINT(A$,SNOOP,L.OCAL)
	  END.NOW = 1
	  EXIT SUB
15050 GET 1,LP + R
	  A = CVI(FI$)
	  IF L = A THEN _
		 A = 0
	  LSET FI$ = MKI$(L)
	  PUT 1,LP + R
	  GET 1,L
	  LSET FO$ = MKI$(A)
	  LSET FP$ = MKI$(P)
	  PUT 1,L
	  END SUB

	  REM **/ Remove ship P from person-in-sector chain /**
	  SUB REM.SHIP(P) STATIC
15100 GET 1,P
	  R = CVI(FF$)
	  GET 1,LP + R
	  A = CVI(FI$)
	  IF A = 0 THEN _
		 EXIT SUB
	  GET 1,A
	  IF CVI(FP$) = P THEN
		 B = CVI(FO$)
		 LSET FP$ = MKI$(0)
		 LSET FO$ = MKI$(0)
		 PUT 1,A
		 GET 1,LP + R
		 LSET FI$ = MKI$(B)
		 PUT 1,LP + R
		 EXIT SUB
	  END IF
15130 L = CVI(FO$)
	  IF L = 0 THEN _
		 EXIT SUB
	  GET 1,L
	  IF CVI(FP$) = P THEN
		 B = CVI(FO$)
		 LSET FP$ = MKI$(0)
		 LSET FO$ = MKI$(0)
		 PUT 1,L
		 GET 1,A
		 LSET FO$ = MKI$(B)
		 PUT 1,A
		 EXIT SUB
	  END IF
	  A = L
	  GOTO 15130
	  END SUB

	  REM **/ Player PN kills ship P (Delete it) /**
	  SUB DEL.P(M.AINT,P,PN) STATIC
15200 CALL REM.SHIP(P)
	  GET 1,P
	  LSET FC$ = MKI$(PN)
	  LSET FF$ = MKI$(0)
	  PUT 1,P
	  FOR L = LP + 1 TO LS
		GET 1,L
		IF CVI(FM$) = P THEN
		   LSET FM$ = MKI$(0)
		   LSET FL$ = MKI$(0)
		   PUT 1,L
		END IF
	  NEXT L
	  GET 1,P
	  IF CVI(FE$) = 1 THEN
		 Z = CVI(FR$)
		 GET 1,(LT + Z)
		 Z1 = CVI(FT$)
		 LSET FT$ = MKI$(Z1 - 1)
		 IF (Z1 - 1) < 1 THEN
			LSET FQ$ = MKI$(0)
		 END IF
		 PUT 1,(LT + Z)
		 GET 1,P
		 LSET FE$ = MKI$(0)
		 LSET FR$ = MKI$(0)
		 PUT 1,P
	  END IF
	  IF M.AINT = 1 THEN _
		 EXIT SUB
	  GET 1,PN
	  A$ = "  -  " + _
		   LEFT$(FA$,CVI(FM$)) + _
		   " killed "
	  GET 1,P
	  A$ = A$ + LEFT$(FA$,CVI(FM$))
	  CALL SYSOP.MESSAGE(A$)
	  END SUB

	  REM **/ Delete player P from game /**
	  SUB DEL.PLAYER(A$,P,PN) STATIC
34230 GET 1,P
	  A$ = "Deleting " + LEFT$(FA$,CVI(FM$)) + "..."
	  CALL X.LNPRINT(A$,SNOOP,L.OCAL)
	  CALL REM.SHIP(P)
	  IF CVI(FE$) = 1 THEN
		 Z = CVI(FR$)
		 GET 1, (LT + Z)
		 Z1 = CVI(FT$)
		 LSET FT$ = MKI$(Z1 - 1)
		 IF (Z1 - 1) < 1 THEN _
			LSET FQ$ = MKI$(0)
		 PUT 1, (LT + Z)
	  END IF
	  GET 1,P
	  LSET FM$ = MKI$(0)
	  LSET FE$ = MKI$(0)
	  LSET FR$ = MKI$(0)
	  PUT 1,P
	  FOR L = LP + 1 TO LS
		GET 1,L
		IF CVI(FM$) = P THEN
		   LSET FM$ = MKI$(0)
		   LSET FL$ = MKI$(0)
		   PUT 1,L
		END IF
	  NEXT
	  OPEN "TWPMSG.DAT" FOR RANDOM AS #5 LEN = 8
	  FIELD 5,2 AS GA$, _
			  2 AS GB$, _
			  2 AS GC$, _
			  2 AS GD$
	  GET 5,1
	  LSET GB$ = MKI$(0)
	  PUT 5,1
	  FOR L = 2 TO CVI(GA$)
		GET 5,L
		IF CVI(GA$) = P THEN
		   LSET GA$ = MKI$(0)
		   LSET GB$ = MKI$(0)
		   LSET GC$ = MKI$(0)
		   LSET GD$ = MKI$(0)
		   PUT 5,L
		END IF
		IF CVI(GB$) = 1 AND CVI(GC$) = P THEN
		   LSET GC$ = MKI$(-98)
		   PUT 5,L
		END IF
	  NEXT
	  CLOSE 5
	  OPEN "TWRMSG.DAT" FOR RANDOM AS #5 LEN = 80
	  FIELD 5, 2 AS HA$, _
			   2 AS HB$, _
			   2 AS HC$, _
			  74 AS HD$
	  GET 5,1
	  R = CVI(HA$)
	  FOR L = 1 TO R
		  GET 5,L
		  IF CVI(HB$) = P THEN
			 LSET HA$ = MKI$(0)
			 PUT 5,L
		  END IF
	  NEXT
	  CLOSE 5
	  FOR L = 2 TO LP
		GET 1,L
		IF CVI(FC$) = P THEN
		   LSET FC$ = MKI$(-98)
		   PUT 1,L
		END IF
	  NEXT
	  END SUB

	  REM **/ Move group Cabal (group G) from sector A to sector B (Next to each other) /**
	  SUB MOVE.CABAL(A,B,G) STATIC
	  CALL S.LNPRINT(".",SNOOP,L.OCAL)
	  IF (A < 1) OR (B < 1) OR (A > LS - LP) OR (B > LS - LP) OR (A = B) THEN _
		 EXIT SUB
35090 N = G(G,1)
	  GET 1,A + LP
	  IF CVI(FM$) <> -1 THEN
		 G(G,0) = 0
		 G(G,1) = 0
		 EXIT SUB
	  END IF
	  IF CVI(FL$) <= N THEN
		 N = CVI(FL$)
		 G(G,1) = N
		 LSET FL$ = MKI$(0)
		 LSET FM$ = MKI$(0)
		 PUT 1,A + LP
		 GOTO 35140
	  END IF
	  IF CVI(FL$) > N THEN
		 LSET FL$ = MKI$(CVI(FL$) - N)
		 PUT 1,A+LP
	  END IF
35140 G(G,0) = B
	  GET 1,B + LP
	  IF CVI(FL$) = 0 THEN
		 LSET FL$ = MKI$(N)
		 LSET FM$ = MKI$(-1)
		 PUT 1,B + LP
		 GOTO 35230
	  END IF
	  P = CVI(FM$)
	  IF P = -1 THEN
		 LSET FL$ = MKI$(CVI(FL$) + N)
		 PUT 1,B + LP
		 GOTO 35230
	  END IF
	  L = 0
	  K = 0
	  GET 1,B + LP
35170 A = 2
	  CALL RAND(V,A)
	  IF V = 1 THEN _
		 L = L + 1 ELSE _
		 K = K + 1
	  IF L < CVI(FL$) AND K < G(G,1) THEN _
		 35170
	  A$ = "      Group" + STR$(G) + " --> Sector" + STR$(B) + "("
	  GET 1,P
	  A$ = A$ + LEFT$(FA$,CVI(FM$)) + _
		   "): lost" + STR$(K) + ", dstrd" + STR$(L) + " ("
	  GET 1,B + LP
	  IF L >= CVI(FL$) THEN _
		 35220
	  G(G,0) = 0
	  G(G,1) = 0
	  LSET FL$ = MKI$(CVI(FL$) - L)
	  PUT 1,B + LP
	  A$ = A$ + "Cabal ftrs dstrd)"
	  CALL SYSOP.MESSAGE(A$)
	  GOTO 35230
35220 LSET FL$ = MKI$(N - K)
	  LSET FM$ = MKI$(-1)
	  PUT 1,B + LP
	  N = N - K
	  G(G,1) = N
	  A$ = A$ + "Player ftrs dstrd)"
	  CALL SYSOP.MESSAGE(A$)
35230 END SUB

	  REM **/ Cabal (group G) attack player P with F fighters (if possible) /**
	  SUB CABAL.ATTACK(M.AINT,P,F,G) STATIC
	  CALL S.LNPRINT(".",SNOOP,L.OCAL)
35240 IF F > G(G,1) THEN _
		 F = G(G,1)
	  IF P < 1 OR P > LP THEN _
		 EXIT SUB
	  C13 = G(G,0) + LP
	  GET 1,C13
	  IF CVI(FM$) <> -1 OR F < 1 THEN _
		 EXIT SUB
	  GET 1,P
	  IF CVI(FF$) <> C13 - LP THEN _
		 EXIT SUB
	  R = 0
	  K = 0
35300 A = 2
	  CALL RAND(V,A)
	  IF V = 1 THEN _
		 R = R + 1 ELSE _
		 K = K + 1
	  IF R <= CVI(FG$) AND K < F THEN _
		 35300
	  G(G,1) = G(G,1) - K
	  GET 1,C13
	  LSET FL$ = MKI$(G(G,1))                  'G(G,1) = Cabal fghtrs in sctr
	  PUT 1,C13
	  IF G(G,1) < 1 THEN
		 LSET FM$ = MKI$(0)
		 LSET FL$ = MKI$(0)
		 PUT 1,C13
		 G(G,0) = 0
		 G(G,1) = 0
	  END IF
	  GET 1,P
	  F = CVI(FG$) - R
	  N = R
	  R13 = R
	  PN = -1
	  CALL MESS.TO.PLAYER(N,P,PN)
	  IF F > 0 THEN
		 GET 1,P
		 LSET FG$ = MKI$(F)
		 PUT 1,P
	   ELSE CALL DEL.P(M.AINT,P,PN)
	  END IF
	  A$ = "      Group" + STR$(G) + " --> "
	  GET 1,P
	  A$ = A$ + LEFT$(FA$,CVI(FM$)) + _
		   ": lost" + STR$(K) + ", dstrd" + STR$(R13)
	  IF G(G,0) = 0 THEN
		 A$ = A$ + " (Cabal dstrd)"
		 CALL SYSOP.MESSAGE(A$)
	   ELSE GET 1,P
		 IF CVI(FC$) = -1 THEN
			A$ = A$ + " (Player dstrd)"
			CALL SYSOP.MESSAGE(A$)
		  ELSE CALL SYSOP.MESSAGE(A$)
		 END IF
	  END IF
	  END SUB

	  REM **/ Pick a random sector as a goal and put it in V - Uses A /**
	  SUB PICK.RAND(V,A) STATIC
	  A = 3
	  CALL RAND(V,A)
	  IF V <> 1 THEN
		 A = (LS - LP)
		 CALL RAND(V,A)
		 EXIT SUB
	  END IF
	  A = 6
	  CALL RAND(V,A)
	  IF V = 1 THEN
		 V = 80
	   ELSEIF V = 2 THEN
		 V = 81
	   ELSEIF V = 3 THEN
		 V = 84
	   ELSEIF V = 4 THEN
		 V = 82
	   ELSEIF V = 5 THEN
		 V = 71
	   ELSE V = 86                                ' Locate around the base
	  END IF
	  END SUB

  REM **/ Initialize player's ship /**
	  SUB INIT.SHIP(PD,PN,END.NOW) STATIC
	  CALL X.LNPRINT(" ",SNOOP,L.OCAL)
	  CALL X.LNPRINT("Your ship is being initialized.",SNOOP,L.OCAL)
	  P = PN
	  CALL REM.SHIP(P)
	  GET 1,1
	  A = CVI(FH$)
	  B = CVI(FI$)
	  C = CVI(FJ$)
	  GET 1,PN
	  LSET FB$ = MKI$(PD)
	  LSET FC$ = MKI$(0)
	  LSET FD$ = MKI$(TT)
	  LSET FE$ = MKI$(0)
	  LSET FF$ = MKI$(1)
	  LSET FG$ = MKI$(A)
	  LSET FH$ = MKI$(C)
	  LSET FI$ = MKI$(0)
	  LSET FJ$ = MKI$(0)
	  LSET FK$ = MKI$(0)
	  LSET FL$ = MKI$(B)
	  LSET FR$ = MKI$(0)
	  PUT 1,PN
	  P = PN
	  CALL ADD.P(P,END.NOW)
	  END SUB

 REM **/ Rank Players and create rankings file /**
	SUB Twrank(FIL$) STATIC
	CALL OPENDATA
Rank.Write:
64001 OPEN FIL$ FOR OUTPUT AS #4
	  RANK = 1
	  PRINT #4, ""
	  PRINT #4, "  T R A D E W A R S   I I - 500T   S C O R E B O A R D  "
	  PRINT #4, ""
	  PRINT #4, "Last updated at: "+DATE$+" "+TIME$
	  PRINT #4, ""
	  CALL RANK.PLAYERS(P)
	  PRINT #4, "Player Rankings"
	  PRINT #4, "Rank     Value      Team   Player"
	  PRINT #4, "==== ============= ====== ================"
	  R = 1
	  FOR COUNT = 1 TO 10
		GET 1,P
		B$ = STR$(R)
		L = 4
		CALL ADD.SPACE(B$,L)
		A$ = B$
		B$ = STR$(CVS(FV$))
		L = 14
		CALL ADD.SPACE(B$,L)
		A$ = A$ + B$
		IF CVI(FE$) = 1 THEN
		   B$ = STR$(CVI(FR$))
		   L = 5
		   CALL ADD.SPACE(B$,L)
		 ELSE B$ = SPACE$(5)
		END IF
		A$ = A$ + B$
		A$ = A$ + "   " + LEFT$(FA$,CVI(FM$))
		PRINT #4, A$
		P = CVI(FT$)
		IF P = -1 THEN _
		   EXIT FOR
		R = R + 1
	  NEXT COUNT
	  PRINT #4, ""
	  PRINT #4, "Team Rankings"
	  PRINT #4, "Rank     Value      Team"
	  PRINT #4, "==== ============= ======"
	  CALL TEAM.RANK
	  GET 1,1
	  FOR TEAM = 1 TO CVI(FT$)
		  IF TEAM.TOTAL#(TEAM) > 0 THEN
			 B$ = STR$(TEAM)
			 L = 4
			 CALL ADD.SPACE(B$,L)
			 A$ = B$
			 B$ = STR$(TEAM.TOTAL#(TEAM))
			 L = 14
			 CALL ADD.SPACE(B$,L)
			 A$ = A$ + B$
			 B$ = STR$(TEAM.NUM(TEAM))
			 L = 4
			 CALL ADD.SPACE(B$,L)
			 A$ = A$ + B$
			 PRINT #4, A$
		  END IF
	  NEXT
Rank.End:
	  CLOSE 1,4
	  END SUB

	  SUB GET.TW2.DAT(FIL$,LOG.OFF$,ALIAS,NM.TIME) STATIC
	  OPEN "TW2.DAT" FOR INPUT AS #2
	  INPUT #2, FIL$
	  INPUT #2, LOG.OFF$
	  INPUT #2, ALIAS
	  INPUT #2, NM.TIME
	  CLOSE 2
	  END SUB

	  SUB GET.ALIAS(FIRST$,LAST$,FOUND) STATIC
	  REAL.NAME$ = FIRST$ + " " + LAST$
	  OPEN "TWNAME.DAT" FOR INPUT AS #5
	  WHILE (NOT EOF(5))
		INPUT #5, A1$, A2$, A3$, A4$
		IF REAL.NAME$ = A1$ + " " + A2$ THEN
		   CLOSE 5
		   FIRST$ = A3$
		   LAST$ = A4$
		   FOUND = 1
		   EXIT SUB
		END IF
	  WEND
	  CLOSE 5
	  END SUB

	  SUB CHECK.ALIAS(FULL.ALIAS$,FOUND) STATIC
	  OPEN "TWNAME.DAT" FOR INPUT AS #5
	  WHILE (NOT EOF(5))
		 INPUT #5, A1$, A2$, A3$, A4$
		 IF FULL.ALIAS$ = A3$ + " " + A4$ THEN
			FOUND = 1
			CLOSE 5
			EXIT SUB
		 END IF
	  WEND
	  CLOSE 5
	  FOUND = 0
	  END SUB

	  SUB PUT.ALIAS(FIRST$,LAST$,ALIAS.FIRST$,ALIAS.LAST$) STATIC
	  OPEN "TWNAME.DAT" FOR APPEND AS #5
	  NEW.ALIAS$ = FIRST$ + "," + LAST$ + "," + _
				   ALIAS.FIRST$ + "," + ALIAS.LAST$
	  PRINT #5, NEW.ALIAS$
	  CLOSE 5
	  END SUB

	 SUB ANSI.COLOR(FC,BC,BLINK,HIGH,L.OCAL) STATIC
	 IF FC = BC THEN _
		BC = 0
	 AC$ = CHR$(27) + "[3"

	 LFC = FC                                     'THIS SECTION FOR LOCAL
	 IF HIGH = 1 THEN                             'SCREEN
		LFC = LFC + 8
	 END IF
	 IF BLINK = 1 THEN
		LFC = LFC + 16
	 END IF
	 COLOR C.OLOR(LFC),C.OLOR(BC)
	 IF L.OCAL = 1 THEN
		BLINK = 0
		HIGH = 0
		EXIT SUB
	 END IF

	 REMOTE.ANSI$ = AC$ + MID$(STR$(FC),2,1) + _  'FROM HERE ON OUT REMOTE
					";4" + MID$(STR$(BC),2,1)     'USER ROUTINE
	 IF BLINK = 1 THEN _
		REMOTE.ANSI$ = REMOTE.ANSI$ + ";5"
	 IF HIGH = 1 THEN _
		REMOTE.ANSI$ = REMOTE.ANSI$ + ";1"
	 REMOTE.ANSI$ = REMOTE.ANSI$ + "m"
	 PRINT #3, REMOTE.ANSI$;
	 BLINK = 0
	 HIGH = 0
	 END SUB

	 SUB ANSI.COL STATIC               'SET LOCAL 'ANSI' COLORS INTO ARRAYS
	 C.OLOR(0)  = 0  'black
	 C.OLOR(1)  = 4  'red
	 C.OLOR(2)  = 2  'green
	 C.OLOR(3)  = 6  'yellow (brown)
	 C.OLOR(4)  = 1  'blue
	 C.OLOR(5)  = 5  'magenta
	 C.OLOR(6)  = 3  'cyan
	 C.OLOR(7)  = 7  'white
	 C.OLOR(8)  = 8  'grey
	 C.OLOR(9)  = 12 'light red
	 C.OLOR(10) = 10 'light green
	 C.OLOR(11) = 14 'yellow
	 C.OLOR(12) = 9  'light blue
	 C.OLOR(13) = 13 'light magenta
	 C.OLOR(14) = 11 'light cyan
	 C.OLOR(15) = 15 'white
	 C.OLOR(16) = 16 'black      (blink)
	 C.OLOR(17) = 20 'red        (blink)
	 C.OLOR(18) = 18 'green      (blink)
	 C.OLOR(19) = 22 'yellow     (blink)
	 C.OLOR(20) = 17 'blue       (blink)
	 C.OLOR(21) = 21 'magenta    (blink)
	 C.OLOR(22) = 19 'cyan       (blink)
	 C.OLOR(23) = 23 'white      (blink)
	 C.OLOR(24) = 24 'grey       (blink)
	 C.OLOR(25) = 28 'light red  (blink)
	 C.OLOR(26) = 26 'lght green (blink)
	 C.OLOR(27) = 30 'yellow     (blink)
	 C.OLOR(28) = 25 'light blue (blink)
	 C.OLOR(29) = 29 'lt magenta (blink)
	 C.OLOR(30) = 27 'lt cyan    (blink)
	 C.OLOR(31) = 31 'white      (blink)
	 END SUB



