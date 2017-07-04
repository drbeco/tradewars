  ' $INCLUDE: 'TWMERG1.BAS'
  ' **/ Trade Wars, by Chris Sherrick (PTL) 6/86 /**
  ' **/ Copyright 1986 Chris Sherrick /**
  ' **/ 200, 500, Team versions by John Morris /**
  M.AINT = 0
  DOOR.NAME$ = "TradeWars v.ii"
  R.ELEASE$ = "2.0.TW2_500T"
  END.NOW = 0
  RANK = 0
  CALL NAM.ADJ(FIRST$)
  CALL NAM.ADJ(LAST$)
  ON.AT$ = TIME$
  IF (SNOOP OR L.OCAL = 1) THEN _
	 CALL LINE25
  DIM M(3), _
	  P$(3), _
	  B(3), _
	  N(3), _
	  P(3), _
	  C(3), _
	  H(3)
   P$(1) = "Ore......."
   P$(2) = "Organics.."
   P$(3) = "Equipment."
   B(1) = 10
   B(2) = 20
   B(3) = 35
   FS$ = "TWDATA.DAT"
   GOTO 10000
60 REM **/ Return to BBS /**
   CLOSE 1
   GOSUB OUTDOOR
   A$ = "Returning to Door monitor..."
   IF NO.MONITOR = 1 THEN _
      A$ = "Returning to RBBS..."
   GOSUB 80
   CALL TWRANK(FIL$)
   IF NO.MONITOR = 1 THEN _
      END
   RUN MON.SUB$ + "MONITOR1"
80 REM **/ Print A$ + Carriage return and linefeed/**
   LN$ = A$
   NORET = 0
   GOSUB OUT.PUT
   RETURN
100 REM **/ Print A$ through modem /**
    LN$ = A$
    NORET = 1
    GOSUB OUT.PUT
    RETURN
400 REM **/ Input A$ /*
    GOSUB IN.PUT
    A$ = ARG$
    RETURN
800 REM **/ Maintence program /**
	FC = 3
    A$ = "Running maintence program..."
    GOSUB 80
    GOTO 34000
2000 REM **/ Open data file /**
	 CALL OPENDATA
	 FIELD 1,41 AS FA$, _ 'Have to assign fields in BOTH segments of the code
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
     RETURN
2300 REM **/ Close data file /**
     CLOSE 1
     O.PENFILE$ = ""
     RETURN
4000 REM **/ Instructions /**
     A$ = ""
     GOSUB 80
     GOSUB 2300
     NOFSX$ = ""
	 OPEN "TWINSTR.DOC" FOR INPUT AS #1
	 WHILE (NOFSX$ <> "Q") AND (NOT EOF(1))
		 LINE INPUT #1,A$
		 GOSUB 80
	 WEND
	 CLOSE 1
     GOSUB 2000
     RETURN
5000 REM **/ Read opening message /**
	 GOSUB 2300
	 O.PENFILE$ = "TWOPENG.DAT"
	 IF GRAPHICS = 1 THEN
		CALL TWANSIO("TWOPENC.DAT",L.OCAL)
		CALL LINE25
		GOSUB 2000
		RETURN
	 END IF
	 OPEN O.PENFILE$ FOR INPUT AS #1
	 WHILE (NOFSX$ <> "Q") AND (NOT EOF(1))
        LINE INPUT #1,A$
        GOSUB 80
	 WEND
     CLOSE 1
     GOSUB 2000
     RETURN
5800 REM **/ Open TWPMSG.DAT /**
	 OPEN "TWPMSG.DAT" FOR RANDOM AS #1 LEN = 8
     FIELD 1,2 AS FA$, _
             2 AS FB$, _
             2 AS FC$, _
             2 AS FD$
     RETURN
5900 REM **/ Close TWPMSG.DAT /**
     CLOSE 1
	 RETURN
6500 REM **/ Read messages to player PN /**
     GOSUB 2300
     GOSUB 5800
	 F = 0
     A$ = "The following happened to your ship since your last time on:"
     GOSUB 80
     GET 1,1
     FOR L = 2 TO CVI(FA$)
       GET 1,L
       IF CVI(FA$) <> PN OR CVI(FB$) = 0 THEN _
          6560
       P = CVI(FC$)
       N = CVI(FD$)
       LSET FB$ = MKI$(0)
       PUT 1,L
       IF P = -98 THEN
          A$ = "A deleted player destroyed" + _
               STR$(N) + _
               " fighters."
          GOSUB 80
          F = 1
          GOTO 6560
       END IF
	   IF P = -1 THEN
		  FC = 1
          A$ = "The Cabal destroyed" + _
               STR$(N) + _
               " fighters."
          GOSUB 80
          F = 1
          GOTO 6560
       END IF
       GOSUB 5900
       GOSUB 2000
       GET 1,P
	   A$ = LEFT$(FA$,CVI(FM$))
	   IF CVI(FE$) = 1 THEN _
		  A$ = A$ + "   Team [" + RIGHT$(STR$(CVI(FR$)),LEN(STR$(CVI(FR$))) - 1) + "]"
	   A$ = A$ + " destroyed" + _
            STR$(N) + _
            " of your fighters."
       GOSUB 80
       F = 1
       GOSUB 2300
       GOSUB 5800
6560 NEXT
     GOSUB 5900
     GOSUB 2000
     IF F = 0 THEN
        A$ = "Nothing"
        GOSUB 80
     END IF
	 RETURN
10000 REM **/ Initialize /**
      GOSUB 2000
		CALL GET.D(D,AY)
      IF CVI(FL$) < D THEN _
         800
		A$ = ""
		FC = 6
      GOSUB 80
      GOSUB 80
      A$ = "       Trade Wars (v.ii)"
      GOSUB 80
      A$ = "    By Chris Sherrick (PTL)"
      GOSUB 80
      A$ = " Copyright 1986 Chris Sherrick"
      GOSUB 80
      A$ = ""
      GOSUB 80
      A$ = "         " + RBBS.NAME$
      GOSUB 80
      SY$ = SYSOP.FIRST$ + " " + SYSOP.LAST$
      A$ = "       Sysop  " + SY$
      GOSUB 80
      A$ = ""
      GOSUB 80
      GOSUB 5000
      A$ = ""
		GOSUB 80
		FC = 7
      A$ = "Initializing..."
      GOSUB 80
		CALL GET.D(D,AY)
      PD = D
      A$ = ""
		GOSUB 80
		IF ALIAS = 1 THEN _
			GOSUB ALIAS
      PN$ = FIRST$ + " " + LAST$
      A$ = "Welcome " + PN$ + "!"
      GOSUB 80
      A$ = "Searching my records for your name."
      GOSUB 80
      FOR L = 2 TO LP
        GET 1,L
        IF LEFT$(FA$,CVI(FM$)) = PN$ THEN
		   PN = L
		   TN = CVI(FR$)
           GOTO 10130
        END IF
		NEXT
		FC = 2
      A$ = "I can't find your record, so I am assuming you are a new player."
      GOSUB 80
      GOTO 11000
10130 PN$ = LEFT$(FA$,CVI(FM$))
      A$ = ""
      GOSUB 80
      A$ = N$
      GOSUB 80
      A$ = TIME$ + " " + DATE$ + " " + PN$ + "(" + _
           STR$(PN) + "): Logged on"
	  CALL SYSOP.MESSAGE(A$)
      GET 1,PN
      A = CVI(FB$)
      IF A < PD THEN _
         10300
      IF A = PD THEN _
         10200
      A$ = "I'm sorry, but you won't be allowed on for another" + _
           STR$(A - PD) + " day(s)"
      GOSUB 80
      GOTO 60
10200 IF CVI(FC$) = -99 THEN _
         10500
      A$ = "You have been on today."
      GOSUB 80
      IF CVI(FD$) < 1 THEN
		 A$ = "You don't have any turns left today. You will be allowed to play tomorrow."
         GOSUB 80
         GOTO 60
      END IF
      IF CVI(FC$) = PN THEN
		 A$ = "You killed yourself today. You will be allowed to play tomorrow."
         GOSUB 80
         GOTO 60
      END IF
10300 GOSUB 6500
      GET 1,PN
      IF CVI(FC$) <> 0 THEN _
         10500
      IF CVI(FD$) <= TT AND CVI(FB$) < PD THEN
         LSET FD$ = MKI$(TT)
         LSET FB$ = MKI$(PD)
         PUT 1,PN
      END IF
      A$ = ""
      GOSUB 80
      A$ = "You have" + _
           STR$(CVI(FD$)) + _
           " turns today."
      GOSUB 80
      GOTO 19000
10500 A = CVI(FC$)
      IF A = -99 THEN _
         10600
      IF A = -98 THEN
		 A$ = "You have been killed by a deleted player."
         GOSUB 80
         GOTO 10600
      END IF
	  IF A = -1 THEN
		 FC = 1
         A$ = "You have been killed by the Cabal!"
         GOSUB 80
         GOTO 10600
      END IF
		IF A = PN THEN
		 FC = 1
         A$ = ""
         GOSUB 80
         A$ = "You managed to kill yourself on your last time on."
         GOSUB 80
         GOTO 10600
      END IF
      IF A > 1 AND A <= LP THEN
         GET 1,A
         A$ = LEFT$(FA$,CVI(FM$)) + _
              " destroyed your ship!"
         GOSUB 80
         GET 1,PN
         GOTO 10600
      END IF
10600 CALL INIT.SHIP(PD,PN,END.NOW)
	  IF END.NOW = 1 THEN _
		 GOTO 60
      GOTO 19000
11000 REM * Enter a new user *
	  A$ = ""
	  FC = 5
      GOSUB 80
      A$ = "Entering a new player..."
      GOSUB 80
      FOR PN = 2 TO LP
        GET 1,PN
        IF CVI(FM$) < 1 THEN _
           11120
      NEXT PN
      A$ = "I'm sorry but the game is full."
      GOSUB 80
      A$ = "Please leave a message for the Sysop so"
      GOSUB 80
      A$ = "he can save a space for you when one opens up."
      GOSUB 80
      A$ = TIME$ + " " + DATE$ + " " + PN$ + _
           ": New player not allowed - game full."
	  CALL SYSOP.MESSAGE(A$)
      GOTO 60
11120 GET 1,1
      A$ = ""
      GOSUB 80
      A$ = "Notice: If you don't play this game for" + _
            STR$(CVI(FK$)) + _
            " days, you will"
      GOSUB 80
      A$ = "be deleted to make room for someone else."
      GOSUB 80
      A$ = ""
      GOSUB 80
	  CALL INIT.SHIP(PD,PN,END.NOW)
	  IF END.NOW = 1 THEN _
		 GOTO 60
      GET 1,PN
      LSET FA$ = PN$
      LSET FM$ = MKI$(LEN(PN$))
      LSET FR$ = MKI$(PN)
      PUT 1,PN
      A$ = TIME$ + " " + DATE$ + " " + PN$ + "(" + _
           STR$(PN) + _
           "): New Player logged on"
		CALL SYSOP.MESSAGE(A$)
      GOSUB 28000
      GOTO 19000
15300 A$ = "Your ship has been destroyed!"
	  FC = 1
      GOSUB 80
      A$ = ""
      GOSUB 80
      A$ = "You will start over tomorrow with a new ship."
      GOSUB 80
      GET 1,PN
      P = PN
	  CALL DEL.P(M.AINT,P,PN)
      GOTO 60
15600 REM **/ Player PN salvages ship from player P /**
      GET 1,P
      A = INT(CVI(FH$)/4) + 1
      GET 1,PN
      IF A + CVI(FH$) > 50 THEN _
         A = 50-CVI(FH$)
	  IF A < 1 THEN
		 A$ = "You destroyed the ship, but you can't salvage anything from it"
		 GOSUB 80
		 RETURN
	  END IF
      B = 0
      C = 0
      D = 0
      E = 0
      GET 1,P
      F = CVI(FI$)
      G = CVI(FJ$)
      H = CVI(FK$)
      I = CVI(FH$)
      FOR L = 1 TO A
        A = I
		CALL RAND(V,A)
        V = V - 1
        IF V < F THEN
               B = B + 1
               F = F - 1
        ELSEIF V < F + G THEN
               C = C + 1
               G = G - 1
        ELSEIF V < F + G + H THEN
               D = D + 1
               H = H - 1
        ELSE E = E + 1
      END IF
      I = I - 1
      NEXT
      GET 1,PN
      LSET FH$ = MKI$(CVI(FH$) + B + C + D + E)
      LSET FI$ = MKI$(CVI(FI$) + B)
      LSET FJ$ = MKI$(CVI(FJ$) + C)
      LSET FK$ = MKI$(CVI(FK$) + D)
      PUT 1,PN
      A$ = "You destroyed the ship and salvaged these cargo holds:"
      GOSUB 80
      IF E > 0 THEN
         A$ = "  " + _
              STR$(E) + _
              " empty"
         GOSUB 80
      END IF
      IF B <> 0 THEN
         A$ = "  " + _
              STR$(B) + _
              " with ore"
         GOSUB 80
      END IF
      IF C <> 0 THEN
         A$ = "  " + _
              STR$(C) + _
              " with organics"
         GOSUB 80
      END IF
      IF D <> 0 THEN
         A$ = "  " + _
              STR$(D) + _
              " with equipment"
         GOSUB 80
      END IF
      RETURN
16000 REM **/ Print info on player /**
	  FC = 2
      P.LINE = 0
      GET 1,PN
      A$ = ""
      GOSUB 80
      A$ = "Name: " + LEFT$(FA$,CVI(FM$))
	  GOSUB 100
	  IF CVI(FE$) = 1 THEN
		 A$ = "   Team [" + RIGHT$(STR$(CVI(FR$)),LEN(STR$(CVI(FR$))) - 1) + "]"
	   ELSE A$ = ""
	  END IF
	  GOSUB 80
	  GET 1,PN
      A$ = "Sector:" + STR$(CVI(FF$)) + _
           "   Turns left: " + STR$(CVI(FD$))
      GOSUB 80
      A$ = "Fighters:" + STR$(CVI(FG$))
      GOSUB 80
      A$ = "Cargo Holds:" + STR$(CVI(FH$))
      GOSUB 80
      A$ = "  Ore:" + STR$(CVI(FI$)) + _
           "   Org:" + STR$(CVI(FJ$)) + _
           "   Eqp:" + STR$(CVI(FK$))
      GOSUB 80
      A$ = "Credits:" + STR$(CVI(FL$))
	  GOSUB 80
	  IF NO.MONITOR = 0 THEN
		 A$ = "Door Monitor points: " + STR$(POINTS#)
		 GOSUB 80
	  END IF
      RETURN
19000 REM **/ Start playing /**
	  CALL READ.RADIO(PN,SNOOP,L.OCAL)
      GET 1,PN
      IF CVI(FF$) < 1 THEN
         A$ = "You are being moved to sector 1"
         GOSUB 80
         LSET FF$ = MKI$(1)
         LSET FQ$ = MKI$(0)
         PUT 1,PN
      END IF
      GET 1,PN
19900 IF CVI(FL$) > 25000 THEN
         A$ = ""
         GOSUB 80
		 A$ = "Tax time! You are being taxed 5000 credits to help support the resistance"
         GOSUB 80
         A$ = "against the evil Cabal."
         GOSUB 80
         LSET FL$ = MKI$(CVI(FL$) - 5000)
         PUT 1,PN
      END IF
20000 REM **/ Enter a sector /**
	  GET 1,PN
	  FC = 3
	  BC = 0
      PR = CVI(FF$)
      S2 = PR + LP
      GET 1,S2
      E(1) = CVI(FB$)
      E(2) = CVI(FC$)
      E(3) = CVI(FD$)
      E(4) = CVI(FE$)
      E(5) = CVI(FF$)
      E(6) = CVI(FG$)
      A$ = ""
      GOSUB 80
	  IF CVI(FM$) = PN THEN _
		 21000
      IF CVI(FL$) <> 0 THEN _
         20100
      GOTO 21000
20100 GET 1,S2
	  IF CVI(FM$) > 0 THEN
		 GET 1,CVI(FM$)
		 IF CVI(FR$) = TN THEN _
			21000
	  END IF
	  GOSUB 21100
      A$ = ""
      GOSUB 80
      A$ = "You have to destroy the fighters before you can enter this sector."
      GOSUB 80
      ZX$ = ""                                      'reset stack commands
20200 GET 1,PN
      F2 = CVI(FG$)
      GET 1,S2
      E2 = CVI(FL$)
      A$ = ""
      GOSUB 80
      A$ = "Fighters:" + STR$(F2) + " /" + STR$(E2)
      GOSUB 80
20210 A$ = "Option? (A,D,I,Q,R,?):? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN
		 A$ = "?=<Help>"
         GOSUB 80
         GOTO 20210
      END IF
      A = ASC(LEFT$(A$,1)) AND 223
	  IF A = 82 THEN _               'retreat
		 20300
20235 IF A = 68 THEN                 'display sector
         A$ = "<Display>"
         GOSUB 80
         GOSUB 21100
         GOTO 20200
      END IF
	  IF A = 65 THEN _               'attack
         20400
	  IF A = 81 THEN _               'quit
         20500
	  IF A = 73 THEN                 'info on player
         A$ = "<Info>"
         GOSUB 80
         GOSUB 16000
         GOTO 20200
      END IF
	  IF A <> 31 THEN
         A$ = ""
         GOSUB 80
         A$ = "Invalid command.  ? = Help"
         GOSUB 80
         GOTO 20200
      END IF
      A$ = "Help"
      GOSUB 80
      A$ = ""
      GOSUB 80
      A$ = "A - <A>ttack"
      GOSUB 80
      A$ = "D - re<D>isplay sector"
      GOSUB 80
      A$ = "I - <I>nformation about your ship"
      GOSUB 80
      A$ = "Q - <Q>uit the game"
      GOSUB 80
      A$ = "R - <R>etreat"
      GOSUB 80
      GOTO 20200
20300 A$ = "<Retreat>"
      GOSUB 80
      GET 1,PN
      LR = CVI(FQ$)
20320 IF LR = 0 OR LR = PR THEN
         A = 6
		 CALL RAND(V,A)
         LR = E(V)
         GOTO 20320
      END IF
      IF CVI(FG$) < 1 THEN _
         20360
      LSET FG$ = MKI$(CVI(FG$) - 1)
      PUT 1,PN
      A$ = "You have" + STR$(CVI(FG$)) + " fighter(s) left."
      GOSUB 80
20350 P = PN
	  CALL REM.SHIP(P)
      GET 1,PN
      LSET FF$ = MKI$(LR)
      LSET FQ$ = MKI$(PR)
      PUT 1,PN
      P = PN
	  CALL ADD.P(P,END.NOW)
	  IF END.NOW = 1 THEN _
		 GOTO 60
      LR = A
      GOTO 20000
20360 A = 2
	  CALL RAND(V,A)
      IF V = 1 THEN
         A$ = "You escaped!"
         GOSUB 80
         GOTO 20350
	  END IF
	  FC = 1
      A$ = "You didn't escape!"
      GOSUB 80
      GOTO 15300
20400 A$ = "<Attack>"
      GOSUB 80
      IF F2 < 1 THEN
         A$ = "You don't have any fighters!"
         GOSUB 80
         GOTO 20200
      END IF
      A$ = "How many fighters do you wish to use? "
      GOSUB 100
	  GOSUB 400
	  IF LEN(A$) > 4 THEN _
		 20200
      IF VAL(A$) < 1 OR VAL(A$) > 9999 THEN _
         20200
      L = 0
      K = 0
      C = VAL(A$)
      IF C > F2 THEN
         A$ = ""
         GOSUB 80
         A$ = "You don't have that many fighters."
         GOSUB 80
         GOTO 20200
      END IF
20430 A = 2
	  GET 1,S2
	  Z99 = CVI(FM$)
	  IF Z99 > 0 THEN
		 GET 1,Z99
		 A99 = CVI(FE$)
	  END IF
	  GET 1,PN
	  A98 = CVI(FE$)
	  IF (A98 = 0) AND (A99 = 1) THEN
		 A = 10
		 CALL RAND(V,A)
		 IF V > 6 THEN _
			L = L + 1 ELSE _
			K = K + 1
	   ELSEIF (A98 = 1 AND A99 = 0) AND Z99 > 0 THEN
		 A = 10
		 CALL RAND(V,A)
		 IF V > 6 THEN _
			K = K + 1 ELSE _
			L = L + 1
	   ELSE CALL RAND(V,A)
		 IF V = 1 THEN _
			L = L + 1  ELSE _
			K = K + 1
	  END IF
      IF L < C AND K < E2 THEN _
         20430
      F2 = F2 - L
      E2 = E2 - K
      GET 1,PN
      LSET FG$ = MKI$(F2)
      PUT 1,PN
      GET 1,S2
      LSET FL$ = MKI$(E2)
      IF E2 < 1 THEN
         LSET FL$ = MKI$(0)
         LSET FM$ = MKI$(0)
      END IF
      PUT 1,S2
      A$ = "You lost" + STR$(L) + " fighter(s)"
      GOSUB 80
      A$ = "You destroyed" + STR$(K) + " enemy fighters."
      GOSUB 80
	  POINTS# = POINTS# + 100*K
      A$ = ""
      GOSUB 80
	  A$ = "You just recieved" + STR$(100*K) + _
		   " points for that."
      GOSUB 80
      IF E2 > 0 THEN _
         20200
      A$ = "You destroyed all the fighters."
      GOSUB 80
      GOTO 21000
20500 A$ = "<Quit>"
	  FC = 7
	  BC = 1
      GOSUB 80
      A$ = "Are you sure (Y/N)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         20500 ELSE _
         A$ = LEFT$(A$,1)
      IF A$ = "Y" OR A$ = "y" THEN _
         60
      GOTO 20200
21000 REM **/ Made it in /**
	  IF PR <> 85 THEN
		 GOSUB 21100
		 GOTO 22000
	  END IF
	  A$ = ""
	  FC = 7
      GOSUB 80
      A$ = ""
      GOSUB 80
	  A$ = "Congratulations!  You've defeated the cabal and gained 100,000 points!"
      GOSUB 80
      A$ = "Unfortunately, victory is fleeting..."
      GOSUB 80
      GET 1,S2
	  LSET FL$ = MKI$(3000)
      LSET FM$ = MKI$(-1)
      PUT 1,S2
	  POINTS# = POINTS# + 100000
	  A$ = "Congratulations go to " + PN$ + _
           " who invaded the Cabal empire on " + DATE$ + _
		   " and received 100,000 points!"
	  CALL TWOPENG.ADD(A$)
      GOTO 20000
21100 REM **/ Print sector description /**
	  P.LINE = 0
	  FC = 3
	  BC = 0
      GET 1,PN
      PR = CVI(FF$)
      S2 = PR + LP
      A$ = ""
      GOSUB 80
      A$ = "Sector:" + STR$(PR)
      GOSUB 80
	  GET 1,S2
	  FC = 1
      A$ = "Ports: "
      GOSUB 100
      ST = CVI(FH$)
      IF ST <> 0 THEN
         GET 1,ST + LS
         A$ = LEFT$(FA$,CVI(FM$)) + ", class" + STR$(CVI(FB$))
         GOSUB 80
       ELSE A$ = "None"
         GOSUB 80
      END IF
      GET 1,S2
      A = CVI(FO$)
      IF A <> 0 THEN
         GET 1,A + LT
         A$ = "Planet: " + LEFT$(FA$,CVI(FM$))
         GOSUB 80
         GET 1,S2
      END IF
      G2 = 0
	  A$ = "Other Ships: "
	  FC = 6
      GOSUB 100
      A = CVI(FI$)
      IF A = 0 THEN
         A$ = "None"
         GOSUB 80
         GOTO 21160
      END IF
21140 GET 1,A
	  P.LINE = 0
      IF CVI(FP$) <> PN THEN
         GET 1,CVI(FP$)
         A$ = ""
         GOSUB 80
		 A$ = "   " + LEFT$(FA$,CVI(FM$))
		 IF CVI(FE$) = 1 THEN _
			A$ = A$ + "  Team [" + RIGHT$(STR$(CVI(FR$)),LEN(STR$(CVI(FR$)))-1) + "]"
		 A$ = A$ + " with" + STR$(CVI(FG$)) + _
				   " fighters"
         GOSUB 100
         G2 = 1
      END IF
      GET 1,A
      A = CVI(FO$)
      IF A <> 0 THEN
           GOTO 21140
       ELSEIF G2 = 0 THEN
           A$ = "None"
           GOSUB 80
       ELSE A$ = ""
           GOSUB 80
      END IF
21160 GET 1,S2
	  FC = 2
      A$ = "Fighters in sector:"
      GOSUB 100
      IF CVI(FL$) = 0 THEN
         A$ = " None"
         GOSUB 80
         GOTO 21180
      END IF
      A$ = STR$(CVI(FL$))
      IF CVI(FM$) < 1 THEN
           A$ = A$ + " (Cabal)"
           GOSUB 80
       ELSEIF CVI(FM$) = PN THEN
           A$ = A$ + " (yours)"
           GOSUB 80
       ELSE GET 1,CVI(FM$)
		   A$ = A$ + " (belong to " + LEFT$(FA$,CVI(FM$))
		   IF CVI(FE$) = 1 THEN _
			  A$ = A$ + "  Team [" + RIGHT$(STR$(CVI(FR$)),LEN(STR$(CVI(FR$)))-1) + "]"
		   A$ = A$ + ")"
		   GOSUB 80
      END IF
21180 A$ = "Warps lead to:"
	  FC = 5
      GOSUB 100
      B$ = ""
      FOR L = 1 TO 6
		IF E(L) <> 0 THEN
           A$ = B$ + STR$(E(L))
		   GOSUB 100
		   B$ = ","
        END IF
      NEXT
      T.IMER = 1
	  A$ = ""
	  FC = 7
      GOSUB 80
      GET 1,PN
	  IF SNOOP THEN _
         PRINT "Turns left: " + STR$(CVI(FD$))
      RETURN
22000 P.LINE = 0
	  A$ = ""
	  FC = 6
	  BC = 0
      GOSUB 80
      A$ = "Command (?=Help)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN
         A$ = "? = Help"
         GOSUB 80
         GOTO 22000
       ELSE A = ASC(LEFT$(A$,1)) AND 223
      END IF
      IF A = 65 THEN _
         25000
	  IF A = 67 THEN _
		 33640
	  IF A = 68 THEN
		 A$ = "<Display>"
		 GOSUB 80
		 GOSUB 21100
		 GOTO 22000
	  END IF
	  IF A = 69 THEN _
		 22800
	  IF A = 70 THEN _
		 24000
      IF A = 71 THEN _
         27500
	  IF A = 73 THEN
		 A$ = "<Info>"
		 GOSUB 80
		 GOSUB 16000
		 GOTO 22000
	  END IF
	  IF A = 76 THEN _
		 31000
	  IF A = 77 THEN _
		 23000
      IF A = 80 THEN _
		 32850
	  IF A = 81 THEN _
		 22700
	  IF A = 84 THEN _
		 32799
      IF A = 90 THEN
         A$ = "<Instructions>"
         GOSUB 80
         GOSUB 28000
         GOTO 22000
      END IF
      IF A <> 31 THEN
         A$ = "Invalid command.  ? = Help"
         GOSUB 80
         GOTO 22000
	  END IF
	  FC = 6
      A$ = "<Help>"
      GOSUB 80
      A$=""
      GOSUB 80
      A$ = "A - <A>ttack a player's ship"
      GOSUB 80
      A$ = "C - <C>omputer"
      GOSUB 80
      A$ = "D - re<D>isplay sector"
      GOSUB 80
	  IF SYS.OP = 1 THEN
		 A$ = "E - Tradewars <E>ditor"
		 GOSUB 80
      END IF
22120 A$ = "F - take or leave <F>ighters"
      GOSUB 80
      A$ = "G - <G>amble"
      GOSUB 80
      A$ = "I - <I>nfo on your ship"
	  GOSUB 80
	  A$ = "L - <L>and on or create a planet."
      GOSUB 80
      A$ = "M - <M>ove to another sector"
      GOSUB 80
      A$="P - land at a space <P>ort (and trade)"
      GOSUB 80
      A$ = "Q - <Q>uit game"
      GOSUB 80
	  A$ = "T - <T>eam menu"
	  GOSUB 80
	  A$ = "Z - instructions"
      GOSUB 80
      GOTO 22000
22700 A$ = "<Quit>"
	  FC = 7
	  BC = 1
      GOSUB 80
      A$ = "Are you sure (Y/N)? "
      GOSUB 100
22710 GOSUB 400
      IF A$ = "" THEN _
         22710 ELSE _
		 A$ = LEFT$(A$,1)
      IF A$ = "Y" OR A$ = "y" THEN _
         60
      GOTO 22000
22800 IF SYS.OP <> 1 THEN _
         22000
      A$ = "<TW Editor>"
      GOSUB 80
      A$ = "Do you wish to use the editor? Y/N [N] "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
		 A$ = "N"
	  A$ = LEFT$(A$,1)
	  IF A$ = "Y" OR A$ = "y" THEN
		 A$ = "Running Tradewars ][ Editor..."
		 FC = 6
		 BC = 7
		 GOSUB 80
		 RUN "TWEDIT"
	  END IF
	  GOTO 22000
23000 A$ = "<Move>"
      GOSUB 80
      GET 1,PN
      T2 = CVI(FD$)
      IF T2 < 1 THEN
         A$ = "I'm sorry but you don't have any turns left."
         GOSUB 80
         GOTO 22000
      END IF
      A$ = "Warps lead to: "
      GOSUB 100
      B$ = ""
      FOR L = 1 TO 6
        IF E(L) <> 0 THEN
           A$ = B$ + STR$(E(L))
           GOSUB 100
		   B$ = ","
        END IF
      NEXT L
      A$ = ""
      GOSUB 80
      A$ = "To which sector? "
      GOSUB 100
	  GOSUB 400
	  IF LEN(A$) > 4 THEN _
		 22000
      IF VAL(A$) < 1 OR VAL(A$) > 9999 THEN _
         22000 ELSE _
         N2 = VAL(A$)
      FOR L = 1 TO 6
		IF E(L) = N2 THEN _
		   GOTO 23050
	  NEXT L
	  A$ = ""
	  GOSUB 80
	  A$ = "You can't get there from here."
	  GOSUB 80
	  GOTO 22000
23050 T2 = T2 - 1
      P = PN
	  CALL REM.SHIP(P)
      GET 1,PN
      LSET FF$ = MKI$(N2)
      LSET FQ$ = MKI$(PR)
      LSET FD$ = MKI$(T2)
      PUT 1,PN
      P = PN
	  CALL ADD.P(P,END.NOW)
	  IF END.NOW = 1 THEN _
		 GOTO 60
      IF T2 = 10 OR T2 < 6 THEN
         A$ = ""
         GOSUB 80
         A$ = "You have" + STR$(T2) + " turns left."
         GOSUB 80
      END IF
      GOTO 20000
24000 A$ = "<Drop/Take Fighters>"
      GOSUB 80
      IF PR < 8 THEN
         A$ = "You can't leave fighters in the Union (sectors 1-7)"
         GOSUB 80
         GOTO 22000
      END IF
      GET 1,S2
	  IF CVI(FL$) > 0 AND CVI(FM$) <> PN THEN
		 A$ = "There are already fighters in this sector!"
		 GOSUB 80
		 GOTO 22000
	  END IF
	  D2 = CVI(FL$)
	  GET 1,PN
      F2 = CVI(FG$)
      A$ = "You have" + STR$(F2 + D2) + _
           " fighters available."
      GOSUB 80
      A$ = "How many fighters do you want defending this sector? "
      GOSUB 100
	  GOSUB 400
	  IF LEN(A$) > 4 THEN _
		 22000
      IF A$ = "" OR INT(VAL(A$)) < 0 THEN _
         22000
      L = INT(VAL(A$))
      B = F2 + D2 - L
      IF B < 0 THEN
         A$ = "You don't have that many ships available"
         GOSUB 80
         GOTO 22000
      END IF
      IF B > 9999 THEN
         A$ = "Too many ships in your fleet!  You are limited to 9999"
         GOSUB 80
         GOTO 22000
      END IF
      GET 1,S2
      LSET FL$ = MKI$(L)
      LSET FM$ = MKI$(PN*SGN(L))
      PUT 1,S2
      GET 1,PN
      LSET FG$ = MKI$(B)
      PUT 1,PN
      A$ = "Done.  You have" + STR$(B) + _
           " fighter(s) in your fleet."
      GOSUB 80
      GOTO 22000
25000 A$ = "<Attack>"
      GOSUB 80
      GET 1,S2
      A = CVI(FI$)
      GET 1,A
      IF CVI(FO$) = 0 THEN
         A$ = "There are no ships here to attack."
         GOSUB 80
         GOTO 22000
      END IF
      GET 1,PN
      F2 = CVI(FG$)
      IF F2 < 1 THEN
         A$ = "You don't have any fighters."
         GOSUB 80
         GOTO 22000
      END IF
      IF LA > 1 AND LA <= LP THEN
         GET 1,LA
         IF CVI(FF$) = PR THEN
            A$ = "Attack " + LEFT$(FA$,CVI(FM$)) + " (Y/N)[Y]? "
            GOSUB 100
            GOSUB 400
            B$ = LEFT$(A$ + " ",1)
         END IF
      END IF
      IF A$ = "" OR B$ = "Y" OR B$ = "y" THEN
         P = LA
         GOTO 25080
      END IF
25040 GET 1,A
      P = CVI(FP$)
      GET 1,P
      IF P = PN OR P = LA THEN _
         25060
      A$ = "Attack " + LEFT$(FA$,CVI(FM$)) + " (Y/N)[N]? "
      GOSUB 100
      GOSUB 400
      A$ = LEFT$(A$ + " ",1)
      IF A$ = "Y" OR A$ = "y" THEN _
         25080
25060 GET 1,A
      A = CVI(FO$)
      IF A <> 0 THEN _
         25040
      A$ = "There are no other ships in here"
      GOSUB 80
      GOTO 22000
25080 LA = P
      A$ = "How many fighters do you wish to use [0]? "
      GOSUB 100
	  GOSUB 400
	  IF LEN(A$) > 4 THEN _
		 22000
      IF VAL(A$) < 1 OR VAL(A$) > 9999 THEN _
         22000
      GET 1,P
      E2 = CVI(FG$)
      L1 = 0
      K1 = 0
      C = VAL(A$)
      GET 1,PN
      F2 = CVI(FG$)
      IF C > F2 THEN
         A$ = "You don't have that many fighters."
         GOSUB 80
         GOTO 22000
      END IF
25100 A = 2
	  GET 1,P
	  A99 = CVI(FE$)
	  GET 1,PN
	  A98 = CVI(FE$)
	  IF (A98 = 0) AND (A99 = 1) THEN
		 A = 10
		 CALL RAND(V,A)
		 IF V > 6 THEN
			L1 = L1 + 1
		  ELSE K1 = K1 + 1
		 END IF
	   ELSEIF (A98 = 1) AND (A99 = 0) THEN
		 A = 10
		 CALL RAND(V,A)
		 IF V > 6 THEN
			K1 = K1 + 1
		  ELSE L1 = L1 + 1
		 END IF
	   ELSE CALL RAND(V,A)
		 IF V = 1 THEN
			L1 = L1 + 1
		  ELSE K1 = K1 + 1
		 END IF
	  END IF
      IF L1 < C AND K1 <= E2 THEN _
         25100
      IF K1 > 0 THEN
         N = K1
		 CALL MESS.TO.PLAYER(N,P,PN)
      END IF
      GET 1,PN
      LSET FG$ = MKI$(F2 - L1)
      PUT 1,PN
      GET 1,P
      LSET FG$ = MKI$(E2 - K1)
      PUT 1,P
      A$ = ""
      GOSUB 80
      A$ = "You lost" + STR$(L1) + _
           " fighter(s)," + STR$(F2 - L1) + _
           " remain."
      GOSUB 80
      IF E2 - K1 > 0 THEN
         A$ = "You destroyed" + STR$(K1) + _
              " enemy fighters," + STR$(E2 - K1) + _
              " remain."
         GOSUB 80
         GOTO 22000
      END IF
	  CALL DEL.P(M.AINT,P,PN)
      GOSUB 15600
      GOTO 22000
27500 REM **/ Gamble /**
      A$ = "<Gamble>"
      GOSUB 80
      GET 1,PN
      M = CVI(FL$)
      A$ = "You have" + STR$(M) + " credits."
      GOSUB 80
      A$ = "How much do you want to gamble at double or nothing (50-50 odds)"
      GOSUB 80
      A$ = "[0]? "
      GOSUB 100
	  GOSUB 400
	  IF LEN(A$) > 5 THEN _
		 21000
      A$ = A$ + " "
      IF VAL(A$) <= 0 THEN _
         21000
      IF VAL(A$) > M THEN
         A$ = "You're not that rich."
         GOSUB 80
         GOTO 21000
      END IF
	  N = VAL(A$)
	  IF ((N + M) > 32000) THEN _
		 19900
      A$ = "Flipping the coin..."
      GOSUB 100
      FOR L = 1 TO 300
      NEXT
      A = 2
	  CALL RAND(V,A)
      IF V = 1 THEN
         LSET FL$ = MKI$(M - N)
         PUT 1,PN
         A$ = ""
         GOSUB 80
         A$ = "Sorry, you lost."
         GOSUB 80
         GOTO 27590
      END IF
      LSET FL$ = MKI$(M + N)
      PUT 1,PN
      A$ = "You won!"
      GOSUB 80
27590 A$ = "You now have" + STR$(CVI(FL$)) + " credits."
      GOSUB 80
      GOTO 20000
28000 REM **/ Instructions /**
      A$ = "Do you want instructions (Y/N) [N]? "
      GOSUB 100
      GOSUB 400
      IF A$ = "N" OR A$ = "n" OR A$ = "" THEN _
         RETURN
28030 GOSUB 4000
	  RETURN
31000 A$ = "<Land/Create planet>"
      GOSUB 80
	  A$ = ""
	  FC = 3
	  BC = 1
      GOSUB 80
      A$ = "Landing..."
      GOSUB 80
      GET 1,S2
      L = CVI(FO$)
      IF L <> 0 THEN
         L2 = L + LT
         GOTO 32310
      END IF
      A$ = "There isn't a planet in this sector."
      GOSUB 80
      A$ = "Planets cost 10,000 credits."
      GOSUB 80
      GET 1,PN
      IF CVI(FL$) < 10000 THEN
         A$ = "You're too poor to buy one."
         GOSUB 80
		 GOTO 20000
      END IF
32200 GET 1,PN
      A$ = "You have" + STR$(CVI(FL$)) + " credits."
      GOSUB 80
      A$ = "Do you wish to buy a planet(Y/N) [N]? "
      GOSUB 100
      GOSUB 400
      A$ = LEFT$(A$ + "N",1)
	  IF A$ = "N" OR A$ = "n" THEN _
		 20000
      FOR L = LT + 1 TO LL
		GET 1,L
		IF CVI(FM$) = 0 THEN
		   PLANET.REC = L
		   EXIT FOR
		END IF
		IF L = LL THEN
		   A$ = ""
		   GOSUB 80
		   A$ = "I'm sorry, but all planets are taken."
		   GOSUB 80
		   A$ = "One has to be destroyed before you can buy a planet."
		   GOSUB 80
		   GOTO 20000
		END IF
32210 NEXT
32220 A$ = "What do you want to name this planet? (41 chars. max)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         32200
      A = LEN(A$)
      IF A > 41 THEN
         A$ = ""
         GOSUB 80
         A$ = "You are limited to 41 characters. That is" + _
               STR$(A) + _
               " characters."
         GOSUB 80
         GOTO 32220
      END IF
	  CALL NAM.ADJ(A$)
      N$ = A$
	  GET 1,PLANET.REC
      LSET FA$ = N$
      LSET FM$ = MKI$(LEN(N$))
      LSET FC$ = MKI$(1)
      LSET FD$ = MKI$(1)
      LSET FE$ = MKI$(1)
      LSET FF$ = MKI$(1)
      LSET FG$ = MKI$(1)
      LSET FH$ = MKI$(1)
      LSET FI$ = MKI$(0)
      LSET FJ$ = MKI$(0)
      LSET FK$ = MKI$(0)
	  PUT 1,PLANET.REC
      GET 1,S2
	  LSET FO$ = MKI$(PLANET.REC - LT)
      PUT 1,S2
	  CALL GET.D(D,AY)
      A$ = TIME$
      M = VAL(LEFT$(A$,2))*60 + VAL(MID$(A$,4,2))
	  GET 1,PLANET.REC
      LSET FB$ = MKI$(D)
      LSET FR$ = MKI$(M)
	  PUT 1,PLANET.REC
      GET 1,PN
      LSET FL$ = MKI$(CVI(FL$) - 10000)
      PUT 1,PN
      GET 1,PN
      A$ = "  -  " + LEFT$(FA$,CVI(FM$)) + _
           " made a planet: " + N$
	  CALL SYSOP.MESSAGE(A$)
      A$ = ""
      GOSUB 80
      A$ = "Planet created"
      GOSUB 80
	  GOTO 20000
32310 GOSUB 32760
      A$ = ""
      GOSUB 80
      GET 1,L2
      A$ = "Planet: " + LEFT$(FA$,CVI(FM$))
      GOSUB 80
      A$ = " Item      Prod.  Amount  in holds"
      GOSUB 80
      A$ = " ====      =====  ======  ========"
      GOSUB 80
	  FOR I = 1 TO 3
        A$ = P$(I)
        B$ = STR$(P(I))
        L = 5
		CALL ADD.SPACE(B$,L)
        A$ = A$ + B$
        B$ = STR$(N(I))
        L = 8
		CALL ADD.SPACE(B$,L)
        A$ = A$ + B$
        B$ = STR$(H(I))
        L = 8
		CALL ADD.SPACE(B$,L)
        A$ = A$ + B$
        GOSUB 80
      NEXT I
      A$ = "You have" + STR$(H(0) - H(1) - H(2) - H(3)) + _
           " free cargo holds."
      GOSUB 80
32360 P.LINE = 0
      A$ = ""
      GOSUB 80
      A$ = "Planet command (?=help)[A]? "
      GOSUB 100
      GOSUB 400
      A = ASC(A$ + "A") AND 223
      IF A = 17 THEN _
         32450
      IF A = 18 THEN _
         32480
      IF A = 19 THEN _
         32510
      IF A = 82 THEN _
         32310
      IF A = 76 THEN _
		 20000
      IF A = 65 THEN _
         32540
      IF A = 73 THEN _
         32620
      IF A = 68 THEN _
         32730
      IF A$ <> "?" THEN
         A$ = "Invalid command.  Enter ? for help"
         GOSUB 80
         GOTO 32360
      END IF
      A$ = ""
      GOSUB 80
      A$ = "1 - take ore"
      GOSUB 80
      A$ = "2 - take organics"
      GOSUB 80
      A$ = "3 - take equipment"
      GOSUB 80
      A$ = "A - take <A>ll"
      GOSUB 80
      A$ = "D - <D>estroy the planet"
      GOSUB 80
      A$ = "I - <I>ncrease productivity"
      GOSUB 80
      A$ = "L - <L>eave planet"
      GOSUB 80
      A$ = "R - planet <R>eport"
      GOSUB 80
      GOTO 32360
32450 A$ = "<Take ore>"
      GOSUB 80
      B$ = STR$(N(1))
      A$ = "How much [" + RIGHT$(B$,LEN(B$) - 1) + "]? "
      GOSUB 100
	  GOSUB 400
	  IF LEN(A$) > 5 THEN _
		 32360
      IF A$ = "" THEN _
         A = N(1) ELSE _
		 A = VAL(A$ + " ")
	  IF (A > N(1)) OR (A < 0) THEN
         A$ = "They don't have that many."
         GOSUB 80
         GOTO 32360
       ELSEIF H(0) - H(1) - H(2) - H(3) < A THEN
         A$ = "You don't have enough free cargo holds."
         GOSUB 80
         GOTO 32360
      END IF
      GET 1,PN
      LSET FI$ = MKI$(CVI(FI$) + A)
      PUT 1,PN
      GET 1,L2
      LSET FF$ = MKI$(CVI(FF$) - A)
      PUT 1,L2
      H(1) = H(1) + A
      N(1) = N(1) - A
      GOTO 32360
32480 A$ = "<Take organics>"
      GOSUB 80
      B$ = STR$(N(2))
      A$ = "How much [" + RIGHT$(B$,LEN(B$) - 1) + "]? "
      GOSUB 100
	  GOSUB 400
	  IF LEN(A$) > 5 THEN _
		 32360
      IF A$ = "" THEN _
         A = N(2) ELSE _
         A = VAL(A$ + " ")
	  IF (A > N(2)) OR (A < 0) THEN
         A$ = "They don't have that many."
         GOSUB 80
         GOTO 32360
       ELSEIF H(0) - H(1) - H(2) - H(3) < A THEN
         A$ = "You don't have enough free cargo holds."
         GOSUB 80
         GOTO 32360
      END IF
      GET 1,PN
      LSET FJ$ = MKI$(CVI(FJ$) + A)
      PUT 1,PN
      GET 1,L2
      LSET FG$ = MKI$(CVI(FG$) - A)
      PUT 1,L2
      H(2) = H(2) + A
      N(2) = N(2) - A
      GOTO 32360
32510 A$ = "<Take equipment>"
      GOSUB 80
      B$ = STR$(N(3))
      A$ = "How much [" + RIGHT$(B$,LEN(B$) - 1) + "]? "
      GOSUB 100
	  GOSUB 400
	  IF LEN(A$) > 5 THEN _
		 32360
      IF A$ = "" THEN _
         A = N(3) ELSE _
         A = VAL(A$ + " ")
	  IF (A > N(3)) OR (A < 0) THEN
         A$ = "They don't have that many."
         GOSUB 80
         GOTO 32360
       ELSEIF H(0) - H(1) - H(2) - H(3) < A THEN
         A$ = "You don't have enough free cargo holds."
         GOSUB 80
         GOTO 32360
      END IF
      GET 1,PN
      LSET FK$ = MKI$(CVI(FK$) + A)
      PUT 1,PN
      GET 1,L2
      LSET FH$ = MKI$(CVI(FH$) - A)
      PUT 1,L2
      H(3) = H(3) + A
      N(3) = N(3) - A
      GOTO 32360
32540 A$ = "<Take all>"
      GOSUB 80
      F = H(0) - H(1) - H(2) - H(3)
      A = N(3)
      IF F = 0 THEN
         A$ = "You don't have any free holds."
         GOSUB 80
         GOTO 32360
       ELSEIF F < A THEN
         A = F
      END IF
      IF F <> 0 THEN
         F = F - A
         H(3) = H(3) + A
         N(3) = N(3) - A
         GET 1,PN
         LSET FK$ = MKI$(CVI(FK$) + A)
         PUT 1,PN
         GET 1,L2
         LSET FH$ = MKI$(CVI(FH$) - A)
         PUT 1,L2
         A$ = "You took" + STR$(A) + " holds of equipment."
         GOSUB 80
      END IF
      A = N(2)
      IF F = 0 THEN
         A$ = "Your cargo holds are full."
         GOSUB 80
         GOTO 32360
       ELSEIF F < A THEN
         A = F
      END IF
      IF F <> 0 THEN
         F = F - A
         H(2) = H(2) + A
         N(2) = N(2) - A
         GET 1,PN
         LSET FJ$ = MKI$(CVI(FJ$) + A)
         PUT 1,PN
         GET 1,L2
         LSET FG$ = MKI$(CVI(FG$) - A)
         PUT 1,L2
         A$ = "You took" + STR$(A) + " holds of organics."
         GOSUB 80
      END IF
      A = N(1)
      IF F = 0 THEN
         A$ = "Your cargo holds are full."
         GOSUB 80
         GOTO 32360
       ELSEIF F < A THEN
         A = F
      END IF
      IF F <> 0 THEN
         F = F - A
         H(1) = H(1) + A
         N(1) = N(1) - A
         GET 1,PN
         LSET FI$ = MKI$(CVI(FI$) + A)
         PUT 1,PN
         GET 1,L2
         LSET FF$ = MKI$(CVI(FF$) - A)
         PUT 1,L2
         A$ = "You took" + STR$(A) + " holds of ore."
         GOSUB 80
      END IF
      GOTO 32360
32620 A$ = "<Increase productivity>"
      GOSUB 80
      A$ = ""
      GOSUB 80
      GET 1,PN
      A$ = "You have" + STR$(CVI(FL$)) + " credits."
      GOSUB 80
      A$ = "1 - Ore costs 200"
      GOSUB 80
      A$ = "2 - Organics costs 400"
      GOSUB 80
      A$ = "3 - Equipment costs 700."
      GOSUB 80
32630 A$ = "Which one do you want to increase (1,2,3)? "
      GOSUB 100
      GOSUB 400
	  IF A$ = "" OR LEN(A$) > 1 THEN _
         32360
      A = VAL(A$)
      IF A < 1 OR A > 4 THEN _
         32360
32640 IF A <> 1 THEN _
         32670
      IF P(1) > 19 THEN
         A$ = "It's at its maximum value."
         GOSUB 80
         GOTO 32360
      END IF
      A$ = "Ore: Increase by how many units? "
      GOSUB 100
      GOSUB 400
	  IF (A$ = "") OR (LEN(A$) > 5) THEN _
         32360
      A = VAL(A$)
      IF A < 1 THEN _
         32360
      IF A + P(1) > 20 THEN
         A$ = "The most it can be is 20 units."
         GOSUB 80
         A = 1
         GOTO 32640
      END IF
      GET 1,PN
      B = CVI(FL$)
      IF A*200 > B THEN
         A$ = "You're too poor.  You only have" + _
              STR$(CVI(FL$)) + " credits."
         GOSUB 80
         A = 1
         GOTO 32640
       ELSE GET 1,PN
         LSET FL$ = MKI$(B - 200*A)
         PUT 1,PN
         GET 1,L2
         LSET FC$ = MKI$(CVI(FC$) + A)
         PUT 1,L2
         GOTO 32310
      END IF
32670 IF A <> 2 THEN _
         32700
      IF P(2) > 19 THEN
         A$ = "It's at its maximum value."
         GOSUB 80
         GOTO 32360
      END IF
      A$ = "Organics: Increase by how many units? "
      GOSUB 100
      GOSUB 400
	  IF (A$ = "") OR (LEN(A$) > 5) THEN _
         32360
      A = VAL(A$)
      IF A < 1 THEN _
         32360
      IF A + P(2) > 20 THEN
		 A$ = "The most it can be is 20 units."
         GOSUB 80
         A = 2
         GOTO 32640
      END IF
      GET 1,PN
      B = CVI(FL$)
      IF A*400 > B THEN
         A$ = "You're too poor.  You only have" + _
              STR$(CVI(FL$)) + " credits."
         GOSUB 80
         A = 2
         GOTO 32640
       ELSE GET 1,PN
         LSET FL$ = MKI$(B - 400*A)
         PUT 1,PN
         GET 1,L2
         LSET FD$ = MKI$(CVI(FD$) + A)
         PUT 1,L2
         GOTO 32310
      END IF
32700 IF A <> 3 THEN _
         32630
      IF P(3) > 19 THEN
         A$ = "It's at its maximum value."
         GOSUB 80
         GOTO 32360
      END IF
      A$ = "Equipment: Increase by how many units? "
	  GOSUB 100
	  GOSUB 400
	  IF (A$ = "") OR (LEN(A$) > 5) THEN _
		 32360
      A = VAL(A$)
      IF A < 1 THEN _
         32360
      IF A + P(3) > 20 THEN
         A$ = "The most it can be is 20 units."
         GOSUB 80
         A = 3
         GOTO 32640
      END IF
      GET 1,PN
      B = CVI(FL$)
      IF A*700 > B THEN
         A$ = "You're too poor.  You only have" + _
              STR$(CVI(FL$)) + " credits."
         GOSUB 80
         A = 3
         GOTO 32640
       ELSE GET 1,PN
         LSET FL$ = MKI$(B - 700*A)
         PUT 1,PN
         GET 1,L2
         LSET FE$ = MKI$(CVI(FE$) + A)
         PUT 1,L2
         GOTO 32310
      END IF
32730 A$ = ""
	  GOSUB 80
	  FC = 1
	  BC = 7
	  BLINK = 1
	  HIGH = 1
      A$ = "*** DESTROY THE PLANET ***"
      GOSUB 80
	  A$ = ""
	  FC = 3
	  BC = 1
      GOSUB 80
      A$ = "Are you sure (Y/N)[N]? "
      GOSUB 100
      GOSUB 400
      A$ = LEFT$(A$ + "N",1)
      IF A$ <> "Y" AND A$ <> "y" THEN _
         32360
      GET 1,L2
      LSET FM$ = MKI$(0)
      PUT 1,L2
      GET 1,S2
      LSET FO$ = MKI$(0)
      PUT 1,S2
      GET 1,PN
      A$ = "  -  " + LEFT$(FA$,CVI(FM$)) + _
		   " destroyed the planet in sector" + STR$(S2 - LP)
	  CALL SYSOP.MESSAGE(A$)
      A$ = ""
      GOSUB 80
      A$ = "Planet destroyed."
      GOSUB 80
	  GOTO 20000
32760 REM **/ Update planet values and load into H(),N(),P() /**
	  GET 1,S2
      IF CVI(FO$) = 0 THEN _
         RETURN
      L2 = CVI(FO$) + LT
      GET 1,PN
      H(0) = CVI(FH$)
      H(1) = CVI(FI$)
      H(2) = CVI(FJ$)
      H(3) = CVI(FK$)
      GET 1,L2
      N(1) = CVI(FF$) + CVI(FI$)/10000
      N(2) = CVI(FG$) + CVI(FJ$)/10000
      N(3) = CVI(FH$) + CVI(FK$)/10000
      P(1) = CVI(FC$)
      P(2) = CVI(FD$)
      P(3) = CVI(FE$)
	  CALL GET.D(D,AY)
      C = D
      A$ = TIME$
      M = VAL(LEFT$(A$,2))*60 + VAL(MID$(A$,4,2))
      D = D - CVI(FB$) + (M-CVI(FR$))/1440
      IF D < 0 THEN _
         D = 0
      IF D > 10 THEN _
         D = 10
      FOR L = 1 TO 3
        N(L) = N(L) + P(L)*D
        IF N(L) > P(L)*10 THEN _
           N(L)=P(L)*10
      NEXT
      GET 1,L2
      LSET FB$ = MKI$(C)
      LSET FF$ = MKI$(INT(N(1)))
      LSET FG$ = MKI$(INT(N(2)))
      LSET FH$ = MKI$(INT(N(3)))
      FOR L = 1 TO 3
        S(L,0) = INT((N(L) - INT(N(L)))*10000 + .5)
        N(L) = INT(N(L))
      NEXT
      LSET FI$ = MKI$(S(1,0))
      LSET FJ$ = MKI$(S(2,0))
      LSET FK$ = MKI$(S(3,0))
      LSET FR$ = MKI$(M)
      PUT 1,L2
	  RETURN
32799 REM  **/Join a Team /**
	  INACT = 0
	  FC = 8
	  BC = 1
	  A$ = "<Team menu>"
	  GOSUB 80
	  A$ = ""
	  GOSUB 80
32800 P.LINE = 0
	  A$ = ""
	  GOSUB 80
	  A$ = "1)  Exit Team menu"
	  FC = 3
	  HIGH = 1
	  GOSUB 80
	  A$ = "2)  Create a Team"
	  FC = 6
	  HIGH = 1
	  GOSUB 80
	  A$ = "3)  Join a Team"
	  FC = 2
	  HIGH = 1
	  GOSUB 80
	  A$ = "4)  Quit a Team"
	  FC = 7
	  HIGH = 1
	  GOSUB 80
	  GET 1,PN
	  IF CVI(FE$) = 1 THEN
		 A$ = "5)  Cash Transfer"
		 FC = 5
		 HIGH = 1
		 GOSUB 80
		 A$ = "6)  Fighter Transfer"
		 FC = 0
		 HIGH = 1
		 GOSUB 80
		 A$ = "7)  Search for Team member(s)"
		 FC = 4
		 HIGH = 1
		 GOSUB 80
	  END IF
32810 T.IMER = 1
	  A$ = ""
	  GOSUB 80
	  FC = 7
	  A$ = "Team Command (?=help)? "
	  GOSUB 100
	  GOSUB 400
	  IF A$ = "?" THEN _
		 32800
	  IF A$ = "1" THEN _
		 20000
	  IF A$ = "2" THEN
		 GET 1,PN
		 IF CVI(FE$) <> 0 THEN
			A$ = "You already belong to a Team! "
			GOSUB 80
			A$ = "You must quit a team to form another Team."
			GOSUB 80
			GOTO 32810
		 END IF
32811    A$ = "To form a Team you will need to input a 4 character Team Password"
		 GOSUB 80
		 A$ = "Enter Password [ENTER]=quit? "
		 GOSUB 100
		 GOSUB 400
		 IF A$ = "" THEN _
			32810
		 IF LEN(A$) > 4 OR LEN(A$) < 4 THEN
			A$ = "Password MUST be 4 characters"
			GOSUB 80
			GOTO 32811
		 END IF
		 TPASS$ = A$
		 A$ = ""
		 GOSUB 80
		 A$ = "REMEMBER YOUR TEAM PASSWORD!"
		 GOSUB 80
		 GET 1,1
		 HTN = CVI(FT$)
		 FOR A97 = 1 TO HTN
			 GET 1,(LT + A97)
			 IF CVI(FQ$) = 0 THEN
				INACT = 1
				NTN = A97
				EXIT FOR
			 END IF
		 NEXT
		 IF INACT = 0 THEN
			NTN = HTN + 1
			GET 1,1
			LSET FT$ = MKI$(NTN)
			PUT 1,1
		 END IF
		 GET 1,PN
		 LSET FE$ = MKI$(1)
		 LSET FR$ = MKI$(NTN)
		 PUT 1,PN
		 GET 1,LT + NTN
		 LSET FQ$ = MKI$(1)
		 LSET FT$ = MKI$(1)
		 LSET FV$ = TPASS$
		 PUT 1,LT + NTN
		 TN = NTN
		 A$ = ""
		 GOSUB 80
		 FC = 3
		 BC = 1
		 BLINK = 1
		 HIGH = 1
		 A$ = FIRST$ + " " + LAST$ + " Created Team" + STR$(TN) + _
			  " the " + TNAM$
		 CALL SYSOP.MESSAGE(A$)
		 A$ = "Team number [" + _
			  RIGHT$(STR$(NTN),LEN(STR$(NTN))-1) + _
			  "] CREATED!"
		 GOSUB 80
		 FC = 8
		 BC = 1
	  END IF
32820 IF A$ = "3" THEN
		 GET 1,PN
		 IF CVI(FE$) <> 0 THEN
			A$ = "You already belong to a Team! "
			GOSUB 80
			A$ = "You must quit one team to join another Team."
			GOSUB 80
			GOTO 32810
		 END IF
		 GET 1,1
		 HTN = CVI(FT$)
		 A$ = "What Team number do you wish to join (0=quit)? "
		 GOSUB 100
		 GOSUB 400
		 IF LEN(A$) > 2 THEN _
			32820
		 A = VAL(A$)
		 IF A <= 0 THEN _
			32810
		 IF A < 1 OR A > HTN THEN
			A$ = "Invalid Team number!"
			GOSUB 80
			GOTO 32820
		 END IF
		 Z = A
		 GET 1,LT + Z
		 IF CVI(FT$) > 3 THEN
			A$ = "The Team you picked has a maximum amount of members!"
			GOSUB 80
			GOTO 32810
		 END IF
		 PW$ = FV$
		 PRINT "Password: ";PW$
		 A$ = "Please enter Password to Join Team #" + STR$(Z) + " ? "
		 GOSUB 100
		 GOSUB 400
		 IF A$ <> PW$ THEN
			A$ = "Invalid Password entered!"
			GOSUB 80
			GOTO 32810
		 END IF
		 GET 1,PN
		 LSET FE$ = MKI$(1)
		 LSET FR$ = MKI$(Z)
		 PUT 1,PN
		 GET 1,LT + Z
		 NN = CVI(FT$)
		 NN = NN + 1
		 LSET FT$ = MKI$(NN)
		 PUT 1,LT + Z
		 TN = Z
		 FC = 14
		 BC = 1
		 A$ = FIRST$ + " " + LAST$ + " Joined Team" + STR$(TN)
		 CALL SYSOP.MESSAGE(A$)
		 A$ = "Your Team info has been recorded!  Have fun!"
		 GOSUB 80
		 FC = 8
		 BC = 1
	  END IF
	  IF A$ = "4" THEN
		 GET 1,PN
		 IF CVI(FE$) = 0 THEN
			A$ = "You don't belong to a Team!"
			GOSUB 80
			GOTO 32810
		 END IF
		 A$ = "Are you sure you wish to quit your Team [N]? "
		 GOSUB 100
		 GOSUB 400
		 IF A$ = "" THEN _
			A$ = "N"
		 IF A$ = "Y" OR A$ = "y" THEN
			GET 1,PN
			OTN = CVI(FR$)
			LSET FE$ = MKI$(0)
			LSET FR$ = MKI$(0)
			PUT 1,PN
			GET 1,LT + OTN
			NN = CVI(FT$)
			NN = NN - 1
			LSET FT$ = MKI$(NN)
			PUT 1,LT + OTN
			IF NN = 0 THEN
			   GET 1,LT + OTN
			   LSET FQ$ = MKI$(0)
			   LSET FV$ = SPACE$(4)
			   PUT 1,LT + OTN
			END IF
			A$ = ""
			GOSUB 80
			FC = 26
			BC = 1
			A$ = "You have been removed from Team play"
			GOSUB 80
			FC = 8
			BC = 1
		 END IF
		 GOTO 32810
	  END IF
	  IF A$ = "5" THEN
32840    A$ = "Transfer Credits to another Team member [N]? "
		 GOSUB 100
		 GOSUB 400
		 IF A$ = "" THEN _
			A$ = "N"
		 IF A$ = "Y" OR A$ = "y" THEN
			GET 1,S2
			A = CVI(FI$)
			GET 1,A
			IF CVI(FO$) = 0 THEN
			   A$ = "There is no one else in this sector."
			   GOSUB 80
			   GOTO 32810
			END IF
			GET 1,PN
			IF CVI(FL$) < 1 THEN
			   A$ = "You don't have any Credits."
			   GOSUB 80
			   GOTO 32810
			END IF
32841       GET 1,A
			P = CVI(FP$)
			IF P = PN THEN _
			   32842
			GET 1,P
			IF CVI(FR$) <> TN THEN _
			   32842
			A$ = "Transfer Credits to " + LEFT$(FA$,CVI(FM$)) + " (Y/[N])? "
			GOSUB 100
			GOSUB 400
			IF A$ = "" THEN _
			   A$ = "N"
			IF (A$ = "Y" OR A$ = "y") THEN _
			   32843
32842       GET 1,A
			A = CVI(FO$)
			IF A <> 0 THEN _
			   32841
			A$ = "There is no one else in this sector"
			GOSUB 80
			GOTO 32810
32843       GET 1,PN
			M = CVI(FL$)
			A$ = "You have" + STR$(M) + " credits."
			GOSUB 80
			GET 1,P
			OPC = CVI(FL$)
			A$ = LEFT$(FA$,CVI(FM$)) + " has" + STR$(OPC) + " credits."
			GOSUB 80
			A$ = "How many Credits do you wish to Transfer? "
			GOSUB 100
			GOSUB 400
			IF LEN(A$) > 5 THEN _
			   32843
			CTT = VAL(A$)
			IF CTT <= 0 THEN _
			   32810
			IF CTT > M THEN
			   A$ = "You don't have that many Credits!
			   GOSUB 80
			   GOTO 32843
			END IF
			IF (CTT + OPC) > 32000 THEN _
			   32843
			IF (CTT + OPC) > 25000 THEN
			   A$ = "You Team mate will have more than 25,000 credits"
			   GOSUB 80
			   A$ = "Do you wish to complete the Transfer [N]? "
			   GOSUB 100
			   GOSUB 400
			   IF A$ = "" THEN _
				  A$ = "N"
			   IF A$ = "N" OR A$ = "n" THEN _
				  32843
			END IF
			GET 1,PN
			LSET FL$ = MKI$(M - CTT)
			PUT 1,PN
			GET 1,P
			LSET FL$ = MKI$(OPC + CTT)
			PUT 1,P
			A$ = ""
			GOSUB 80
			A$ = "Credits Transfered!"
			GOSUB 80
		 END IF
		 GOTO 32810
	  END IF
	  IF A$ = "6" THEN
32845    A$ = "Transfer Fighters to another Team member [N]? "
		 GOSUB 100
		 GOSUB 400
		 IF A$ = "" THEN _
			A$ = "N"
		 IF A$ = "Y" OR A$ = "y" THEN
			GET 1,S2
			A = CVI(FI$)
			GET 1,A
			IF CVI(FO$) = 0 THEN
			   A$ = "There is no one else in this sector."
			   GOSUB 80
			   GOTO 32810
			END IF
			GET 1,PN
			IF CVI(FL$) < 1 THEN
			   A$ = "You don't have any Fighters."
			   GOSUB 80
			   GOTO 32810
			END IF
32846       GET 1,A
			P = CVI(FP$)
			IF P = PN THEN _
			   32847
			GET 1,P
			IF CVI(FR$) <> TN THEN _
			   32847
			A$ = "Transfer Fighters to " + LEFT$(FA$,CVI(FM$)) + " (Y/[N])? "
			GOSUB 100
			GOSUB 400
			IF A$ = "" THEN _
			   A$ = "N"
			IF A$ = "Y" OR A$ = "y" THEN _
			   32848
32847       GET 1,A
			A = CVI(FO$)
			IF A <> 0 THEN _
			   32846
			A$ = "There is no one else in this sector"
			GOSUB 80
			GOTO 32810
32848       GET 1,PN
			FM = CVI(FG$)
			A$ = "You have" + STR$(FM) + " Fighters."
			GOSUB 80
			GET 1,P
			OPF = CVI(FG$)
			A$ = LEFT$(FA$,CVI(FM$)) + " has" + STR$(OPF) + " fighters."
			GOSUB 80
			A$ = ""
			GOSUB 80
			A$ = "How many Fighters do you wish to Transfer? "
			GOSUB 100
			GOSUB 400
			IF LEN(A$) > 4 THEN _
			   32848
			FTT = VAL(A$)
			IF FTT <= 0 THEN _
			   32810
			IF FTT > FM THEN
			   A$ = "You don't have that many Fighters."
			   GOSUB 80
			   GOTO 32848
			END IF
			IF (FTT + OPF) > 9999 THEN
			   A$ = "Maximum amount of fighters per player is 9999"
			   GOSUB 80
			   GOTO 32848
			END IF
			GET 1,PN
			LSET FG$ = MKI$(FM - FTT)
			PUT 1,PN
			GET 1,P
			LSET FG$ = MKI$(OPF + FTT)
			PUT 1,P
			A$ = ""
			GOSUB 80
			A$ = "Fighters Transfered!"
			GOSUB 80
		 END IF
		 GOTO 32810
	  END IF
	  IF A$ = "7" THEN
		 A$ = "Locating Other Team members."
		 GOSUB 80
		 A$ = ""
		 GOSUB 80
		 A$ = "Name                                     Sector"
		 GOSUB 80
		 A$ = "====================                     ======"
		 GOSUB 80
		 MF = 0
		 FOR NP = 2 TO LP
			 GET 1,NP
			 IF CVI(FE$) = 1 THEN
				IF CVI(FR$) = TN AND (NP <> PN) THEN
				   NUM = CVI(FF$)
				   ST$ = STR$(NUM)
				   IF NUM = 0 THEN _
					  ST$ = "Dead!"
				   A$ = FA$ + ST$
				   GOSUB 80
				   MF = 1
				END IF
			 END IF
		 NEXT
		 A$ = ""
		 GOSUB 80
		 IF MF = 0 THEN
			A$ = "None Found"
			GOSUB 80
		 END IF
	  END IF
	  GOTO 32810
32850 A$ = "<Port>"
	  GOSUB 80
	  A$ = ""
	  FC = 3
	  BC = 4
	  GOSUB 80
	  A$ = "Docking..."
	  GOSUB 80
32860 GET 1,PN
      A = CVI(FD$)
      IF A < 1 THEN
         A$ = "Sorry, but you don't have any turns left."
         GOSUB 80
		 GOTO 20000
      END IF
      GET 1,S2
      ST = CVI(FH$)
      P2 = ST + LS
      IF ST = 0 THEN
         A$ = "There are no ports in this sector."
         GOSUB 80
		 GOTO 20000
      END IF
      GET 1,PN
      LSET FD$ = MKI$(A - 1)
      PUT 1,PN
      A$ = "One turn deducted"
      GOSUB 80
      GET 1,P2
      IF PR = 1 THEN _
         GOTO 33200
      GOSUB 33000
      GET 1,PN
      H(0) = CVI(FH$)
      H(1) = CVI(FI$)
      H(2) = CVI(FJ$)
      H(3) = CVI(FK$)
      GOSUB 33100
      A$ = ""
      GOSUB 80
      F2 = 0
      FOR L2 = 1 TO 3
        IF C(L2) < 0 THEN _
           GOSUB 33330
      NEXT
      FOR L2 = 1 TO 3
        IF C(L2) > 0 THEN _
           GOSUB 33330
      NEXT L2
	  IF F2 = 0 THEN
		 FC = 6
		 BLINK = 1
         A$ = "You don't have anything they want, and they don't have anything you can buy"
         GOSUB 80
      END IF
      A$ = ""
      GOSUB 80
      GET 1,PN
      A$ = "You have" + STR$(CVI(FL$)) + _
           " credits and" + STR$(H(0) - H(1) - H(2) - H(3)) + _
           " empty cargo holds."
      GOSUB 80
	  GOTO 20000
33000 REM /** Update port and load values into the arrays **/
      GET 1,S2
      P2 = CVI(FH$) + LS
      GET 1,P2
      N(1) = CVI(FD$) + CVI(FR$)/10000
      N(2) = CVI(FE$) + CVI(FO$)/10000
      N(3) = CVI(FF$) + CVI(FP$)/10000
      P(1) = CVI(FG$)
      P(2) = CVI(FH$)
      P(3) = CVI(FI$)
      C(1) = CVI(FJ$)
      C(2) = CVI(FK$)
      C(3) = CVI(FL$)
	  CALL GET.D(D,AY)
      C = D
      A$ = TIME$
      M = VAL(LEFT$(A$,2))*60 + VAL(MID$(A$,4,2))
      D = D - CVI(FC$) + (M - CVI(FQ$))/1440
      IF D < 0 THEN _
         33060
      IF D > 10 THEN _
         D = 10
      FOR L = 1 TO 3
        N(L) = N(L) + P(L)*D
        IF N(L) > P(L)*10 THEN _
           N(L) = P(L)*10
      NEXT
33060 FOR L = 1 TO 3
        M(L) = INT(B(L)*(1 - C(L)*N(L)/P(L)/1000) + .5)
      NEXT
      GET 1,P2
      LSET FC$ = MKI$(C)
      LSET FD$ = MKI$(INT(N(1)))
      LSET FE$ = MKI$(INT(N(2)))
      LSET FF$ = MKI$(INT(N(3)))
      FOR L = 1 TO 3
        S(L,0) = INT((N(L) - INT(N(L)))*10000 + .5)
        N(L) = INT(N(L))
      NEXT
      LSET FR$ = MKI$(S(1,0))
      LSET FO$ = MKI$(S(2,0))
      LSET FP$ = MKI$(S(3,0))
      LSET FQ$ = MKI$(M)
      PUT 1,P2
      RETURN
33100 P.LINE = 0
      GET 1,PN
      H(0) = CVI(FH$)
      H(1) = CVI(FI$)
      H(2) = CVI(FJ$)
      H(3) = CVI(FK$)
      GET 1,P2
      A$ = ""
      GOSUB 80
      A$ = "Commerce report for " + LEFT$(FA$,CVI(FM$)) + _
           ": " + DATE$ + " " + TIME$
      GOSUB 80
      A$ = ""
      GOSUB 80
      A$ = " Items     Status   # units  in holds"
      GOSUB 80
	  A$ = " =====     ======   =======  ========"
	  FC = 0
	  HIGH = 1
      GOSUB 80
      FOR I = 1 TO 3
        A$ = P$(I)
        IF C(I) < 0 THEN
           A$ = A$ + " Buying "
         ELSE A$ = A$ + " Selling"
        END IF
        B$ = STR$(N(I))
        L = 7
		CALL ADD.SPACE(B$,L)
        A$ = A$ + B$
        L = 9
        B$ = STR$(H(I))
		CALL ADD.SPACE(B$,L)
		A$ = A$ + B$
		FC = I
        GOSUB 80
      NEXT
      RETURN
33150 CALL GET.D(D,AY)
      M(1) = INT(50*SIN(.89756*D) + .5)
      M(2) = INT(10*SIN(.89714*D + 1.5707) + .5)
      A$ = ""
      GOSUB 80
      P.LINE = 0
      M(1) = M(1) + 500
      M(2) = M(2) + 100
      A$ = "Commerce report for Sol: " + DATE$ + " " + TIME$
      GOSUB 80
      A$ = "  Cargo holds:" + STR$(M(1)) + " credits/hold"
      GOSUB 80
      A$ = "  Fighters   :" + STR$(M(2)) + " credits/fighter"
      GOSUB 80
      A$ = "  Turns      : 300 credits each."
      GOSUB 80
      A$ = ""
      RETURN
33200 GOSUB 33150
      GET 1,PN
      C2 = CVI(FL$)
      A$ = "You have" + STR$(C2) + " credits."
      GOSUB 80
      A$ = ""
      GOSUB 80
33210 A$ = "How many holds do you want to buy [0]? "
      GOSUB 100
	  GOSUB 400
	  IF LEN(A$) > 4 THEN _
		 33210
      A = INT(VAL(A$))
      IF A < 0 THEN _
         33210
      IF M(1)*A > C2 THEN
         A$="You don't have enough money.  The maximum amount you can buy is" + _
         STR$(INT(C2/M(1))) + "."
         GOSUB 80
         GOTO 33210
      END IF
      IF A + CVI(FH$) > 50 THEN
         A$ = "You are limited to 50 cargo holds."
         GOSUB 80
         GOTO 33210
      END IF
      LSET FH$ = MKI$(CVI(FH$) + A)
      C2 = C2 - M(1)*A
      LSET FL$ = MKI$(C2)
      PUT 1,PN
      A$ = "You have" + STR$(C2) + " credits."
      GOSUB 80
      A$ = ""
      GOSUB 80
33250 A$ = "How many fighters do you want to buy [0]? "
      GOSUB 100
	  GOSUB 400
	  IF LEN(A$) > 4 THEN _
		 33250
      A = INT(VAL(A$))
      IF A < 0 THEN _
         33250
      IF M(2)*A > C2 THEN
         A$ = "You don't have enough money.  The most you can buy is" + _
              STR$(INT(C2/M(2))) + "."
         GOSUB 80
         GOTO 33250
      END IF
      IF A + CVI(FG$) > 9999 THEN
         A$ = "Your fleet is limited to 9999 fighters."
         GOSUB 80
         GOTO 33250
      END IF
      LSET FG$ = MKI$(CVI(FG$) + A)
      C2 = C2 - M(2)*A
      LSET FL$ = MKI$(C2)
      PUT 1,PN
      A$ = "You have" + STR$(C2) + " credits."
      GOSUB 80
      A$ = ""
      GOSUB 80
33290 A$ = "How many turns do you want to buy [0]? "
      GOSUB 100
	  GOSUB 400
	  IF LEN(A$) > 4 THEN _
		 33290
	  A = INT(VAL(A$))
      IF A < 0 THEN _
         33290
      IF A*300 > C2 THEN
         A$ = "You don't have enough money.  The most you can buy is" + _
              STR$(INT(C2/300)) + "."
         GOSUB 80
         GOTO 33290
      END IF
      LSET FD$ = MKI$(CVI(FD$) + A)
      C2 = C2 - 300*A
      LSET FL$ = MKI$(C2)
      PUT 1,PN
      A$ = "You have" + STR$(C2) + " credits."
      GOSUB 80
      A$ = ""
      GOSUB 80
	  GOTO 20000
33330 GET 1,PN
	  M2 = CVI(FL$)
	  IF M2 > 30000 THEN _
		 GOTO 19900
      H = H(0) - H(1) - H(2) - H(3)
      IF C(L2) > 0 THEN
           D$ = "buy"
           E$ = "sell"
           B = -1
       ELSE D$ = "sell"
           E$ = "buy"
           B = 0
      END IF
      IF B THEN
         M = H
         IF M > N(L2) THEN _
         M = N(L2)
      END IF
      IF B THEN
         IF M*M(L2) > M2 THEN _
            M = INT(M2/M(L2))
      END IF
      IF B = 0 THEN
         M = N(L2)
         IF M > H(L2) THEN _
         M = H(L2)
      END IF
      IF M = 0 THEN _
         RETURN
      A$ = ""
      GOSUB 80
      A$ = "You have" + STR$(M2) + _
           " credits and" + STR$(H) + _
           " empty cargo holds."
      GOSUB 80
      A$ = ""
      GOSUB 80
      A$ = "We are " + E$ + _
           "ing up to" + STR$(N(L2)) + _
           ".  You have" + STR$(H(L2)) + _
           " in your holds."
      GOSUB 80
33400 F2 = 1
      A$ = "How many holds of "
      IF L2 = 1 THEN
         A$ = A$ + "Ore"
       ELSEIF L2 = 2 THEN
         A$ = A$ + "Organics"
       ELSE A$ = A$ + "Equipment"
      END IF
      A$ = A$ + " do you want to " + D$ + " ["
      B$ = STR$(M)
      A$ = A$ + RIGHT$(B$,LEN(B$)-1) + "]? "
      GOSUB 100
	  GOSUB 400
	  IF LEN(A$) > 2 THEN _
		 33400
      IF A$ = "" THEN _
         N = M ELSE _
         N = INT(VAL(A$))
      IF N < 1 THEN _
         RETURN
      IF B AND (N > H) THEN
         A$ = "You don't have enough cargo holds."
         GOSUB 80
         GOTO 33400
      END IF
      IF B AND (N > N(L2)) THEN
         A$ = "They're not selling that many."
         GOSUB 80
         GOTO  33400
      END IF
      IF B = 0 AND N > N(L2) THEN
         A$ = "They don't want that many."
         GOSUB 80
         GOTO 33400
      END IF
      IF B = 0 AND N > H(L2) THEN
         A$ = "You don't have that many in your holds."
         GOSUB 80
         GOTO 33400
      END IF
      A = 3
	  CALL RAND(V,A)
      A$ = "Agreed," + STR$(N) + " units."
      GOSUB 80
	  FOR R = 1 TO V + 1
        A$ = ""
        GOSUB 80
        IF R = V + 1 THEN
           A$ = "Our final offer is"
         ELSEIF B THEN
           A$ = "We'll sell them for"
         ELSE A$ = "We'll buy them for"
        END IF
        A$ = A$ + STR$(INT(N*M(L2)*(1 + C(L2)/1000) + .5)) + " credits."
        GOSUB 80
33490   A$ = "Your offer? "
        GOSUB 100
		GOSUB 400
		IF LEN(A$) > 6 THEN _
		   33490
        A = VAL(A$ + " ")
        IF A < M(L2)*N/10 OR A > M(L2)*N*10 THEN
           A$ = ""
           GOSUB 80
		   A$ = "That was pretty funny, Now make a serious offer."
           GOSUB 80
           GOTO 33490
        END IF
        IF A > M2 AND B THEN
           A$ = "  You only have" + STR$(M2) + " credits!"
           GOSUB 80
           GOTO 33490
        END IF
        IF B = 0 AND A <= M(L2)*N THEN
           A$ = "We'll take them!"
           GOSUB 80
           GOTO 33570
         ELSEIF B AND (A >= M(L2)*N) THEN
           A$ = "Sold!"
           GOSUB 80
           GOTO 33570
        END IF
        T = INT(N*M(L2)*(1 - C(L2)/250/R) + .5)
        IF B = 0 AND A > T THEN
           GOTO 33540
		 ELSEIF B AND (A < T) THEN
           GOTO 33540
        END IF
        M(L2) = .7*M(L2) + .3*A/N
      NEXT R
33540 GOSUB 33550
      IF B = 0 THEN
         A$ = "Too high.  We'll buy them from local traders."
         GOSUB 80
         GOTO 33610
       ELSE A$ = ""
         GOSUB 80
         A$ = "Too low.  We'll sell them to local traders."
         GOSUB 80
         GOTO 33610
      END IF
33550 GET 1,P2
      IF L2 = 1 THEN
         LSET FD$ = MKI$(N(1) - N)
       ELSEIF L2 = 2 THEN
         LSET FE$ = MKI$(N(2) - N)
       ELSE LSET FF$ = MKI$(N(3) - N)         ' Deduct units from port
      END IF
      PUT 1,P2
      RETURN
33570 S = SGN(C(L2))
	  GET 1,PN
      LSET FL$ = MKI$(M2 - S*A)
      IF L2 = 1 THEN
         LSET FI$ = MKI$(H(1) + S*N)
       ELSEIF L2 = 2 THEN
         LSET FJ$ = MKI$(H(2) + S*N)
       ELSE LSET FK$ = MKI$(H(3) + S*N)
      END IF
      PUT 1,PN
      GOSUB 33550
      PUT 1,P2
      H(L2) = H(L2) + S*N
33610 RETURN
33640 A$ = "<Computer>"
	  GOSUB 80
	  A$ = ""
	  GOSUB 80
	  A$ = "<Computer activated>"
	  FC = 7
	  BC = 5
	  GOSUB 80
33650 A$ = ""
	  GOSUB 80
	  P.LINE = 0
	  FC = 7
      A$ = "Computer commands:"
      GOSUB 80
	  A$ = "1)  Exit computer"
	  FC = 0
      GOSUB 80
	  A$ = "2)  Port report"
	  FC = 7
	  HIGH = 1
      GOSUB 80
	  A$ = "3)  Sector finder"
	  FC = 2
      GOSUB 80
	  A$ = "4)  Rank players"
	  HIGH = 1
	  FC = 3
	  GOSUB 80
	  A$ = "5)  Send Message"
	  HIGH = 1
	  FC = 0
	  GOSUB 80
33710 GET 1,PN
      PR = CVI(FF$)
	  S2 = PR + LP
	  T.IMER = 1
      A$ = ""
	  GOSUB 80
	  FC = 6
	  HIGH = 1
      A$ = "Computer command (?=help)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "?" THEN _
         33650
      IF A$ = "1" THEN _
		 33990
      IF A$ = "2" THEN _
         33780
      IF A$ = "3" THEN _
         33830
      IF A$ = "4" THEN _
		 33900
	  IF A$ = "5" THEN _
		 33960
      A$ = "Invalid command.  Enter ? for help"
      GOSUB 80
      GOTO 33710
33780 A$ = "What sector number is the port in? "
      GOSUB 100
      GOSUB 400
	  IF A$ = "" OR (LEN(A$) > 4) THEN _
         33710
      A = VAL(A$)
      S2 = A + LP
      IF A < 1 OR A > LS - LP THEN
         A$ = "Valid sector numbers are from 1 to" + STR$(LS - LP) + "."
         GOSUB 80
         GOTO 33780
      END IF
      GET 1,LP + A
      P = CVI(FH$)
      IF P = 0 OR (CVI(FL$) > 0 AND CVI(FM$) <> PN) THEN
         A$ = ""
         GOSUB 80
         A$ = "I have no information about that port."
         GOSUB 80
         GOTO 33710
      END IF
      IF P <> 1 THEN
         GOSUB 33000
         GOSUB 33100
         GOTO 33710
      END IF
      GOSUB 33150
      GOTO 33710
33830 A = PR
      A$ = "What sector do you want to get to? "
      GOSUB 100
      GOSUB 400
	  IF A$ = "" OR (LEN(A$) > 4) THEN _
         33710
      B = VAL(A$)
      IF B < 1 OR B > LS - LP THEN
         A$ = "Valid sector numbers are from 1 to" + STR$(LS - LP) + "."
         GOSUB 80
         GOTO 33830
      END IF
      IF A = B THEN
         A$ = "You are in that sector."
         GOSUB 80
         GOTO 33710
      END IF
      A$ = ""
      GOSUB 80
	  A$ = "Computing shortest path."
	  GOSUB 100
	  CALL FIND.WARP(A,B,M.AINT)
      IF S(A,1) = 0 THEN
         A$ = "There was an error in computation between sectors."
         GOSUB 80
         GOTO 33890
      END IF
      A$ = ""
      GOSUB 80
      A$ = "The shortest path from sector" + STR$(A) + _
           " to sector" + STR$(B) + " is:"
      GOSUB 80
      A$ = STR$(A)
      GOSUB 100
      S = A
33880 S = S(S,1)
	  P.LINE = 0
      IF S <> 0 THEN
         A$ = " -" + STR$(S)
         GOSUB 100
         GOTO 33880
       ELSE A$ = ""
         GOSUB 80
      END IF
33890 GET 1,LP + PR
      E(1) = CVI(FB$)
      E(2) = CVI(FC$)
      E(3) = CVI(FD$)
      E(4) = CVI(FE$)
      E(5) = CVI(FF$)
      E(6) = CVI(FG$)
      GOTO 33710
33900 NOFSX$ = ""
      A$ = ""
      GOSUB 80
	  A$ = "Ranking players."
	  GOSUB 100
	  CALL RANK.PLAYERS(P)
      A$ = ""
      GOSUB 80
      A$ = "Player Rankings: " + DATE$ + " " + TIME$
      GOSUB 80
	  A$ = "Rank     Value      Team  Player"
      GOSUB 80
	  A$ = "==== ============= ====== ======"
      GOSUB 80
      R = 1
33930 GET 1,P
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
	  A$ = A$ + B$ + "   " + _
           LEFT$(FA$,CVI(FM$))
      GOSUB 80
      P = CVI(FT$)
      IF NOFSX$ = "Q" OR P = -1 THEN _
         33710
      R = R + 1
	  GOTO 33930
33960 REM **/ Message to Other Player /**
	  A$ = ""
	  GOSUB 80
	  A$ = "Warming up sub-space radio."
	  GOSUB 80
	  FOR SP = 1 TO 5000
	  NEXT SP
	  A$ = ""
	  GOSUB 80
	  A$ = "Who do you wish to send this message to (search string)? "
	  GOSUB 100
	  GOSUB 400
	  CALL NAM.ADJ(A$)
	  GOSUB 33968
	  IF P = 0 THEN _
		 33710
	  GET 1,P
	  A$ = "Tuning in to " + LEFT$(FA$,CVI(FM$)) + "'s frequency."
	  GOSUB 80
	  A$ = ""
	  GOSUB 80
33962 A$ = "Due to the distances involved, messages are limited to 74 chars."
	  GOSUB 80
	  A$ = "Pressing [ENTER] with no input quits"
	  GOSUB 80
	  A$ = "  [" + STRING$(72,"-") + "]"
	  GOSUB 80
	  A$ = "? "
	  GOSUB 100
	  GOSUB 400
	  IF A$ = "" THEN _
		 33710
	  IF LEN(A$) > 74 THEN
		 A$ = "Too Long"
		 GOSUB 80
		 GOTO 33962
	  END IF
	  CALL RADIO.MESSAGE(P,PN,A$)
	  A$ = "Message sent."
	  GOSUB 80
	  GOTO 33710
33968 REM **/ Get P, a user number from A$, a given an name or number. /**
	  REM **/ P = 0 = None /**
	  B$ = A$
	  IF A$ = "" THEN
		 P = 0
		 RETURN
	  END IF
	  IF VAL(A$) <> 0 THEN
		 P = VAL(A$)
		 RETURN
	  END IF
	  FOR L = 2 TO LP
		GET 1,L
		IF INSTR(LEFT$(FA$,CVI(FM$)),B$) = 0 THEN _
		   33969
		A$ = LEFT$(FA$,CVI(FM$)) + " (Y/N)[Y]? "
		GOSUB 100
		GOSUB 400
		IF A$ = "" OR A$ = "Y" OR A$ = "y" THEN
		   P = L
		   RETURN
		END IF
33969 NEXT L
	  A$ = "Not found."
	  GOSUB 80
	  P = 0
	  RETURN
33990 A$ = ""
      GOSUB 80
	  A$ = "<Computer deactivated>"
	  BC = 0
	  FC = 3
      GOSUB 80
	  GOTO 20000
33999 REM **/ TWMAINT /**
34000 M.AINT = 1
	  A$ = ""
      GOSUB 80
      A$ = ""
      GOSUB 80
      A$ = "Trade Wars maintence program"
      GOSUB 80
      A$ = "  by Chris Sherrick (PTL)"
      GOSUB 80
      A$ = ""
      GOSUB 80
      A$ = "This program is run once per day."
      GOSUB 80
      A$ = TIME$ + " " + _
           DATE$ + ": " + _
           FIRST$ + " " + _
           LAST$ + ": Maintence program ran"
	  CALL SYSOP.MESSAGE(A$)
	  CALL MSG.COMPRESS
      GET 1,1
      L2 = CVI(FK$)
      A$ = ""
      GOSUB 80
	  CALL GET.D(D,AY)
      L2 = D - L2
	  A$ = "Deleting inactive players"
	  GOSUB 80
      FOR P = 2 TO LP
        GET 1,P
        IF CVI(FB$) > L2 THEN _
		   34590
        IF CVI(FC$) = 0 OR CVI(FM$) = 0 THEN _
		   34590
        N1$ = LEFT$(FA$,CVI(FM$))
		CALL DEL.PLAYER(A$,P,PN)
        A$ = "  - " + N1$ + " deleted from game"
		CALL SYSOP.MESSAGE(A$)
34590 NEXT
34600 REM **/ Move Cabal /**
      A$ = ""
      GOSUB 80
	  A$ = "Moving the Cabal."
	  GOSUB 100
      A$ = "    Cabal report:"
	  CALL SYSOP.MESSAGE(A$)
      FOR L = 1 TO 9
        GET 1,L + LP
		G(L,0) = CVI(FT$)
        G(L,1) = 0
      NEXT
      FOR L = 1 TO 8
		FOR M = L + 1 TO 9
		IF G(L,0) = G(M,0) THEN _
             G(M,0)=0
        NEXT
      NEXT
      G = 0
      FOR L = 1 TO 9
		IF G(L,0) <> 0 THEN
           GET 1,G(L,0) + LP
		   IF CVI(FM$) = -1 THEN
			  G = G + CVI(FL$)
			  G(L,1) = CVI(FL$)
		   END IF
		END IF
      NEXT
      GET 1,1
      R = CVI(FR$)
      IF G < 2000 - R THEN
         E1 = R
	   ELSE E1 = 2000 - G
		 IF E1 < 0 THEN _
			E1 = 0                              ' Find how many to add.
	  END IF
      G = 2
      A = 83
      B = 85                                 'sectors 83 and 85 hardcoded
	  CALL MOVE.CABAL(A,B,G)             ' Move group 2 to sector 85
      GET 1,85 + LP
      IF CVI(FM$) <> -1 THEN
         G(1,1) = 1000
         LSET FM$ = MKI$(-1)
         LSET FL$ = MKI$(1000)
         PUT 1,85 + LP
      END IF
      A = CVI(FL$)
      LSET FL$ = MKI$(CVI(FL$) + E1)
      PUT 1,85 + LP
      S1 = G(1,1) + G(2,1) + E1
      IF S1 < 1500 THEN _
		 E1 = 1 ELSE _
		 E1 = 0
      IF S1 < 1000 THEN
         G(1,1) = S1
         G(2,0) = 0
         G(2,1) = 0
       ELSE G(1,1) = 1000
         G(2,1) = S1 - 1000
         G(2,0) = 85
      END IF
      G = 2
      A = 85
      B = 83
	  CALL MOVE.CABAL(A,B,G)              ' Move group 2 to sector 83
      FOR G1 = 3 TO 5                         ' Move group type II fighters
        IF G(G1,1) = 0 OR G(G1,0) = 0 THEN _
           34820
34760   GET 1,G1 + LP
        IF G(G1,0) = CVI(FQ$) OR CVI(FQ$) = 0 THEN
		   CALL PICK.RAND(V,A)
           LSET FQ$ = MKI$(V)
           PUT 1,G1 + LP
           GOTO 34760
        END IF
        IF G(G1,1) < 50 OR G(G1,1) > 100 THEN
           LSET FQ$ = MKI$(83)
           PUT 1,G1 + LP
        END IF
        IF E1 = 1 THEN
           LSET FQ$ = MKI$(85)
           PUT 1,G1 + LP
        END IF
        A = G(G1,0)
        B = CVI(FQ$)
		CALL FIND.WARP(A,B,M.AINT)
        IF S(A,1) = 0 THEN _
           34810
        B = S(A,1)
        G = G1
		CALL MOVE.CABAL(A,B,G)                ' Move 1 step toward goal
34810   GOTO 34840
34820   IF G(2,1) < 600 THEN _
           34840
        G(G1,1) = 100
        G(2,1) = G(2,1) - 100
        G(G1,0) = 83
        GOTO 34760                                ' Create a group II group
34840   IF (G(G1,0) > 0 AND G(G1,0) < 8) AND G(G1,1) <> 0 THEN _
		   34760
      NEXT G1
	  CALL RANK.PLAYERS(P)                'get top player & sector in
      IF P < 1 THEN
         SC1 = 0
         T1 = 0
       ELSE T1 = P
		 GET 1,T1
		 SC1 = CVI(FF$)
		 IF CVS(FV$) < 25000 THEN
			SC1 = 0
			T1 = 0
		 END IF
      END IF
      IF SC1 = 0 OR T1 = 0 THEN
         SC1 = 83
         T1 = 0
      END IF
      FOR G1 = 6 TO 9                 ' Move group type III groupes fighters
        IF G(G1,1) = 0 OR G(G1,0) = 0 THEN _
           35030
34890   CALL PICK.RAND(V,A)
        B1 = V
        IF G1 = 9 THEN
           B1 = SC1
         ELSEIF V = G(G1,0) THEN
           GOTO 34890
        END IF
        IF G(G1,1) < 20 OR G(G1,1) > 50 THEN _
           B1 = 83
        IF E1 THEN _
           B1 = 85
        A = G(G1,0)
        B = B1
		CALL FIND.WARP(A,B,M.AINT)
        IF S(G(G1,0),1) = 0 THEN _
           35050                                     ' Find path
34930   IF G(G1,1) < 0 OR G(G1,0) = 0 THEN
           G(G1,0) = 0
           G(G1,1) = 0
           GOTO 35050
		END IF
        IF G1 = 9 AND G(G1,0) = SC1 THEN _
           35000
        A = G(G1,0)
        B = S(G(G1,0),1)
        G = G1
		CALL MOVE.CABAL(A,B,G)                   ' Move one step
        IF G(G1,1) < 0 OR G(G1,0) = 0 THEN
           G(G1,0) = 0
           G(G1,1) = 0
           GOTO 35050
        END IF
        GET 1,G(G1,0) + LP
        IF G1 = 9 OR CVI(FI$) = 0 THEN _
           35000
        GET 1,CVI(FI$)
        P = CVI(FP$)
        F = 20
		G = G1
		CALL CABAL.ATTACK(M.AINT,P,F,G)                  ' Attack player
35000   IF G(G1,0) <> B1 THEN _
           34930
		IF T1 <> 0 AND G1 = 9 THEN
           P = T1
           F = G(G1,1)
           G = G1
		   CALL CABAL.ATTACK(M.AINT,P,F,G)
        END IF
        GOTO 35050
35030   IF G(2,1) < 550 THEN _
           35050
        G(G1,1) = 50
        G(2,1) = G(2,1) - 50
        G(G1,0) = 83
        GOTO 34890                                    ' Create group III
35050   IF (G(G1,0) > 0 AND G(G1,0) < 8) AND G(G1,1) <> 0 THEN
           S1 = 85
           GOTO 34890
        END IF
      NEXT
      FOR L = 1 TO 9
        GET 1,LP + L
        LSET FT$ = MKI$(G(L,0))
        PUT 1,LP + L
      NEXT
35420                                             ' Continue adding here
	  CALL GET.D(D,AY)
      GET 1,1
      LSET FL$ = MKI$(D)
      PUT 1,1
      GOSUB 2300
	  M.AINT = 0
	  GOTO 10000

' $INCLUDE: 'TWMERG2.BAS'

