 ' $INCLUDE: 'DMERG1.BAS'
 ' **/ Trade Wars, by Chris Sherrick (PTL) 5/86
 ' **/ Copyright 1986 Chris Sherrick
   P.RINT = 0
   DOOR.NAME$ = "TWEDIT"
   R.ELEASE$ = "2.E500T"
   FC = 7
   DIM M(3), _
       P$(3), _
       B(3), _
       E(6), _
       N(3), _
       P(6), _
       C(3), _
       H(3), _
	   S(500,1), _
       Z(3,3), _
       G(9,1)
   FS$ = "TWDATA.DAT"
   ON.AT$ = TIME$
   IF (SNOOP OR L.OCAL = 1) THEN _
	  GOSUB LINE25
   GOTO 10000
60 REM **/ Return to BBS /**
   CLOSE 1
   A$ = ""
   GOSUB 80
   A$ = "Returning to Tradewars..."
   GOSUB 80
   RUN "TW2"
80 REM **/ Print A$ + Carriage return and linefeed/*
   LN$ = A$
   NORET = 0
   IF P.RINT = 1 THEN
      LPRINT LN$
   END IF
   GOSUB OUT.PUT
   RETURN
100 REM **/ Print A$ through modem w/o LF /**
    LN$ = A$
    NORET = 1
    IF P.RINT = 1 THEN
       LPRINT LN$;
    END IF
	GOSUB OUT.PUT
    RETURN
200 REM **/ Get a value from modem (if any) and put it in A$ /**
    GOSUB IN.PUT
    A$ = ARG$
    RETURN
300 REM **/ V=RND(A) /**
    V = INT(RND(1)*A) + 1
    RETURN
400 REM **/ Line Input A$ /*
    GOSUB IN.PUT
    A$ = ARG$
    RETURN
1000 '   **/ Get year, month and day, and convert to days(D). AY=year game started.  Uses A /**
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
          D = D + 90                           'add days for what month
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
        Y = Y + 100                               'Past the end of the decade
     IF Y <> AY THEN
        FOR A = AY TO Y - 1
          D = D + 365
          IF A/4 = INT(A/4) THEN _
             D = D + 1                             'Add leap years past.
		  IF Y <> AY THEN _
			 EXIT FOR
        NEXT A
     END IF
     RETURN
2000 REM **/ Open data file /**
     O.PENFILE$ = FS$
     OPEN"R",1,O.PENFILE$,143
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
     RETURN
2300 REM **/ Close data file /**
     CLOSE 1
     O.PENFILE$=""
     RETURN
2500 REM **/ Rank players with FT$ and FV$.  P = starting person, 0=no players /**
     FOR L = 2 TO LP
       GET 1,L
       IF CVI(FM$) = 0 THEN
          LSET FV$ = MKS$(-1)
          PUT 1,L
          GOTO 2530
       END IF
       IF CVI(FC$)<>0 THEN
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
2530 NEXT
     FOR L = LP + 1 TO LS
       GET 1,L
       IF CVI(FL$) = 0 OR CVI(FM$) < 2 THEN _
          GOTO 2560
       A = CVI(FL$)
       P = CVI(FM$)
       GET 1,P
       LSET FV$ = MKS$(CVS(FV$)+A*100)
       PUT 1,P
2560 NEXT
     P = 0
     FOR L = 2 TO LP
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
2620 NEXT
     RETURN
2700 REM **/ Print messages to sysop /**
     GOSUB 2300
     OPEN"I",1,"TWMESG.DAT"
     A$ = ""
     GOSUB 80
2730 IF EOF(1) OR NOFSX$="Q" THEN
        CLOSE 1
        GOSUB 2000
        RETURN
     END IF
     LINE INPUT#1,A$
     GOSUB 80
     GOTO 2730
2900 REM **/ Add message A$ to messages to Sysop /**
     GOSUB 2300
     OPEN"A",1,"TWMESG.DAT"
     PRINT#1,A$
     CLOSE 1
     GOSUB 2000
     RETURN
4000 REM **/ Instructions /**
     A$ = ""
     GOSUB 80
     GOSUB 2300
     NOFSX$ = ""
     OPEN"I",1,"TWEDITOR.DOC"
4030 IF EOF(1) OR NOFSX$ = "Q" THEN _
        4050
     LINE INPUT #1,A$
     GOSUB 80
     GOTO 4030
4050 CLOSE 1
     GOSUB 2000
     RETURN
5800 REM **/ Open TWPMSG.DAT /**
     OPEN "R",1,"TWPMSG.DAT",8
     FIELD 1,2 AS FA$, _
             2 AS FB$, _
             2 AS FC$, _
             2 AS FD$
     RETURN
5900 REM **/ Close TWPMSG.DAT /**
     CLOSE 1
     RETURN
7000 REM **/ Add ship P person-in-sector chain /**
     GET 1,P
     R = CVI(FF$)
     IF R = 0 THEN _
        RETURN
     FOR L = 2 TO LP
       GET 1,L
	   IF CVI(FP$) = 0 THEN _
		  GOTO 7050
	 NEXT
	 A$ = "ERROR IN PLACING PEOPLE IN ROOMS - NOT ENOUGH ROOM!, LINE 7000"
	 GOSUB 2900
	 GOSUB 80
	 GOTO 60
7050 GET 1,LP + R
     A = CVI(FI$)
     IF L = A THEN _
        A = 0
     LSET FI$ = MKI$(L)
     PUT 1,LP + R
     GET 1,L
     LSET FO$ = MKI$(A)
     LSET FP$ = MKI$(P)
     PUT 1,L
     RETURN
7100 REM **/ Remove ship P from person-in-sector chain /**
     GET 1,P
     R = CVI(FF$)
     GET 1,LP + R
     A = CVI(FI$)
     IF A = 0 THEN _
        RETURN
     GET 1,A
     IF CVI(FP$) = P THEN
        B = CVI(FO$)
        LSET FP$ = MKI$(0)
        LSET FO$ = MKI$(0)
        PUT 1,A
        GET 1,LP + R
        LSET FI$ = MKI$(B)
        PUT 1,LP + R
        RETURN
     END IF
7130 L = CVI(FO$)
     IF L = 0 THEN _
        RETURN
     GET 1,L
     IF CVI(FP$) = P THEN
        B = CVI(FO$)
        LSET FP$ = MKI$(0)
        LSET FO$ = MKI$(0)
        PUT 1,L
        GET 1,A
        LSET FO$ = MKI$(B)
        PUT 1,A
        RETURN
     END IF
     A = L
     GOTO 7130
7500 REM **/ Find shortest route from A to B in S(500,1) /**
     N = 1
     C = B
     IF A = B THEN
        S(0,0) = A
        S(0,1) = 0
        S(A,1) = 0
        RETURN
     END IF
	 FOR L = 1 TO (LS - LP)
       FOR M = 0 TO 1
         S(L,M) = 0
	   NEXT
     NEXT
     S(A,1) = 1
7530 FOR L = 1 TO (LS - LP)
       IF S(L,1) <> N THEN _
          GOTO 7580
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
               IF E(M) = B THEN _
                  GOTO 7590
            END IF
         END IF
	   NEXT
	   PRINT ".";
7580 NEXT
     N = N + 1
     IF N < 9999 THEN _
        GOTO 7530
     A$ = "*** Error - Sector path not found - from sector" + _
          STR$(A) + " to sector" + STR$(B)
     GOSUB 2900
     GOSUB 80
     S(A,1) = 0
     RETURN
7590 S(S(C,0),1) = C
     C = S(C,0)
     IF S(C,0) <> 0 THEN _
        GOTO 7590
	 S(B,1) = 0
	 PRINT
     RETURN
8000 REM **/ Delete player P from game /**
     GET 1,P
     A$ = "Deleting " + LEFT$(FA$,CVI(FM$)) + "..."
     GOSUB 80
     GOSUB 7100
     GET 1,P
     LSET FM$ = MKI$(0)
     PUT 1,P
     FOR L = LP + 1 TO LS
       GET 1,L
       IF CVI(FM$) = P THEN
          LSET FM$ = MKI$(0)
          LSET FL$ = MKI$(0)
          PUT 1,L
       END IF
     NEXT
     GOSUB 2300
     GOSUB 5800
     GET 1,1
     LSET FB$ = MKI$(0)
     PUT 1,1
     FOR L = 2 TO CVI(FA$)
       GET 1,L
       IF CVI(FA$) = P THEN
          LSET FA$ = MKI$(0)
          LSET FB$ = MKI$(0)
          LSET FC$ = MKI$(0)
          LSET FD$ = MKI$(0)
          PUT 1,L
       END IF
       IF CVI(FB$) = 1 AND CVI(FC$) = P THEN
          LSET FC$ = MKI$(-98)
          PUT 1,L
       END IF
     NEXT
     GOSUB 5900
     GOSUB 2000
     FOR L = 2 TO LP
       GET 1,L
       IF CVI(FC$) = P THEN
          LSET FC$ = MKI$(-98)
          PUT 1,L
       END IF
     NEXT
	 RETURN

 ' $INCLUDE: 'DMERG2.BAS'

10000 A$ = "Trade Wars DATA Editor"
      GOSUB 80
      A$ = "By Chris Sherrick (PTL)"
      GOSUB 80
      A$ = ""
      GOSUB 80
      B(1) = 10
      B(2) = 20
      B(3) = 35
      IF L.OCAL = 1 THEN
         A$ = "Since you're in LOCAL mode, you don't need to enter a password."
         GOSUB 80
         GOTO 11000
      END IF
      A$ = "Password please: "
      GOSUB 100
      GOSUB 400
      GOSUB 2000
      GET 1,1
      IF A$ = LEFT$(FA$,CVI(FB$)) THEN _
         GOTO 10540
      A$ = TIME$ + " " + DATE$ + ": " + _
           FIRST$ + " " + LAST$ + _
           " ---> Illegal attempt at editor  <---"
      GOSUB 2900
      GOSUB 80
      GOTO 60
10540 A$ = "Good guess " + FIRST$ + "!"
      GOSUB 80
11000 A$ = ""
      GOSUB 80
      A$ = "Here's a list of the messages to TW sysop:"
      GOSUB 80
      GOSUB 2700
      A$ = ""
      GOSUB 80
      A$ = "Delete this list (Y/N)[N]? "
      GOSUB 100
      GOSUB 400
      IF A$ = "Y" OR A$ = "y" THEN
         CLOSE 1
         O.PENFILE$ = "TWMESG.DAT"
         OPEN"O",1,O.PENFILE$
         CLOSE 1
		 O.PENFILE$ = ""
         GOSUB 2000
      END IF
      A$ = TIME$ + " " + DATE$ + ": " + _
           FIRST$ + " " + LAST$ + "  *** Editor entered ***"
      GOSUB  2900
      GOTO 20000
18000 ' Update port and load values into the arrays
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
      GOSUB 1000
      C = D
      A$ = TIME$
      M = VAL(LEFT$(A$,2))*60 + VAL(MID$(A$,4,2))
      D = D - CVI(FC$) + (M - CVI(FQ$))/1440
      IF D < 0 THEN
         GOTO 18060
       ELSEIF D > 10 THEN
          D = 10
      END IF
      FOR L = 1 TO 3
        N(L) = N(L) + P(L)*D
        IF N(L) > P(L)*10 THEN _
           N(L)=P(L)*10
      NEXT
      IF P2 = LS + 1 THEN
         M(1) = 0
         M(2) = 0
         M(3) = 0
         RETURN
      END IF
18060 FOR L = 1 TO 3
        M(L) = INT(B(L)*(1 - C(L)*N(L)/P(L)/1000) + .5)
      NEXT
      GET 1,P2
      LSET FC$ = MKI$(C)
      LSET FD$ = MKI$(INT(N(1)))
      LSET FE$ = MKI$(INT(N(2)))
      LSET FF$ = MKI$(INT(N(3)))
      FOR L = 1 TO 3
        S(L,0) = INT((N(L)-INT(N(L)))*10000 + .5)
        N(L) = INT(N(L))
      NEXT
      LSET FR$ = MKI$(S(1,0))
      LSET FO$ = MKI$(S(2,0))
      LSET FP$ = MKI$(S(3,0))
      LSET FQ$ = MKI$(M)
      PUT 1,P2
      RETURN
19000 REM **/ Get P, a user number from A$, a given an name or number.  P=0 = None /**
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
           19050
        A$ = LEFT$(FA$,CVI(FM$)) + " (Y/N)[Y]? "
        GOSUB 100
        GOSUB 400
        IF A$ = "" OR A$ = "Y" OR A$ = "y" THEN
           P = L
           RETURN
        END IF
19050 NEXT L
      A$ = "Not found."
      GOSUB 80
      P = 0
      RETURN
19500 REM **/ Get P, a port number from A$, a given an name or number.  P=0 = None /**
      B$ = A$
      IF A$ = "" THEN
         P = 0
         RETURN
      END IF
      IF VAL(A$) <> 0 THEN
         P = VAL(A$)
         RETURN
      END IF
      FOR L = LS + 1 TO LT
        GET 1,L
        IF INSTR(LEFT$(FA$,CVI(FM$)),B$) = 0 THEN _
           19550
        A$ = LEFT$(FA$,CVI(FM$)) + " (Y/N)[Y]? "
        GOSUB 100
        GOSUB 400
        IF A$ = "" OR A$ = "Y" OR A$ = "y" THEN
           P = L - LS
           RETURN
        END IF
19550 NEXT L
      A$ = "Not found."
      GOSUB 80
      P = 0
      RETURN
20000 B(1) = 10
      B(2) = 20
      B(3) = 35
      A$ = ""
      GOSUB 80
      A$ = "TWEDIT Main functions (?=help)"
	  GOSUB 80
	  A = 4
	  GOSUB 300
	  IF V = 1 THEN _
		 PR$ = "Well? "
	  IF V = 2 THEN _
		 PR$ = "What now! "
	  IF V = 3 THEN _
		 PR$ = FIRST$ + " say? "
	  IF V = 4 THEN _
		 PR$ = "Speak: "
	  A$ = PR$
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN
         A$ = "(? = Help)"
         GOSUB 80
         GOTO 20000
      END IF
	  A = ASC(LEFT$(A$,1)) AND 223
	  IF A = 65 THEN
		 A$ = "<Team Editor>"
		 GOSUB 80
		 GOTO 33000
	  END IF
      IF A = 67 THEN
         A$ = "<Cabal editor>"
         GOSUB 80
         GOTO 31110
      END IF
      IF A = 68 THEN
         A$ = "<Daily message editor>"
         GOSUB 80
         GOTO 30340
      END IF
      IF A = 71 THEN
         A$ = "<General info>"
         GOSUB 80
         GOTO 27000
      END IF
      IF A = 76 THEN
         A$ = "<Planet editor>"
         GOSUB 80
         GOTO 30670
	  END IF
	  IF A = 77 THEN
		 A$ = "<Radio Message Editor>"
		 GOSUB 80
		 GOTO 32000
	  END IF
      IF A = 80 THEN
         A$ = "<Port Editor>"
         GOSUB 80
         GOTO 25000
      END IF
      IF A = 82 THEN
         A$ = "<Report>"
         GOSUB 80
         GOTO 29000
      END IF
      IF A = 83 THEN
         A$ = "<Sector Editor>"
         GOSUB 80
         GOTO 23000
      END IF
      IF A = 84 THEN
         A$ = "<Tradewars>"
         GOSUB 80
         GOSUB 2300
         GOTO 60
      END IF
      IF A = 85 THEN
         A$ = "<User Editor>"
         GOSUB 80
         GOTO 21000
	  END IF
	  IF (A = 88 AND L.OCAL = 1) THEN
		 A$ = "<Exit to DOS>"
		 GOSUB 80
		 CLOSE
		 END
	  END IF
      IF A = 90 THEN
         A$ = "<Instructions>"
         GOSUB 80
         GOSUB 28000
         GOTO 20000
      END IF
      IF A <> 31 THEN
         A$ = "Invalid command.  Enter ? for Help"
         GOSUB 80
         GOTO 20000
	  END IF
	  A$ = ""
	  GOSUB 80
	  A$ = "               A - Team Editor    "
      GOSUB 80
	  A$ = "C - Cabal editor         D - Daily message editor"
	  GOSUB 80
	  A$ = "G - edit General info    L - Planet editor"
	  GOSUB 80
	  A$ = "M - radio Message editor P - Port editor"
	  GOSUB 80
	  A$ = "R - Report on players    S - Sector editor"
	  GOSUB 80
	  A$ = "T - Return to Tradewars  U - User editor"
	  GOSUB 80
	  A$ = "               Z - Instructions"
	  GOSUB 80
	  IF L.OCAL = 1 THEN
		 A$ = "               X - eXit to DOS"
		 GOSUB 80
	  END IF
      A$ = ""
      GOSUB 80
      GOTO 20000
21000 ' User Editor
      A$ = ""
      GOSUB 80
      A$ = "User (?=help)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "?" THEN
		 A$ = "Enter a user number or a search string."
		 GOSUB 80
		 A$ = "Note that upper case is DIFFERENT than lower case in the search string."
		 GOSUB 80
		 A$ = "Press ENTER to exit the user editor."
         GOSUB 80
         GOTO 21000
      END IF
      GOSUB 19000
      PN = P
      IF PN = 0 THEN _
         20000
      IF PN < 2 OR PN > LP THEN
         A$ = "Invalid player number."
         GOSUB 80
         GOTO 21000
      END IF
21100 A$ = ""
      GOSUB 80
      GET 1,PN
      A$ = "<A> Name: "
      GOSUB 100
      IF CVI(FM$) = 0 THEN
         A$ = ""
         GOSUB 80
         A$ = "<Player record not used>"
         GOSUB 80
	   ELSE _
		 A$ = " (#" + STR$(PN) + ")" + LEFT$(FA$,CVI(FM$))
		 GOSUB 80
	  END IF
      A$ = "<B> Last day on:"
      GOSUB 100
      GOSUB 1000
      E = CVI(FB$)
      D = D - E
      IF D = 0 THEN
         A$ = " today"
         GOSUB 80
       ELSEIF D > 0 THEN
         A$ = STR$(D) + " days ago"
         GOSUB 80
       ELSE A$ = " Will be allowed on in" + STR$(-D) + " days"
         GOSUB 80
      END IF
      A = CVI(FC$)
      A$ = "<C> Killed by: "
      GOSUB 100
      IF A = 0 THEN
         A$ = "<No one>"
         GOSUB 80
       ELSEIF A = -99 THEN
         A$ = "<To be initialized>"
         GOSUB 80
         A = 0
       ELSEIF  A = -98 THEN
         A$ = "<A person who has been deleted>"
         GOSUB 80
         A = 0
      END IF
      IF A <> 0 THEN
         IF A = -1 THEN
            A$ = "<Cabal>"
            GOSUB 80
          ELSEIF A < 2 OR A > LP THEN
            A$ = "Unknown value:" + STR$(A)
            GOSUB 80
          ELSE GET 1,A
            A$ = LEFT$(FA$,CVI(FM$)) + " (#" + STR$(A) + ")"
            GOSUB 80
            GET 1,PN
         END IF
	  END IF
	  GET 1,PN
      A$ = "<D> Turns left:" + STR$(CVI(FD$))
      GOSUB 80
      A$ = "<E> Location: sector" + STR$(CVI(FF$))
      GOSUB 80
      A$ = "<F> Fighters:" + STR$(CVI(FG$))
      GOSUB 80
      A$ = "<G> Total cargo holds: " + STR$(CVI(FH$))
      GOSUB 80
      A$ = "<H>    Ore:" + STR$(CVI(FI$))
      GOSUB 80
      A$ = "<I>    Org:" + STR$(CVI(FJ$))
      GOSUB 80
      A$ = "<J>    Eqp:" + STR$(CVI(FK$))
      GOSUB 80
      A$ = "<K> Credits: " + STR$(CVI(FL$))
      GOSUB 80
      A$ = "<L> Last sector in:" + STR$(CVI(FQ$))
	  GOSUB 80
	  A$ = "<M> Belongs to a Team - 0=no, 1=yes  -> " + STR$(CVI(FE$))
	  GOSUB 80
	  IF CVI(FE$) = 1 THEN
		 A$ = "<N> Team [" + RIGHT$(STR$(CVI(FR$)),LEN(STR$(CVI(FR$)))-1) + "]"
		 GOSUB 80
	  END IF
      A$ = "<!> Delete player"
      GOSUB 80
      A$ = "[ENTER] - Edit another player/return to main menu"
      GOSUB 80
21300 A$ = ""
      GOSUB 80
      A$ = "Command (?=List)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         21000
      IF A$ = "?" THEN _
         21100
      A = ASC(LEFT$(A$,1)) AND 223
      IF A <> 65 THEN _
         21350                                        ' Name:
21330 A$ = ""
      GOSUB 80
      A$ = "New name? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN
         GOTO 21300
       ELSEIF LEN(A$) > 41 THEN
         A$ = "The name is limited to 41 characters."
         GOSUB 80
         GOTO 21330
      END IF
      LSET FA$ = A$
      LSET FM$ = MKI$(LEN(A$))
      PUT 1,PN
      GOTO 21300
21350 IF A <> 66 THEN _
         21390                                        ' Last day on:
21360 A$ = "Last day on (?=Help)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         21300
      IF A$ = "?" THEN
         A$ = "Enter how many days ago they were on.  Ex: 1 means they were on yesterday.  0 means they were on today.  -3 means they won't be allowed on for another 3 days."
         GOSUB 80
         GOTO 21360
      END IF
      GOSUB 1000
      LSET FB$ = MKI$(D - VAL(A$))
      PUT 1,PN
      GOTO 21300
21390 IF A <> 67 THEN _
         21440                                        ' Killed by:
21400 A$ = "Who killed him (?=Help)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         21300
      IF A$ = "?" THEN
         A$ = "Enter the person's number or a search string.  Enter 0 for still alive, a -1 for cabal, a -98 for killed by an person who'se been deleted, a -99 to be initialized."
         GOSUB 80
         GOTO 21400
      END IF
      IF A$ = "0" THEN
         P2 = 0
       ELSE GOSUB 19000
         P2 = P
         IF P2 = 0 THEN _
            GOTO 21400
      END IF
      LSET FC$ = MKI$(P2)
      PUT 1,PN
      IF P2 <> 0 THEN
         P = PN
         GOSUB 7100
         GET 1,PN
         LSET FF$ = MKI$(0)
         PUT 1,PN
         GOTO 21300
       ELSE GOTO 21300
      END IF
21440 IF A = 68 THEN
         A$ = "Turns left? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN
            GOTO 21300
          ELSE LSET FD$ = MKI$(VAL(A$))
            PUT 1,PN
            GOTO 21300
         END IF
      END IF
      IF A = 69 THEN
         A$ = "Location? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN
            GOTO 21300
          ELSE P = PN
            GOSUB 7100
            GET 1,PN
            LSET FF$ = MKI$(VAL(A$))
            PUT 1,PN
            P = PN
            GOSUB 7000
            GOTO 21300
         END IF
      END IF
      IF A <> 70 THEN _
         21490                                         ' Fighters:
      A$ = "Fighters? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         21300
      LSET FG$ = MKI$(VAL(A$))
      PUT 1,PN
      IF VAL(A$) > 9999 THEN
         A$ = "*** Warning ***  There is a 9999 fighter limit to this game, and having more can lead to problems."
         GOSUB 80
         GOTO 21300
       ELSE GOTO 21300
      END IF
21490 IF A <> 71 THEN _
         21530                                         ' Cargo Holds:
      A$ = "Cargo holds? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         21300
      LSET FH$ = MKI$(VAL(A$))
      PUT 1,PN
      IF CVI(FH$) > 50 THEN
         A$ = "*** Warning ***  There is a 50 cargo hold limit to this game, and having more can lead to problems."
         GOSUB 80
      END IF
21520 IF CVI(FI$) + CVI(FJ$) + CVI(FK$) > CVI(FH$) THEN
         A$ = "*** Warning *** Amount of cargo in holds is greater than to total cargo holds."
         GOSUB 80
         GOTO 21300
       ELSE GOTO 21300
      END IF
21530 IF A = 72 THEN
         A$ = ""
         GOSUB 80
         A$ = "Ore? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN
            GOTO 21300
          ELSE LSET FI$ = MKI$(VAL(A$))
            PUT 1,PN
            GOTO 21520
         END IF
      END IF
      IF A = 73 THEN
         A$ = ""
         GOSUB 80
         A$ = "Organics? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN
            GOTO 21300
          ELSE LSET FJ$ = MKI$(VAL(A$))
            PUT 1,PN
            GOTO 21520
         END IF
      END IF
      IF A = 74 THEN
         A$ = ""
         GOSUB 80
         A$ = "Equipment? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN
            GOTO 21300
          ELSE LSET FK$ = MKI$(VAL(A$))
            PUT 1,PN
            GOTO 21520
         END IF
      END IF
      IF A = 75 THEN
         A$ = ""
         GOSUB 80
         A$ = "Credits? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN
            GOTO 21300
          ELSE LSET FL$ = MKI$(VAL(A$))
            PUT 1,PN
            GOTO 21300
         END IF
      END IF
      IF A = 76 THEN
         A$ = ""
         GOSUB 80
         A$ = "Last sector in? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN
            GOTO 21300
          ELSE FQ$ = MKI$(VAL(A$))
            PUT 1,PN
            GOTO 21520
         END IF
	  END IF
	  IF A = 77 THEN
		 A$ = "Should Player belong to a Team - 0=no, 1=yes? "
		 GOSUB 100
		 GOSUB 400
		 A = VAL(A$)
		 IF A >= 0 AND A < 2 THEN
			LSET FE$ = MKI$(A)
			PUT 1,PN
		 END IF
		 GOTO 21300
	  END IF
	  IF A = 78 THEN
		 A$ = "Enter Team number for this player? "
		 GOSUB 100
		 GOSUB 400
		 GET 1,1
		 HTN = CVI(FT$)
		 GET 1,PN
		 A = VAL(A$)
		 IF A >= 0 AND A <= HTN THEN
			LSET FR$ = MKI$(A)
			PUT 1,PN
		 END IF
		 GOTO 21300
	  END IF
      IF A$ <> "!" THEN _
         21630
      GET 1,PN
      P = PN
      A$ = "Delete " + LEFT$(FA$,CVI(FM$)) + " (Y/N)[N]? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" OR A$ = "N" OR A$ = "n" THEN _
         21300
      GOSUB 8000
      GOTO 21300
21630 A$ = "Invalid command.  Enter ? for help."
      GOSUB 80
      GOTO 21300
23000 REM **/ Sector Editor /**
      A$ = ""
      GOSUB 80
      A$ = "Sector number ([ENTER] quits)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         20000
      S2 = VAL(A$) + LP
      IF S2 < LP + 1 OR LP > LS THEN
         A$ = "Invalid sector number."
         GOSUB 80
         GOTO 23000
      END IF
23100 A$ = ""
      GOSUB 80
      GET 1,S2
      A$ = "Sector:" + STR$(S2 - LP)
      GOSUB 80
      A$ = "    Warps lead to:"
      GOSUB 80
      A$ = "<A>  " + STR$(CVI(FB$))
      GOSUB 80
      A$ = "<B>  " + STR$(CVI(FC$))
      GOSUB 80
      A$ = "<C>  " + STR$(CVI(FD$))
      GOSUB 80
      A$ = "<D>  " + STR$(CVI(FE$))
      GOSUB 80
      A$ = "<E>  " + STR$(CVI(FF$))
      GOSUB 80
      A$ = "<F>  " + STR$(CVI(FG$))
      GOSUB 80
      A$ = "<G> Port in sector: "
      GOSUB 100
      A = CVI(FH$)
      IF A = 0 THEN
         A$ = "None"
         GOSUB 80
       ELSE GET 1,A + LS
         A$ = "#" + STR$(A) + " (" + LEFT$(FA$,CVI(FM$)) + ")"
         GOSUB 80
         GET 1,S2
      END IF
      A$ = "<H> Fighters in sector:" + STR$(CVI(FL$))
      GOSUB 80
      A$ = "       (Belong to: "
      GOSUB 100
      A = CVI(FM$)
      IF A = 0 THEN
         A$ = " <No one>)"
         GOSUB 80
       ELSEIF A < 0 THEN
         A$ = "the Cabal)"
         GOSUB 80
         A = 0
       ELSEIF A > LP THEN
         A$ = "Invalid player number: " + STR$(A) + ")"
         GOSUB 80
         A = 0
      END IF
      IF A <> 0 THEN
         GET 1,A
         A$ = LEFT$(FA$,CVI(FM$)) + " (#" + STR$(A) + "))"
         GOSUB 80
         GET 1,S2
      END IF
      A$ = "    People in sector:"
      GOSUB 80
      A = CVI(FI$)
      IF A = 0 THEN
         A$ = "        None"
         GOSUB 80
         GOTO 23220
      END IF
23210 GET 1,A
      B = CVI(FP$)
      GET 1,B
      A$ = "      " + LEFT$(FA$,CVI(FM$)) + "(#" + STR$(B) + ")"
      GOSUB 80
      GET 1,A
      A = CVI(FO$)
      IF A = 0 THEN
         GET 1,S2
       ELSE GOTO 23210
      END IF
23220 GET 1,S2
      IF CVI(FO$) <> 0 THEN
         GET 1,CVI(FO$) + LT
         A$ = "    Planet in sector: " + LEFT$(FA$,CVI(FM$))
         GOSUB 80
         GET 1,S2
      END IF
      A$ = "<I> Display shortest route between two sectors"
	  GOSUB 80
      A$ = ""
      GOSUB 80
      A$ = "[ENTER] - Get another sector/Return to main menu"
      GOSUB 80
23300 A$ = ""
      GOSUB 80
      A$ = "Command (?=List)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN
         GOTO 23000
       ELSEIF A$ = "?" THEN
         GOTO 23100
      END IF
      A = ASC(LEFT$(A$,1)) AND 223
      IF A > 64 AND A < 71 THEN
         A$ = ""
         GOSUB 80
         A$ = "*** Warning ***  I don't recommend changing this value"
         GOSUB 80
      END IF
      IF A = 65 THEN
         A$ = "Leads to what sector? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN
            GOTO 23300
          ELSE LSET FB$ = MKI$(VAL(A$))
            PUT 1,S2
            GOTO 23300
         END IF
      END IF
      IF A = 66 THEN
         A$ = "Leads to what sector? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN
            GOTO 23300
          ELSE LSET FC$ = MKI$(VAL(A$))
            PUT 1,S2
            GOTO 23300
         END IF
      END IF
      IF A = 67 THEN
         A$ = "Leads to what sector? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN
            GOTO 23300
          ELSE LSET FD$ = MKI$(VAL(A$))
            PUT 1,S2
            GOTO 23300
         END IF
      END IF
      IF A = 68 THEN
         A$ = "Leads to what sector? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN
            GOTO 23300
          ELSE LSET FE$ = MKI$(VAL(A$))
            PUT 1,S2
            GOTO 23300
         END IF
      END IF
      IF A = 69 THEN
         A$ = "Leads to what sector? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN
            GOTO 23300
          ELSE LSET FF$ = MKI$(VAL(A$))
            PUT 1,S2
            GOTO 23300
         END IF
      END IF
      IF A = 70 THEN
         A$ = "Leads to what sector? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN
            GOTO 23300
          ELSE LSET FG$ = MKI$(VAL(A$))
            PUT 1,S2
            GOTO 23300
         END IF
      END IF
      IF A <> 72 THEN _
         23450                                     ' Fighters in sector
23390 A$ = "Number of fighters in the sector? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" AND CVI(FL$) = 0 THEN
         GET 1,S2
         GOTO 23300
       ELSE A = VAL(A$)
         IF A < 0 THEN
            A$ = "Invalid value"
            GOSUB 80
            GOTO 23300
         END IF
      END IF
      IF A$ = "" THEN _
         A = CVI(FL$)
      IF A > 9999 THEN
         A$ = ""
         GOSUB 80
         A$ = "*** Warning ***  There is a maximum value of 9999 fighters allowed in a sector"
         GOSUB 80
      END IF
      LSET FL$ = MKI$(A)
      PUT 1,S2
      IF A = 0 THEN
         LSET FM$ = MKI$(0)
         PUT 1,S2
         GOTO 23300
      END IF
      A$ = "Who do they belong to?"
      GOSUB 80
23420 A$ = "Enter a search string or number (?=help)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN
         IF CVI(FM$) <> 0 THEN
            GOTO 23300
          ELSE GOTO 23390
         END IF
      END IF
      IF A$ = "?" THEN
         A$ = "Enter search string or player number.  -1 = the Cabal.  [ENTER] = No one"
         GOSUB 80
         GOTO 23420
      END IF
      GOSUB 19000
      GET 1,S2
      IF P < -1 OR P = 0 OR P = 1 OR P > LP THEN
         A$ = "Invalid player number"
         GOSUB 80
         GOTO 23420
      END IF
      LSET FM$ = MKI$(P)
      PUT 1,S2
      GOTO 23300
23450 IF A <> 71 THEN _
         23490
23460 A$ = "*** Warning ***  I don't recommend changing this"
      GOSUB 80
      A$ = "Enter search string or port number? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         23300
      GOSUB 19500
      GET 1,S2
      IF P < 1 OR P > LT - LS THEN
         A$ = "Invalid port number"
         GOSUB 80
         GOTO 23460
      END IF
      LSET FH$ = MKI$(P)
      PUT 1,S2
      GOTO 23300
23490 IF A <> 73 THEN _
		 23590
23500 A$ = "From what sector? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 23300
      A = VAL(A$)
      IF A < 1 OR A > LS - LP THEN
         A$ = "Invalid sector number."
         GOSUB 80
         GOTO 23500
      END IF
23510 A$ = "To what sector? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 23300
      B = VAL(A$)
      IF B < 1 OR B > LS - LP THEN
         A$ = "Invalid sector number."
         GOSUB 80
         GOTO 23510
      END IF
	  A$ = "Computing shortest path."
	  GOSUB 100
      GOSUB 7500
      IF S(A,1) = 0 THEN
         A$ = "There was an error in calculations!"
         GOSUB 80
         GOTO 23500
      END IF
      A$ = "The shortest path between sector" + _
           STR$(A) + " and" + STR$(B) + " is:"
      GOSUB 80
      A$ = STR$(A)
      GOSUB 100
      S = A
23550 S = S(S,1)
      IF S = 0 THEN
         A$ = ""
         GOSUB 80
         GOTO 23300
       ELSE A$ = " -" + STR$(S)
         GOSUB 100
         GOTO 23550
	  END IF
23590 A$ = "Invalid command.  Enter ? for help"
      GOSUB 80
      GOTO 23300
25000 REM **/ Port editor /**
      A$ = "Enter port number or search string ([ENTER] quits)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         20000
      GOSUB 19500
      P2 = P + LS
      IF P2 < LS + 1 OR P2 > LT THEN
         A$ = "Invalid port number"
         GOSUB 80
         GOTO 25000
      END IF
25100 GET 1,P2
      GOSUB 18000
      A$ = ""
      GOSUB 80
      A$ = "<A> Name: " + LEFT$(FA$,CVI(FM$)) + " (#" + STR$(P2 - LS) + ")"
      GOSUB 80
      A$ = "<B> Class:" + STR$(CVI(FB$))
      GOSUB 80
      A$ = "<C> Ore:" + STR$(N(1)) + "  (Price =" + STR$(M(1)) + ")"
      GOSUB 100
      IF C(1) > 0 THEN
         A$ = "  <-- Selling"
         GOSUB 80
       ELSE A$ = ""
         GOSUB 80
      END IF
      A$ = "<D> Org:" + STR$(N(2)) + "  (Price =" + STR$(M(2)) + ")"
      GOSUB 100
      IF C(2) > 0 THEN
         A$ = "  <-- Selling"
         GOSUB 80
       ELSE A$ = ""
         GOSUB 80
      END IF
      A$ = "<E> Eqp:" + STR$(N(3)) + "  (Price =" + STR$(M(3)) + ")"
      GOSUB 100
      IF C(3) > 0 THEN
         A$ = "  <-- Selling"
         GOSUB 80
       ELSE A$ = ""
         GOSUB 80
      END IF
      A$ = "<F> Productivity (units/day)"
      GOSUB 80
      A$ = "      Ore:" + STR$(P(1)) + "   Org:" + STR$(P(2)) + _
           "   Eqp:" + STR$(P(3))
      GOSUB 80
      A$ = "<G> Max change in cost (percent)"
      GOSUB 80
      A$ = "      Ore:" + STR$(C(1)) + "   Org:" + STR$(C(2)) + _
           "   Eqp:" + STR$(C(3))
      GOSUB 80
      A$ = "[ENTER] - Edit another port/Return to main menu"
      GOSUB 80
25300 A$ = ""
      GOSUB 80
      A$ = "Command (?=help)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 25000
      IF A$ = "?" THEN _
         GOTO 25100
      A = ASC(LEFT$(A$,1)) AND 223
25320 IF A = 65 THEN
         A$ = "New name? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN
            GOTO 25300
          ELSEIF LEN(A$) > 41 THEN
            A$ = "Too long.  41 Chars. max"
            GOSUB 80
            GOTO 25320
          ELSE LSET FA$ = A$
            LSET FM$ = MKI$(LEN(A$))
            PUT 1,P2
            GOTO 25300
         END IF
      END IF
      IF A = 66 THEN
         A$ = "New class? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN
            GOTO 25300
          ELSE LSET FB$ = MKI$(VAL(A$))
            PUT 1,P2
            GOTO 25300
         END IF
      END IF
      IF A <> 67 THEN _
         25370
      A$ = "Enter amount of ore? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN
         GOTO 25300
       ELSE A = VAL(A$)
         LSET FD$ = MKI$(A)
         PUT 1,P2
      END IF
      IF A < 0 OR A > P(1)*10 THEN
         A$ = ""
         GOSUB 80
         A$ = "*** Warning ***  The range is between 0 and" + STR$(P(1)*10)
         GOSUB 80
         GOTO 25300
       ELSE GOTO 25300
      END IF
25370 IF A <> 68 THEN _
         25400
      A$ = "Enter amount of organics? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 25300
      A = VAL(A$)
      LSET FE$ = MKI$(A)
      PUT 1,P2
      IF A < 0 OR A > P(2)*10 THEN
         A$ = ""
         GOSUB 80
         A$ = "*** Warning ***  The range is between 0 and" + STR$(P(2)*10)
         GOSUB 80
         GOTO 25300
       ELSE GOTO 25300
      END IF
25400 IF A <> 69 THEN _
         25430
      A$ = "Enter amount of equipment? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 25300
      A = VAL(A$)
      LSET FF$ = MKI$(A)
      PUT 1,P2
      IF A < 0 OR A > P(3)*10 THEN
         A$ = ""
         GOSUB 80
         A$ = "*** Warning ***  The range is between 0 and" + STR$(P(3)*10)
         GOSUB 80
         GOTO 25300
       ELSE GOTO 25300
      END IF
25430 IF A <> 70 THEN _
         25490
      A$ = ""
      GOSUB 80
      A$ = "*** Warning ***  I don't recommend changing these values"
      GOSUB 80
      A$ = "Productivity in units/day"
      GOSUB 80
      A$ = "  Ore? "
      GOSUB 100
      GOSUB 400
	  A = VAL(A$)
	  IF A <> 0 THEN
		 LSET FG$ = MKI$(A)
         PUT 1,P2
      END IF
      A$ = "  Organics? "
      GOSUB 100
	  GOSUB 400
	  A = VAL(A$)
	  IF A <> 0 THEN
		 LSET FH$ = MKI$(A)
         PUT 1,P2
      END IF
      A$ = "  Equipment? "
      GOSUB 100
	  GOSUB 400
	  A = VAL(A$)
	  IF A <> 0 THEN
		 LSET FI$ = MKI$(A)
         PUT 1,P2
      END IF
      GOTO 25300
25490 IF A <> 71 THEN _
         GOTO 25550
      A$ = ""
      GOSUB 80
      A$ = "***  Warning ***  I don't recommend changing these values"
      GOSUB 80
      A$ = "Max change in cost:"
      GOSUB 80
      A$ = "  Ore? "
      GOSUB 100
	  GOSUB 400
	  A = VAL(A$)
	  IF A <> 0 THEN
		 LSET FJ$ = MKI$(A)
         PUT 1,P2
      END IF
      A$ = "  Organics? "
      GOSUB 100
	  GOSUB 400
	  A = VAL(A$)
	  IF A <> 0 THEN
		 LSET FK$ = MKI$(A)
         PUT 1,P2
      END IF
      A$ = "  Equipment? "
      GOSUB 100
	  GOSUB 400
	  A = VAL(A$)
	  IF A <> 0 THEN
		 LSET FL$ = MKI$(A)
         PUT 1,P2
      END IF
      GOTO 25300
25550 A$ = "Invalid command.  Enter ? for help"
      GOSUB 80
      GOTO 25300
25590 A$ = "Not implemented."
      GOSUB 80
      GOTO 25300
27000 REM **/ Edit general info /**
      GET 1,1
      A$ = ""
      GOSUB 80
      A$ = "<A> Change editor password"
      GOSUB 80
      A$ = "<B> Turns per day:" + STR$(CVI(FD$))
      GOSUB 80
      A$ = "<C> Initial fighters:" + STR$(CVI(FH$))
      GOSUB 80
      A$ = "<D> Initial credits:" + STR$(CVI(FI$))
      GOSUB 80
      A$ = "<E> Initial cargo holds:" + STR$(CVI(FJ$))
      GOSUB 80
      A$ = "<F> Days until an inactive player is deleted:" + STR$(CVI(FK$))
      GOSUB 80
      A$ = "<G> Last day maintence run:"
      GOSUB 100
      GOSUB 1000
      A = CVI(FL$)
      IF D = A THEN
         A$ = " Today"
         GOSUB 80
       ELSEIF D - 1 = A THEN
         A$ = " Yesterday"
         GOSUB 80
       ELSE A$ = STR$(D - A) + " days ago"
         GOSUB 80
      END IF
      A$ = "    Maximum number of players:" + STR$(LP - 1)
      GOSUB 80
      A$ = "    Number of sectors:" + STR$(LS - LP)
      GOSUB 80
      A$ = "    Number of ports:" + STR$(LT - LS)
      GOSUB 80
      A$ = "<H> Cabal regeneration:" + STR$(CVI(FR$)) + " ftrs/day"
      GOSUB 80
      A$ = "<I> Messages to TW sysop: "
      GOSUB 100
      IF CVI(FP$) = 1 THEN
         A$ = "Off"
         GOSUB 80
       ELSE A$ = "On"
         GOSUB 80
      END IF
27300 A$ = ""
      GOSUB 80
      A$ = "Command (?=help)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN
         GOTO 20000
       ELSEIF A$ = "?" THEN
         GOTO 27000
      END IF
      A = ASC(A$) AND 223
      IF A <> 65 THEN _
         GOTO 27360
      A$ = ""
      GOSUB 80
      A$ = "For security, please enter the current editor password: "
      GOSUB 100
      GOSUB 400
      IF A$ <> LEFT$(FA$,CVI(FB$)) THEN
         A$ = "Invalid."
         GOSUB 80
         GOTO 27300
      END IF
27340 A$ = "Noting that UPPER case is different than lower case, what is the new password?  ? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN
         GOTO 27300
       ELSEIF LEN(FA$) > 41 THEN
         A$ = "You are limited to 41 characters."
         GOSUB 80
         GOTO 27340
      END IF
      LSET FA$ = A$
      LSET FB$ = MKI$(LEN(A$))
      PUT 1,1
      GOTO 27300
27360 IF A = 66 THEN
         A$ = "Turns/day? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN _
            GOTO 27300
         A = VAL(A$)
         LSET FD$ = MKI$(A)
         PUT 1,1
         IF A < 1 THEN
            A$ = ""
            GOSUB 80
            A$ = "*** Warning ***  Must be > 0"
            GOSUB 80
            GOTO 27300
          ELSE GOTO 27300
         END IF
      END IF
      IF A = 67 THEN
         A$ = "# of ftrs? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN _
            GOTO 27300
         A = VAL(A$)
         LSET FH$ = MKI$(A)
         PUT 1,1
         IF A < 0 OR A > 9999 THEN
            A$ = ""
            GOSUB 80
            A$ = "*** Warning ***  Range  = 0..9999"
            GOSUB 80
            GOTO 27300
          ELSE GOTO 27300
         END IF
      END IF
      IF A = 68 THEN
         A$ = "# of credits? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN _
            GOTO 27300
         A = VAL(A$)
         LSET FI$ = MKI$(A)
         PUT 1,1
         IF A < 0 THEN
            A$ = ""
            GOSUB 80
            A$ = "*** Warning ***  Must be > 0"
            GOSUB 80
            GOTO 27300
          ELSE GOTO 27300
         END IF
      END IF
      IF A = 69 THEN
         A$ = "# of holds? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN _
            GOTO 27300
         A = VAL(A$)
         LSET FJ$ = MKI$(A)
         PUT 1,1
         IF A < 0 OR A > 50 THEN
            A$ = ""
            GOSUB 80
            A$ = "*** Warning ***  Range = 0..50"
            GOSUB 80
            GOTO 27300
          ELSE GOTO 27300
         END IF
      END IF
      IF A = 70 THEN
         A$ = "Days until deleted? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN _
            GOTO 27300
         A = VAL(A$)
         LSET FK$ = MKI$(A)
         PUT 1,1
         IF A < 1 THEN
            A$ = ""
            GOSUB 80
            A$ = "*** Warning ***  Must be > 0"
            GOSUB 80
            GOTO 27300
          ELSE GOTO 27300
         END IF
      END IF
      IF A <> 71 THEN _
         27440
27420 A$ = "Last day maintence run (?=help)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "?" THEN
         A$ = "Enter how many days ago it was run. 0=Today, 1=Yesterday, -4=won't be run for"
         GOSUB 80
         A$ = "another 4 days."
         GOSUB 80
         GOTO 27420
      END IF
      IF A$ = "" THEN _
         GOTO 27300
      A = VAL(A$)
      GOSUB 1000
      LSET FL$ = MKI$(D - A)
      PUT 1,1
      GOTO 27300
27440 IF A <> 72 THEN _
         27470
27450 A$ = ""
      GOSUB 80
      A$ = "Cabal regeneration? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 27300
      A = VAL(A$)
      IF A < 0 THEN
         A$ = "Must be positive!"
         GOSUB 80
         GOTO 27450
      END IF
      LSET FR$ = MKI$(A)
      PUT 1,1
      GOTO 27300
27470 IF A = 73 THEN
         A$ = ""
         GOSUB 80
         A$ = "Enter a 1 to turn them off, a 0 to turn them on: "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN _
            GOTO 27300
         LSET FP$ = MKI$(VAL(A$))
         PUT 1,1
         GOTO 27300
      END IF
      A$ = ""
      GOSUB 80
      A$ = "Invalid command.  Enter ? for help"
      GOSUB 80
      GOTO 27300
28000 REM **/ Instructions /**
      A$ = "Do you want TWEDIT instructions (Y/N) [N]? "
      GOSUB 100
      GOSUB 400
      IF A$ = "N" OR A$ = "n" OR A$ = "" THEN _
         RETURN
      GOSUB 4000
      RETURN
29000 REM **/ Player and sector report /**
      A$ = "Do you wish to print this report? "
      GOSUB 100
      GOSUB 400
      IF A$ = "Y" OR A$ = "y" THEN _
         P.RINT = 1
      A$ = "Ranking players..."
      GOSUB 80
      GOSUB 2500
      A$ = ""
      GOSUB 80
      A$ = ""
      GOSUB 80
      A$ = "Player/Sector report: " + DATE$ + " " + TIME$
      GOSUB 80
      A$ = ""
      GOSUB 80
      IF P = 0 THEN
         A$ = "No players."
         GOSUB 80
         GOTO 29120
      END IF
	  A$ = " #  Name                 Day  KB Trn  Loc  Crdts  Ftrs  CH Ore Org Eqp"
      GOSUB 80
	  A$ = "=== ==================== ===  == ===  ===  =====  ====  == === === ==="
      GOSUB 80
      GOSUB 1000
      D2 = D
      T2 = 0
29050 IF P.RINT = 1 THEN _
		 P.LINE = 0
      GET 1,P
      T2 = T2 + 1
      B$ = ""
      A = P
      L = 3
      GOSUB 30000
      A$ = LEFT$(FA$,CVI(FM$))
	  IF LEN(A$) < 20 THEN _
         A$ = A$ + SPACE$(20 - LEN(A$))
      IF LEN(A$) > 21 THEN _
         A$ = RIGHT$(A$,20)
      B$ = B$ + A$
      A = D2 - CVI(FB$)
      L = 28
      GOSUB 30000
      A = CVI(FC$)
      L = 32
      IF A <> 0 THEN _
         GOSUB 30000
      A = CVI(FD$)
      L = 36
      GOSUB 30000
      A = CVI(FF$)
	  L = 41
      GOSUB 30000
      A = CVI(FL$)
	  L = 48
      GOSUB 30000
      A = CVI(FG$)
	  L = 54
      GOSUB 30000
      A = CVI(FH$)
	  L = 58
      GOSUB 30000
      A = CVI(FI$)
	  L = 62
      GOSUB 30000
      A = CVI(FJ$)
	  L = 66
      GOSUB 30000
      A = CVI(FK$)
	  L = 70
	  GOSUB 30000
	  A$ = B$         'REM FIX 'B'
      GOSUB 80
      IF NOFSX$ = "Q" THEN _
         GOTO 20000
      P = CVI(FT$)
      IF P <> -1 THEN
         GOTO 29050
       ELSE A$ = "Number of players:" + STR$(T2)
         GOSUB 80
      END IF
29120 A$ = ""
      GOSUB 80
	  A$ = " Sr  Cl  Ftrs Who"
      GOSUB 80
	  A$ = " === ==  ==== ==="
      GOSUB 80
	  FOR S = LP + 1 TO LS
		IF P.RINT = 1 THEN _
		   P.LINE = 0
		GET 1,S
		IF CVI(FL$) = 0 THEN _
		   29180
		B$ = ""
		A = S - LP
		L = 3
		GOSUB 30000
		IF CVI(FH$) <> 0 THEN
		   GET 1,CVI(FH$) + LS
		   A = CVI(FB$)
		   GET 1,S
		   L = 6
		   GOSUB 30000
		END IF
		A = CVI(FL$)
		L = 13
		GOSUB 30000
		A = CVI(FM$)
		L = 17
		GOSUB 30000
		IF A < 0 THEN
		   A$ = B$ + "(Cabal)"
		   GOSUB 80
		 ELSE GET 1,A
		   A$ = B$ + "(" + LEFT$(FA$,CVI(FM$)) + ")"
		   GOSUB 80
		   GET 1,S
		END IF
29180   IF NOFSX$ = "Q" THEN
		   P.RINT = 0
		   GOTO 20000
		END IF
	  NEXT
29190 P.RINT = 0
      GOTO 20000
30000 REM **/ Used to print a table. /**
      A$ = STR$(A)
      IF LEN(B$) + LEN(A$) < L THEN _
         A$ = SPACE$(L - LEN(A$) - LEN(B$)) + A$
      A$ = A$ + " "
      B$ = B$ + A$
      RETURN
      GOTO 20000
30340 GOSUB 2300
      GOSUB 5800                                ' Daily message editor
30350 A$ = ""
      GOSUB 80
      A$ = "<A> List messages with numbers"
      GOSUB 80
      A$ = "<B> List messages with text"
      GOSUB 80
      A$ = "<C> Edit a message"
      GOSUB 80
      A$ = "<D> Enter a message"
      GOSUB 80
      A$ = "[ENTER] Exit message editor"
      GOSUB 80
30400 A$ = ""
      GOSUB 80
      A$ = "Message editor command (?=help)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN
         GOSUB 5900
         GOSUB 2000
         GOTO 20000
       ELSEIF A$ = "?" THEN
         GOTO 30350
       ELSE A = ASC(LEFT$(A$,1)) AND  223
      END IF
      IF A <> 65 THEN _
         30480
30430 GET 1,1
      M = CVI(FA$)
      A$ = "Starting number? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 30400
      A = VAL(A$)
      IF A < 2 OR A > M THEN
         A$ = "The range is 2 to" + STR$(M)
         GOSUB 80
         GOTO 30430
      END IF
      A$ = ""
      GOSUB 80
      A$ = ""
      GOSUB 80
      A$ = "Mesg #  Who to  Type  Var A  Var B"
      GOSUB 80
      A$ = "======  ======  ====  =====  ====="
      GOSUB 80
      FOR B = A TO M
      B$ = ""
      GET 1,B
      L = 4
      A = B
      GOSUB 30000
      A = CVI(FA$)
      L = 12
      GOSUB 30000
      L = 19
      A = CVI(FB$)
      GOSUB 30000
      A = CVI(FC$)
      L = 25
      GOSUB 30000
      A = CVI(FD$)
      L = 32
      GOSUB 30000
      A$ = ""
      GOSUB 80
      B$ = ""
      IF A$ = " " OR NOFSX$ = "Q" THEN
         GOTO 30340
	  END IF
	  NEXT
	  GOTO 30400
30480 IF A <> 66 THEN _
         30570
      GET 1,1
      M = CVI(FA$)
      A$ = "Starting number? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 30400
      A = VAL(A$)
      IF A < 2 OR A > M THEN
         A$ = "The range is 2 to" + STR$(M)
         GOSUB 80
         GOTO 30430
      END IF
      A$ = ""
      GOSUB 80
      A$ = ""
      GOSUB 80
      A$ = "Mesg # Message"
      GOSUB 80
      A$ = "====== ======="
      GOSUB 80
      FOR N = A TO M
        GET 1,N
        B$ = ""
        L = 4
        A = N
        GOSUB 30000
        A$ = "  "
        GOSUB 100
        A = CVI(FA$)
        B = CVI(FB$)
        C = CVI(FC$)
        D = CVI(FD$)
        IF B = 0 THEN
           A$ = "<Deleted>"
           GOSUB 80
           GOTO 30560
        END IF
        IF B <> 1 THEN
           A$ = "<Unknown message>"
           GOSUB 80
           GOTO 30560
        END IF
        GOSUB 5900
        GOSUB 2000
        GET 1,A
        A$ = "To " + LEFT$(FA$,CVI(FM$)) + ": "
        GOSUB 100
        IF C = -1 THEN
           A$ = "The Cabal"
         ELSEIF C = -98 THEN
           A$ = "A deleted player"
         ELSE GET 1,C
           A$ = LEFT$(FA$,CVI(FM$))
        END IF
        GOSUB 100
        A$ = " destroyed" + STR$(D) + " fighters."
        GOSUB 80
        GOSUB 2300
        GOSUB 5800
30560 NEXT
      GOTO 30400
30570 IF A <> 67 THEN _
         30640
30580 GET 1,1
      B = CVI(FA$)
      A$ = "Which number? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 30400
      M = VAL(A$)
      IF M < 2 OR M > B THEN
         A$ = "The range is 2 .." + STR$(B) + "."
         GOSUB 80
         GOTO 30580
      END IF
      A$ = "Enter <D> to delete: "
      GOSUB 100
      GOSUB 400
      IF A$ = "D" OR A$ = "d" THEN
         GET 1,M
         LSET FB$ = MKI$(0)
         PUT 1,M
         GOTO 30400
       ELSE GET 1,M
         LSET FB$ = MKI$(1)
         PUT 1,M
      END IF
30600 A$ = "Who is the message to? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 30400
      GOSUB 5900
      GOSUB 2000
      GOSUB 19000
      GOSUB 2300
      GOSUB 5800
      IF P = 0 THEN
         A$ = "Player not found."
         GOSUB 80
         GOTO 30600
       ELSE GET 1,M
         LSET FA$ = MKI$(P)
         PUT 1,M
      END IF
      A$ = "How many fighters destroyed? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 30400
      GET 1,M
      LSET FD$ = MKI$(VAL(A$))
      PUT 1,M
30620 A$ = "Who destroyed them? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 30400
      GOSUB 5900
      GOSUB 2000
      GOSUB 19000
      GOSUB 2300
      GOSUB 5800
      IF P = 0 THEN
         A$ = "Player not found."
         GOSUB 80
         GOTO 30620
      END IF
      GET 1,M
      LSET FC$ = MKI$(P)
      PUT 1,M
      GOTO 30400
30640 IF A <> 68 THEN
         A$ = ""
         GOSUB 80
         A$ = "Invalid command.  Enter ? for help"
         GOSUB 80
         GOTO 30400
      END IF
      GET 1,1
      A = CVI(FA$) + 1
      LSET FA$ = MKI$(A)
      LSET FB$ = MKI$(0)
      PUT 1,1
      GET 1,A
      LSET FA$ = MKI$(0)
      LSET FB$ = MKI$(0)
      LSET FC$ = MKI$(0)
      LSET FD$ = MKI$(0)
      PUT 1,A
      A$ = "New null message has been added at the end of the file."
      GOSUB 80
      GOTO 30400
30670                                                   ' Planet editor
30680 A$ = ""
      GOSUB 80
      A$ = "What planet (?=help)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "?" THEN
         A$ = "Enter planet number, search string, or '/'+sector numer the planets in (ie. /14 means the planet in sector 14).  Press [ENTER] to return to main menu."
         GOSUB 80
         GOTO 30680
      END IF
      IF A$ = "" THEN _
         GOTO 20000
      IF LEFT$(A$,1) <> "/" THEN _
         GOTO 30730
      A$ = RIGHT$(A$,LEN(A$) - 1)
      A = VAL(A$)
      IF A < 1 OR A > LS - LP THEN
         A$ = "Invalid sector number."
         GOSUB 80
         GOTO 30680
      END IF
      GET 1,A + LP
      IF CVI(FO$) = 0 THEN
         A$ = "There isn't a planet in that sector."
         GOSUB 80
         GOTO 30680
       ELSE L2 = CVI(FO$) + LT
         GOTO 30770
      END IF
30730 L = VAL(A$)
      IF L <> 0 THEN
         IF L < 1 OR L > LL - LT THEN
            A$ = "Invalid planet number."
            GOSUB 80
            GOTO 30680
          ELSE L2 = L + LT
            GOTO 30770
         END IF
      END IF
      B$ = A$
      FOR L = LT + 1 TO LT
        GET 1,L
        IF INSTR(LEFT$(FA$,CVI(FM$)),B$) = 0 THEN _
           30760
        A$ = LEFT$(FA$,CVI(FM$)) + " (Y/N)[Y]? "
        GOSUB 100
        GOSUB 400
        IF A$ = "" OR A$ = "Y" OR A$ = "y" THEN
           L2 = L
           RETURN
        END IF
30760 NEXT L
      A$ = "Not found."
      GOSUB 80
      P = 0
      GOTO 30680
30770 GOSUB 30990
      GET 1,L2
      A$ = ""
      GOSUB 80
      A$ = "Planet:"
      GOSUB 80
      IF CVI(FM$) = 0 THEN
         A$ = "    <Deleted planet>"
         GOSUB 80
       ELSE A$ = "<A> Name: " + LEFT$(FA$,CVI(FM$)) + _
                 " (#" + STR$(L2 - LT) + ")"
         GOSUB 80
      END IF
      A$ = "<B> Ore:" + STR$(CVI(FF$))
      GOSUB 80
      A$ = "<C> Org:" + STR$(CVI(FG$))
      GOSUB 80
      A$ = "<D> Eqp:" + STR$(CVI(FH$))
      GOSUB 80
      A$ = "    Productivity (units/day):"
      GOSUB 80
      A$ = "    <E> Ore:" + STR$(CVI(FC$)) + _
           "  <F> Org:" + STR$(CVI(FD$)) + _
           "  <G> Eqp:" + STR$(CVI(FE$))
      GOSUB 80
	  A$ = "<!> Delete/Create this planet"
	  GOSUB 80
      A$ = "[ENTER] = Edit another planet/Return to main menu"
      GOSUB 80
30820 A$ = ""
      GOSUB 80
      A$ = "Command (?=help)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         30680
      A = ASC(A$) AND 223
      IF A$ = "?" THEN _
         30770
30840 GET 1,L2
      IF A = 65 THEN
         A$ = "Name? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN _
            GOTO 30820
         IF LEN(A$) > 41 THEN
            A$ = "Too long: 41 chars max."
            GOSUB 80
            GOTO 30840
          ELSE LSET FA$ = A$
            LSET FM$ = MKI$(LEN(A$))
            PUT 1,L2
            GOTO 30820
         END IF
      END IF
      IF A = 66 THEN
         A$ = ""
         GOSUB 80
         A$ = "Ore? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN _
            GOTO 30820
         A = VAL(A$)
         LSET FF$ = MKI$(A)
         PUT 1,L2
         IF A < 0 THEN
            A$ = "*** Warning, should be positive ***"
            GOSUB 80
            GOTO 30820
          ELSE GOTO 30820
         END IF
      END IF
      IF A = 67 THEN
         A$ = ""
         GOSUB 80
         A$ = "Org? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN _
            GOTO 30820
         A = VAL(A$)
         LSET FG$ = MKI$(A)
         PUT 1,L2
         IF A < 0 THEN
            A$ = "*** Warning, should be positive ***"
            GOSUB 80
            GOTO 30820
          ELSE GOTO 30820
         END IF
      END IF
      IF A = 68 THEN
         A$ = ""
         GOSUB 80
         A$ = "Eqp? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN _
            GOTO 30820
         A = VAL(A$)
         LSET FH$ = MKI$(A)
         PUT 1,L2
         IF A < 0 THEN
            A$ = "*** Warning, should be positive ***"
            GOSUB 80
            GOTO 30820
          ELSE GOTO 30820
         END IF
      END IF
      IF A = 69 THEN
         A$ = "Prod. of Ore? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN _
            GOTO 30820
         A = VAL(A$)
         LSET FC$ = MKI$(A)
         PUT 1,L2
         IF A < 0 OR A > 3000 THEN
            A$ = "*** Warning, the range is 0..3000"
            GOSUB 80
            GOTO 30820
          ELSE GOTO 30820
         END IF
      END IF
      IF A = 70 THEN
         A$ = "Prod. of Org? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN _
            GOTO 30820
         A = VAL(A$)
         LSET FD$ = MKI$(A)
         PUT 1,L2
         IF A < 0 OR A > 3000 THEN
            A$ = "*** Warning, the range is 0..3000"
            GOSUB 80
            GOTO 30820
          ELSE GOTO 30820
         END IF
      END IF
      IF A = 71 THEN
         A$ = "Prod. of Eqp? "
         GOSUB 100
         GOSUB 400
         IF A$ = "" THEN _
            GOTO 30820
         A = VAL(A$)
         LSET FE$ = MKI$(A)
         PUT 1,L2
         IF A < 0 OR A > 3000 THEN
            A$ = "*** Warning, the range is 0..3000"
            GOSUB 80
            GOTO 30820
          ELSE GOTO 30820
         END IF
      END IF
      IF A$ <> "!" THEN
         A$ = "Invalid command.  Enter ? for help."
         GOSUB 80
         GOTO 30820
      END IF
      IF CVI(FM$) = 0 THEN _
         GOTO 30940
      A$ = ""
      GOSUB 80
      A$ = "Deleting planet..."
      GOSUB 100
      FOR L = LP + 1 TO LS
        GET 1,L
        IF CVI(FO$) = L2 - LT THEN
           LSET FO$ = MKI$(0)
           PUT 1,L
        END IF
      NEXT
      GET 1,L2
      LSET FM$ = MKI$(0)
      PUT 1,L2
      A$ = " deleted."
      GOSUB 80
      GOTO 30820
30940 A$ = ""
      GOSUB 80
      A$ = "Creating planet"
      GOSUB 80
30950 A$ = "Name? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 30820
      IF LEN(A$) > 41 THEN
         A$ = "You are limited to 41 characters."
         GOSUB 80
         GOTO 30950
       ELSE N$ = A$
      END IF
30960 A$ = ""
      GOSUB 80
      A$ = "What sector do you wish to put it in? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 30820
      S = VAL(A$)
      IF S < 1 OR S > LS - LP THEN
         A$ = "Invalid sector number."
         GOSUB 80
         GOTO 30960
      END IF
      GET 1,S + LP
      IF CVI(FO$) <> 0 THEN
         A$ = "There is already a planet there!"
         GOSUB 80
         GOTO 30960
      END IF
      GET 1,S + LP
      LSET FO$ = MKI$(L2 - LT)
      PUT 1,S+LP
      GET 1,L2
      LSET FA$ = N$
      LSET FM$ = MKI$(LEN(N$))
      PUT 1,L2
      GOTO 30820
30990 REM **/ Update planet values and load into H(),N(),P() /**
      GET 1,L2
      N(1) = CVI(FF$) + CVI(FI$)/10000
      N(2) = CVI(FG$) + CVI(FJ$)/10000
      N(3) = CVI(FH$) + CVI(FK$)/10000
      P(1) = CVI(FC$)
      P(2) = CVI(FD$)
      P(3) = CVI(FE$)
      GOSUB 1000
      C = D
      A$ = TIME$
      M = VAL(LEFT$(A$,2))*60 + VAL(MID$(A$,4,2))
      D = D - CVI(FB$) + (M-CVI(FR$))/1440
      IF D < 0 THEN
         D = 0
       ELSEIF D > 10 THEN
         D = 10
      END IF
      FOR L = 1 TO 3
        N(L) = N(L) + P(L)*D
        IF N(L) > P(L)*10 THEN _
           N(L) = P(L)*10
      NEXT
      GET 1,L2
      LSET FB$ = MKI$(C)
      LSET FF$ = MKI$(INT(N(1)))
      LSET FG$ = MKI$(INT(N(2)))
      LSET FH$ = MKI$(INT(N(3)))
      FOR L = 1 TO 3
        Z(L,0) = INT((N(L) - INT(N(L)))*10000 + .5)
        N(L) = INT(N(L))
      NEXT
      LSET FI$ = MKI$(Z(1,0))
      LSET FJ$ = MKI$(Z(2,0))
      LSET FK$ = MKI$(Z(3,0))
      LSET FR$ = MKI$(M)
      PUT 1,L2
      RETURN
31110 REM **/ Cabal Editor /**
      FOR L = 1 TO 9
        GET 1,L + LP
        G(L,0) = CVI(FT$)
        G(L,1) = 0
      NEXT
      FOR L = 1 TO 8
        FOR M = L + 1 TO 9
          IF G(L,0) = G(M,0) THEN _
             G(M,0) = 0
        NEXT
      NEXT
      G = 0
      FOR L = 1 TO 9
        IF G(L,0) <> 0 THEN _
           GET 1,G(L,0) + LP
        IF CVI(FM$)= -1 THEN _
           G(L,1) = CVI(FL$)
      NEXT
      FOR L = 1 TO 9
        GET 1,L + LP
        LSET FT$ = MKI$(G(L,0))
        PUT 1,L + LP
      NEXT
      A$ = ""
      GOSUB 80
      A$ = "Group Location Size Goal Type"
      GOSUB 80
      A$ = "===== ======== ==== ==== ===="
      GOSUB 80
      P.LINE = 0
      B$ = ""
      FOR B = 1 TO 9
        L = 3
        A = B
        GOSUB 30000
        GET 1,LP + B
        R = CVI(FT$)
        IF R = 0 THEN
           A$ = "   <Does not exist>"
           GOSUB 100
           GOTO 31250
        END IF
        G = CVI(FQ$)
        GET 1,LP + R
        L = 11
        A = R
        GOSUB 30000
        L = 19
        IF CVI(FM$) <> -1 THEN
           A = 0
           GOSUB 30000
         ELSE A = CVI(FL$)
           GOSUB 30000
        END IF
        IF G <> 0 THEN
           L = 23
           A = G
           GOSUB 30000
           P.LINE = 0
           A$ = " "
           GOSUB 100
         ELSE A$ = "     "
           GOSUB 100
        END IF
        IF B < 3 THEN
           A$ = "Defense"
           GOSUB 100
         ELSEIF B < 6 THEN
           A$ = "Wandering"
           GOSUB 100
         ELSEIF B < 9 THEN
           A$ = "Attack"
           GOSUB 100
         ELSE A$ = "Attack top player"
           GOSUB 100
        END IF
31250   A$ = ""
        GOSUB 80
        B$ = ""
      NEXT
      A$ = "[ENTER] = Exit Cabal editor"
      GOSUB 80
31270 A$ = ""
      GOSUB 80
      A$ = "Which group to edit (? = list)? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN
         GOTO 20000
       ELSEIF A$ = "?" THEN
         GOTO 31110
      END IF
      G = VAL(A$)
      IF G < 1 OR G > 9 THEN
         A$ = "Invalid group number. Must be 1 - 9!"
         GOSUB 80
         GOTO 31270
      END IF
      GET 1,G
      A$ = "Group" + STR$(G) + ":"
      GOSUB 80
      GET 1,LP + G
      R = CVI(FT$)
31310 A$ = "  Location? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 31410
      O = R
      R = VAL(A$)
      IF R < 1 OR R > LS - LP THEN
         A$ = "    Valid sector numbers are between 1 and" + _
              STR$(LP - LR) + "."
         GOSUB 80
         GOTO 31310
      END IF
      GET 1,LP + R
      IF CVI(FL$) = 0 THEN _
         GOTO 31370
      IF CVI(FM$) = -1 THEN
         A$ = "    Cabal in that sector.  <C>ombine groups or <A>bort [A]? "
         GOSUB 100
         GOSUB 400
         A$ = LEFT$(A$ + "A",1)
         IF A$ = "A" OR A$ = "a" THEN _
            GOTO 31270
         GOTO 31370
      END IF
      A$ = "    There are" + STR$(CVI(FL$)) + " fighters belonging to "
      GOSUB 100
      GET 1,CVI(FM$)
      A$ = LEFT$(FA$,CVI(FM$)) + " in that sector."
      GOSUB 80
      GET 1,R
      A$ = "    <D>elete player's fighters or <A>bort [A]? "
      GOSUB 100
      GOSUB 400
      A$ = LEFT$(A$ + "A",1)
      IF A$ = "A" OR A$ = "a" THEN _
         GOTO 31270
31370 F = 0
      IF O = 0 THEN _
         GOTO 31390
      GET 1,R + LP
      IF CVI(FM$) > 1 THEN
         LSET FM$ = MKI$(0)
         LSET FL$ = MKI$(0)
         PUT 1,R+LP
      END IF
      GET 1,O + LP
      F = CVI(FL$)
      LSET FL$ = MKI$(0)
      LSET FM$ = MKI$(0)
      PUT 1,O + LP
31390 GET 1,R + LP
      LSET FL$ = MKI$(CVI(FL$) + F)
      LSET FM$ = MKI$(-1)
      PUT 1,R + LP
      GET 1,G + LP
      LSET FT$ = MKI$(R)
      PUT 1,G + LP
31410 A$ = "  Size? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 31450
      A = VAL(A$)
      GET 1,R + LP
      LSET FL$ = MKI$(A)
      PUT 1,R + LP
      IF G > 2 AND G < 6 AND (A < 50 OR A > 100) THEN
         A$ = "    Note: The new goal of this group will be set to sector 83"
         GOSUB 80
      END IF
      IF G > 5 AND (A < 20 OR A > 50) THEN
         A$ = "    Note: This group will aim for sector 83"
         GOSUB 80
      END IF
31450 IF G < 3 OR G > 5 THEN _
         GOTO 31490
31460 A$ = "  Goal? "
      GOSUB 100
      GOSUB 400
      IF A$ = "" THEN _
         GOTO 31490
      A = VAL(A$)
      IF A < 1 OR A > LS - LP THEN
         A$ = "    Valid sector numbers are between 1 and" + STR$(LS - LP) + "."
         GOSUB 80
         GOTO 31460
      END IF
      GET 1,G + LP
      LSET FQ$ = MKI$(A)
      PUT 1,G + LP
31490 GOTO 31110

32000 REM **/ Radio Message Editor /**
	  OPEN "TWRMSG.DAT" FOR RANDOM AS #5 LEN = 80
	  FIELD 5, 2 AS HA$, _
			   2 AS HB$, _
			   2 AS HC$, _
			  74 AS HD$
	  GET 5,1
	  ER = CVI(HA$)
	  A$ = "N A T F Message:"
	  GOSUB 80
	  A$ = STRING$(79,"-")
	  GOSUB 80
32005 FOR RM = 2 TO ER
		  P.LINE = P.LINE + 1
		  GET 5,RM
		  A$ = STR$(RM)
		  A$ = A$ + STR$(CVI(HA$))
		  A$ = A$ + STR$(CVI(HC$))
		  A$ = A$ + STR$(CVI(HB$))
		  A$ = A$ + " " + HD$
		  GOSUB 80
		  IF NOFSX$ = "Q" THEN _
			 EXIT FOR
	  NEXT
32010 A$ = "Which message do you wish to edit - [L]ist, [ENTER]=end? "
	  UPLOW = 1
	  GOSUB 100
	  GOSUB 400
	  IF A$ = "" THEN
		 CLOSE 5
		 UPLOW = 0
		 GOTO 20000
	  END IF
	  IF A$ = "L" THEN _
		 GOTO 32005
	  UPLOW = 0
	  MTE = VAL(A$)
	  IF MTE > 1 AND MTE <= ER THEN
		 GET 5, MTE
		 A$ = "Message #: " + STR$(MTE)
		 GOSUB 100
		 A$ = "    Message Active: "
		 GOSUB 100
		 IF CVI(HA$) = 1 THEN
			A$ = "Yes"
		  ELSE A$ = "NO"
		 END IF
		 GOSUB 80
		 GET 1,CVI(HC$)
		 A$ = "Message From " + LEFT$(FA$,CVI(FM$)) + ":"
		 GOSUB 80
		 GET 1,CVI(HB$)
		 A$ = "Message To   " + LEFT$(FA$,CVI(FM$)) + ":"
		 GOSUB 80
		 A$ = HD$
		 GOSUB 80
		 A$ = ""
		 GOSUB 80
		 A$ = "Correct message to edit (Y/[N])? "
		 GOSUB 100
		 GOSUB 400
		 IF A$ = "" THEN _
			A$ = "N"
		 IF A$ = "Y" THEN
			GET 5,MTE
			A$ = "Make Message ACTIVE (0=no, 1=yes)? "
			GOSUB 100
			GOSUB 400
			IF A$ <> "" THEN _
			   LSET HA$ = MKI$(VAL(A$))
			A$ = "Please enter number of player the message is to:"
			GOSUB 80
			A$ = "[ENTER] means no change ? "
			GOSUB 100
			GOSUB 400
			IF A$ <> "" THEN _
			   LSET HB$ = MKI$(VAL(A$))
			A$ = "Please enter number of player the message is from:"
			GOSUB 80
			A$ = "[ENTER] means no change ? "
			GOSUB 100
			GOSUB 400
			IF A$ <> "" THEN _
			   LSET HC$ = MKI$(VAL(A$))
32015       A$ = "Messages are limited to 74 chars."
			GOSUB 80
			A$ = "Pressing [ENTER] with no input retains current message"
			GOSUB 80
			A$ = "  [" + STRING$(72,"-") + "]"
			GOSUB 80
			A$ = "? "
			GOSUB 100
			GOSUB 400
			IF LEN(A$) > 74 THEN
			   A$ = "Too Long"
			   GOSUB 80
			   GOTO 32015
			END IF
			IF A$ <> "" THEN _
			   LSET HD$ = A$
			PUT 5,MTE
		 END IF
	  END IF
	  GOTO 32010

33000 REM **/ Team Editor /**
	  UPLOW = 1
	  GET 1,1
	  HTN = CVI(FT$)
	  A$ = "Enter Team number you wish to edit - [L]ist, [ENTER]=end? "
	  GOSUB 100
	  GOSUB 400
	  IF A$ = "" THEN _
		 GOTO 20000
	  IF A$ = "L" THEN
		 GET 1,1
		 HTN = CVI(FT$)
		 FOR TTD = 1 TO HTN
			 GET 1, LT + TTD
			 A$ = "   Team [" + RIGHT$(STR$(TTD),LEN(STR$(TTD))-1) + "]"
			 GOSUB 80
			 A$ = "Team Active?      "
			 GOSUB 100
			 IF CVI(FQ$) = 1 THEN
				A$ = "Yes"
			  ELSE A$ = "NO"
			 END IF
			 GOSUB 80
			 A$ = "Team Password:    " + FV$
			 GOSUB 80
			 A$ = "Players on Team:  " + STR$(CVI(FT$))
			 GOSUB 80
			 IF NOFSX$ = "Q" THEN _
				EXIT FOR
		 NEXT
	  END IF
	  A = VAL(A$)
	  IF A <= 0 THEN _
		 GOTO 33000
	  IF A < 1 OR A > HTN THEN
		 A$ = "Invalid Team Number!"
		 GOSUB 80
		 GOTO 33000
	  END IF
33005 GET 1,LT + A
	  A$ = "    Team [" + RIGHT$(STR$(A),LEN(STR$(A))-1) + "]"
	  GOSUB 80
	  A$ = "1. Team Active?      "
	  GOSUB 100
	  IF CVI(FQ$) = 1 THEN
		 A$ = "Yes"
	   ELSE A$ = "NO"
	  END IF
	  GOSUB 80
	  A$ = "2. Team Password:    " + FV$
	  GOSUB 80
	  A$ = "3. Players on Team:  " + STR$(CVI(FT$))
	  GOSUB 80
	  A$ = ""
	  GOSUB 80
	  A$ = "Field to edit (1,2,3) [ENTER] quits? "
	  GOSUB 100
	  GOSUB 400
	  IF A$ = "" THEN _
		 GOTO 33000
	  IF A$ = "1" THEN
		 A$ = "Make Team Active, 0=no, 1=yes ? "
		 GOSUB 100
		 GOSUB 400
		 F1 = VAL(A$)
		 IF F1 >= 0 AND F1 < 2 THEN
			LSET FQ$ = MKI$(F1)
			PUT 1,LT + A
		 END IF
	  END IF
	  IF A$ = "2" THEN
		 UPLOW = 0
		 A$ = "Enter New Team Password ([ENTER] = no change)? "
		 GOSUB 100
		 GOSUB 400
		 IF LEN(A$) = 4 THEN
			LSET FV$ = A$
			PUT 1,LT + A
		 END IF
	  END IF
	  IF A$ = "3" THEN
		 A$ = "Number of players on Team? "
		 GOSUB 100
		 GOSUB 400
		 F3 = VAL(A$)
		 IF F3 <= 0 THEN
			LSET FQ$ = MKI$(0)
		 END IF
		 IF F3 > 0 AND F3 <= 4 THEN
			LSET FT$ = MKI$(F3)
			PUT 1,LT + A
		 END IF
	  END IF
	  GOTO 33005

