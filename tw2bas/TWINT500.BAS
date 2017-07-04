   REM **/ Trade Wars, by Chris Sherrick (PTL) 5/85
   'Copyright 1986 Chris Sherrick
   DOOR.NAME$ = "TWINT500"
   R.ELEASE$ = "TW2.500"
   DIM M(6), _
	   P$(6), _
	   B(6), _
	   E(12), _
	   N(6), _
	   P(6), _
	   C(6), _
	   H(6)
   FS$ = "TWDATA.DAT"
   GOTO 10000
60 REM **/ Return to BBS /**
   CLOSE 1
   OPENFILE$ = ""
   A$ = "Ending TWINIT..."
   GOSUB 80
   SYSTEM
80 REM **/ Print A$ + Carriage return and linefeed/*
   PRINT A$
   RETURN
100 REM **/ Print A$ through modem /**
	PRINT A$;
	RETURN
300 REM **/ V=RND(A) /**
	V = INT(RND(1)*A) + 1
	RETURN
400 REM **/ Input A$ /*
	INPUT A$
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
2900 REM **/ Add message A$ to messages to Sysop /**
	 GOSUB 2300
	 O.PENFILE$ = "TWMESG.DAT"
	 OPEN"A",1,O.PENFILE$
	 PRINT#1,A$
	 CLOSE 1
	 O.PENFILE$ = ""
	 GOSUB 2000
	 RETURN
5800 REM **/ Open TWPMSG.DAT /**
	 O.PENFILE$ = "TWPMSG.DAT"
	 OPEN "R",1,O.PENFILE$,8
	 FIELD 1,2 AS FA$, _
			 2 AS FB$, _
			 2 AS FC$, _
			 2 AS FD$
	 RETURN
5900 REM **/ Close TWPMSG.DAT /**
	 CLOSE 1
	 O.PENFILE$ = ""
	 RETURN
10000 A$ = ""
	  GOSUB 80
	  A$ = "Trade Wars (v.ii) initialization program (500 sector version)"
	  GOSUB 80
	  A$ = "               by Chris Sherrick (PTL)"
	  GOSUB 80
	  A$ = "      (200, 500, and Team versions By John Morris)
	  GOSUB 80
	  A$ = ""
	  GOSUB 80
	  A$ = "This program will initialize Trade Wars (v.ii) and create the files"
	  GOSUB 80
	  A$ = "necessary for it to run.  In the process it will destroy any existing"
	  GOSUB 80
	  A$ = "Trade Wars (v.ii) files, thus very effectively reinitializing the entire game."
	  GOSUB 80
	  A$ = ""
	  GOSUB 80
	  A$ = "Continue (Y/N)? "
	  GOSUB 100
	  GOSUB 400
	  IF A$ <> "Y" AND A$ <> "y" THEN _
		 GOTO 60
	  A$ = ""
	  GOSUB 80
10510 A$ = "Enter the editor password.  You are limited to 41 characters, and UPPER case":GOSUB 80:A$="is different than lower case."
	  GOSUB 80
	  A$ = "Password? "
	  GOSUB 100
	  GOSUB 400
	  IF A$ = "" THEN
		 A$ = "You have to have a password to the editor."
		 GOSUB 80
		 GOTO 10510
	  END IF
	  IF LEN(A$) > 41 THEN
		 A$ = "You are limited to 41 characters."
		 GOSUB 80
		 GOTO 10510
	  END IF
	  PW$ = A$
	  A$ = ""
	  GOSUB 80
	  A$ = "Creating: TWDATA.DAT"
	  O.PENFILE$ = "TWDATA.DAT"
	  OPEN"O",1,O.PENFILE$
	  CLOSE 1
	  O.PENFILE$ = ""
	  GOSUB 2000
	  LSET FA$ = PW$
	  LSET FB$ = MKI$(LEN(PW$))
	  AY = VAL(MID$(DATE$,9,2))
	  LSET FC$ = MKI$(AY)
	  A$ = ""
	  GOSUB 80
	  A$ = "Starting year:" + STR$(AY)
	  GOSUB 80
	  A$ = "Starting player info:"
	  GOSUB 80
	  A = 20
	  A$ = "  # of fighters at start:" + STR$(A)
	  GOSUB 80
	  LSET FH$ = MKI$(A)
	  A = 200
	  A$ = "  # of credits at start:" + STR$(A)
	  GOSUB 80
	  LSET FI$ = MKI$(A)
	  A = 20
	  A$ = "  # of cargo holds at start:" + STR$(A)
	  GOSUB 80
	  LSET FJ$ = MKI$(A)
	  A = 21
	  A$ = "  # of days until an inactive player is deleted:" + STR$(A)
	  GOSUB 80
	  LSET FK$ = MKI$(A)
	  GOSUB 1000
	  A = D - 1
	  A$ = "  Last day maintence run: Yesterday"
	  GOSUB 80
	  LSET FL$ = MKI$(A)
	  A = 30
	  A$ = "  # of turns per day:" + STR$(A)
	  GOSUB 80
	  LSET FD$ = MKI$(A)
	  A = 200
	  A$ = "  Regeneration of Cabal fighters:" + STR$(A) + " ftrs/day"
	  GOSUB 80
	  LSET FR$ = MKI$(A)
	  A$ = "  Messages to TW sysop: On"
	  GOSUB 80
	  LSET FP$ = MKI$(0)
	  PUT 1,1
11000 A$ = ""
	  GOSUB 80
	  A$ = "What will be the maximum ammount of players allowed to the game at a single"
	  GOSUB 80
	  A$ = "time?  I recommend 70.  Note that each player takes up 143 bytes of disk space."
	  GOSUB 80
	  A$ = "? "
	  GOSUB 100
	  GOSUB 400
	  A = VAL(A$)
	  IF A < 1 THEN
		 A$ = "You at least have to have one player!"
		 GOSUB 80
		 GOTO 11000
	  END IF
	  LP = 1 + A
	  FOR L = 2 TO LP
		 PUT 1,L
	  NEXT
	  A$ = "  Allocating player space"
	  GOSUB 80
	  LSET FA$ = SPACE$(41)
	  LSET FB$ = MKI$(0)
	  LSET FC$ = MKI$(0)
	  LSET FD$ = MKI$(0)
	  LSET FE$ = MKI$(0)
	  LSET FF$ = MKI$(0)
	  LSET FG$ = MKI$(0)
	  LSET FH$ = MKI$(0)
	  LSET FI$ = MKI$(0)
	  LSET FJ$ = MKI$(0)
	  LSET FK$ = MKI$(0)
	  LSET FL$ = MKI$(0)
	  LSET FM$ = MKI$(0)
	  LSET FR$ = MKI$(0)
	  LSET FO$ = MKI$(0)
	  LSET FP$ = MKI$(0)
	  LSET FQ$ = MKI$(0)
	  LSET FT$ = MKI$(0)
	  LSET FV$ = MKS$(0)
	  FOR L = 2 TO LP
		 PUT 1,L
	  NEXT
	  GET 1,1
	  LSET FE$ = MKI$(LP)
	  PUT 1,1
	  A$ = ""
	  GOSUB 80
	  A$ = "Initializing sectors..."
	  GOSUB 80
	  LS = LP + 500
	  GET 1,1
	  LSET FF$ = MKI$(LS)
	  PUT 1,1
	  FOR L = 1 TO 500
		 READ A,B,C,D,E,F
		 LSET FA$ = SPACE$(41)
		 LSET FB$ = MKI$(A)
		 LSET FC$ = MKI$(B)
		 LSET FD$ = MKI$(C)
		 LSET FE$ = MKI$(D)
		 LSET FF$ = MKI$(E)
		 LSET FG$ = MKI$(F)
		 LSET FH$ = MKI$(0)
		 LSET FI$ = MKI$(0)
		 LSET FJ$ = MKI$(0)
		 LSET FK$ = MKI$(0)
		 LSET FL$ = MKI$(0)
		 LSET FM$ = MKI$(0)
		 LSET FR$ = MKI$(0)
		 LSET FO$ = MKI$(0)
		 LSET FP$ = MKI$(0)
		 LSET FQ$ = MKI$(0)
		 LSET FT$ = MKI$(0)
		 LSET FV$ = MKS$(0)
		 PUT 1,LP + L
	  NEXT
	  A$ = ""
	  GOSUB 80
	  A$ = "Initializing ports..."
	  GOSUB 80
	  GOSUB 1000
	  PD = D
	  A$ = "   They started producing 15 days ago..."
	  GOSUB 80
	  LT = LS + 150
	  GET 1,1
	  LSET FG$ = MKI$(LT)
	  PUT 1,1
	  FOR L = 1 TO 150
		 READ A$,B,C, D,E,F, G,H,I
		 LSET FA$ = A$
		 LSET FB$ = MKI$(C)
		 LSET FC$ = MKI$(PD-15)
		 LSET FD$ = MKI$(0)
		 LSET FE$ = MKI$(0)
		 LSET FF$ = MKI$(0)
		 LSET FG$ = MKI$(D)
		 LSET FH$ = MKI$(E)
		 LSET FI$ = MKI$(F)
		 LSET FJ$ = MKI$(G)
		 LSET FK$ = MKI$(H)
		 LSET FL$ = MKI$(I)
		 LSET FM$ = MKI$(LEN(A$))
		 LSET FR$ = MKI$(0)
		 LSET FO$ = MKI$(0)
		 LSET FP$ = MKI$(0)
		 LSET FQ$ = MKI$(0)
		 LSET FT$ = MKI$(0)
		 LSET FV$ = MKS$(0)
		 PUT 1,LS + L
		 GET 1,B + LP
		 LSET FH$ = MKI$(L)
		 PUT 1,B + LP
	  NEXT L
	  A$ = ""
	  GOSUB 80
	  A$ = "Initializing planets..."
	  GOSUB 80
	  A = 50
	  A$ = "   Maximum number of planets:" + STR$(A)
	  GOSUB 80
	  LL = LT + A
	  GET 1,1
	  LSET FO$ = MKI$(LL)
	  PUT 1,1
	  FOR L = 1 TO A
		 LSET FZ$ = STRING$(244,0)
		 PUT 1,L + LT
	  NEXT
	  A$ = ""
	  GOSUB 80
	  A$ = "Initializing the Cabal..."
	  GOSUB 80
	  GET 1,LP + 85
	  LSET FL$ = MKI$(3000)
	  LSET FM$ = MKI$(-1)
	  PUT 1,LP + 85
	  GET 1,LP + 1
	  LSET FT$ = MKI$(85)
	  PUT 1,LP + 1
	  A$ = ""
	  GOSUB 80
	  A$ = "Setting up messages to TW Sysop"
	  GOSUB 80
	  A$ = TIME$ + " " + DATE$ + " TW 500 initialized"
	  GOSUB 2900
	  A$ = ""
	  GOSUB 80
	  A$ = "Creating a player message file"
	  GOSUB 80
	  GOSUB 2300
	  GOSUB 5800
	  LSET FA$ = MKI$(1)
	  LSET FB$ = MKI$(0)
	  LSET FC$ = MKI$(0)
	  LSET FD$ = MKI$(0)
	  PUT 1,1
	  LSET FA$ = MKI$(0)
	  FOR L = 2 TO 100
		 PUT 1,L
	  NEXT
	  GOSUB 5900
	  A$ = ""
	  GOSUB 80
	  A$ = "Creating TW2.DAT"
	  GOSUB 80
	  A$ = "Please input filespec for Tradewars Top Ten Rankings: "
	  GOSUB 100
	  GOSUB 400
	  FIL$ = A$
	  A$ = "Please input maximum amount of idle user time (in minutes): "
	  GOSUB 100
	  GOSUB 400
	  LOG.OFF$ = A$
	  A$ = "Are you going to use Aliases (Y/N)? "
	  GOSUB 100
	  GOSUB 400
	  ALIAS = 0
	  IF A$ = "Y" OR A$ = "y" THEN
		 OPEN "TWNAME.DAT" FOR OUTPUT AS #1
		 A$ = "Dummy"
		 PRINT #1, A$;",";A$;",";A$;",";A$
		 CLOSE 1
		 ALIAS = 1
	  END IF
	  PRINT "What is the max. time in MINUTES a user can be in TW2?"
	  PRINT "(This is used only when running withOUT the door monitor)";
	  INPUT TIME.WITHOUT.MONITOR
	  OPEN "TW2.DAT" FOR OUTPUT AS #1
	  PRINT #1, FIL$
	  PRINT #1, LOG.OFF$
	  PRINT #1, ALIAS
	  PRINT #1, TIME.WITHOUT.MONITOR
	  CLOSE 1
	  A$ = "Creating TWRMSG.DAT  (Radio message file)"
	  GOSUB 80
	  OPEN "TWRMSG.DAT" FOR RANDOM AS #1 LEN = 80
	  FIELD 1, 2 AS HA$, _
				  2 AS HB$, _
				  2 AS HC$, _
				 74 AS HD$
	  LSET HA$ = MKI$(2)
	  LSET HB$ = MKI$(0)
	  LSET HC$ = MKI$(0)
	  LSET HD$ = SPACE$(74)
	  PUT 1,1
	  LSET HA$ = MKI$(0)
	  LSET HB$ = MKI$(0)
	  LSET HC$ = MKI$(0)
	  LSET HD$ = SPACE$(74)
	  PUT 1,2
	  CLOSE 1
	  A$ = "Initialization completed"
	  GOSUB 80
	  GOTO 60

	  REM **/ Star System data /**
	  REM **/ Map /**

	  DATA 2,3,4,5,6,7, 9,10,3,1,7,8, 0,20,0,4,1,2, 3,11,12,13,5,1
11001 DATA 1,4,0,0,0,6, 7,1,5,14,15,16, 8,2,1,6,75,17, 19,0,2,7,0,0
	  DATA 38,0,10,2,140,19, 0,0,140,0,2,9, 20,21,0,0,4,151, 0,0,0,22,0,4
	  DATA 4,0,0,32,46,0, 6,0,46,47,201,15, 23,16,6,14,0,142, 17,0,6,15,23,0
	  DATA 0,0,7,16,0,18, 0,25,17,238,24,0, 26,0,9,8,25,0, 27,28,0,11,3,0
	  DATA 0,0,29,0,11,0, 12,30,0,31,32,0, 0,16,15,238,34,0, 54,18,0,0,35,36
	  DATA 0,19,0,0,18,37, 252,0,38,19,0,92, 56,0,0,20,0,0, 39,0,40,110,20,0
	  DATA 0,41,42,0,151,21, 0,43,44,0,22,0, 22,0,0,0,45,141, 13,22,0,0,33,46
	  DATA 46,32,45,88,87,47, 49,23,0,0,48,0, 36,24,49,0,50,0, 53,0,24,35,51,52
	  DATA 91,252,25,0,0,0, 0,55,0,9,140,26, 0,109,110,28,500,56, 0,57,41,0,0,28
	  DATA 40,0,0,0,0,29, 0,159,58,59,43,29, 0,42,0,0,30,0, 0,59,0,0,60,30
	  DATA 0,31,0,95,0,33, 0,13,32,33,0,14, 14,0,33,0,61,0, 0,34,63,0,64,0
	  DATA 0,491,238,34,0,35, 51,35,0,65,71,0, 52,36,0,50,498,0, 75,53,36,51,152,66
	  DATA 67,68,54,36,52,0, 0,0,0,24,0,53, 0,1,56,0,38,157, 0,0,39,27,0,55
	  DATA 0,0,0,0,40,0, 0,0,159,0,59,42, 42,58,0,69,44,0, 0,44,70,0,198,224
	  DATA 0,47,0,89,0,62, 121,0,61,90,0,63, 0,0,62,0,461,48, 65,48,0,100,222,0
	  DATA 50,0,0,64,0,0, 76,0,52,0,74,152, 78,0,0,53,0,77, 0,91,0,0,53,0
	  DATA 59,0,116,118,70,224, 0,69,116,99,0,60, 0,50,0,102,0,72, 73,0,71,0,0,262
	  DATA 74,0,183,72,0,1, 0,66,179,73,152,178, 0,7,0,52,0,0, 79,77,0,66,0,0
	  DATA 0,0,67,0,76,0, 81,0,0,67,0,80, 82,80,0,76,0,0, 83,81,78,0,79,82
	  DATA 84,0,0,78,80,83, 86,83,80,79,0,0, 85,84,81,80,82,86, 0,0,0,81,83,0
	  DATA 0,0,0,83,0,0, 0,0,83,82,0,0, 0,33,88,0,89,0, 33,0,95,0,96,87
	  DATA 61,87,351,98,0,90, 62,0,89,0,120,0, 94,92,0,37,68,0, 0,93,26,0,91,0
	  DATA 0,131,0,204,92,497, 0,0,332,91,130,0, 45,0,105,0,97,88, 0,88,97,0,98,0
	  DATA 0,95,0,115,0,96, 89,96,111,0,1,0, 70,0,117,1,198,116, 64,0,101,103,104,0
	  DATA 100,0,102,0,128,0, 71,0,101,0,0,0, 100,0,104,0,0,0, 100,0,103,190,129,0
	  DATA 95,0,115,202,106,0, 105,0,107,125,108,0, 0,0,0,106,0,0, 0,106,0,126,0,149
	  DATA 0,0,39,0,139,0, 0,39,0,28,0,0, 98,0,113,112,114,0, 145,0,114,0,111,0
	  DATA 0,111,0,132,0,114, 113,112,127,111,0,230, 97,0,442,0,0,105, 69,99,117,70,118,119
	  DATA 0,99,188,136,119,116, 0,0,69,116,224,0, 0,136,116,117,185,0, 90,121,0,0,0,122
	  DATA 62,0,0,0,0,120, 120,125,123,0,200,0, 122,0,125,200,124,0, 144,0,0,143,123,0
	  DATA 188,0,106,126,0,0, 125,150,108,166,0,0, 114,133,176,231,0,0, 0,0,0,0,101,129
	  DATA 128,0,104,0,0,0, 195,94,0,0,205,331, 153,93,0,0,0,0, 113,189,0,0,135,0
	  DATA 189,127,176,170,134,135, 133,0,172,171,0,0, 133,0,0,0,0,132, 119,117,188,137,161,0
	  DATA 138,0,161,0,0,136, 0,0,0,0,137,164, 109,0,0,0,274,0, 38,0,9,0,10,0
	  DATA 31,0,0,0,0,0, 0,0,0,15,0,0, 124,0,440,0,0,144, 143,0,124,221,0,0
	  DATA 199,112,0,0,146,0, 145,0,148,230,147,0, 148,173,0,0,146,231, 146,0,129,173,147,0
	  DATA 108,150,0,0,202,0, 126,149,177,0,400,0, 11,0,0,0,0,29, 66,74,0,0,0,52
	  DATA 131,154,158,156,0,0, 0,0,0,153,0,155, 156,154,0,0,207,0, 153,154,155,0,259,0
	  DATA 55,158,0,0,0,497, 157,153,0,0,261,0, 42,0,58,0,160,0, 0,0,159,0,0,0
	  DATA 0,136,0,137,0,162, 161,0,247,163,0,0, 162,164,197,196,0,0, 138,163,0,0,0,165
	  DATA 164,0,0,0,0,0, 126,177,167,168,0,0, 166,0,225,168,169,177, 166,0,0,167,0,0
	  DATA 0,167,0,0,286,0, 176,133,232,0,172,192, 134,0,0,226,229,172, 171,194,193,134,170,192
	  DATA 148,0,0,147,174,175, 173,172,0,215,0,0, 0,216,0,0,173,174, 127,0,170,0,133,0
	  DATA 150,0,226,0,166,167, 74,179,180,184,183,0, 74,0,0,178,181,0, 178,0,244,239,241,0
	  DATA 179,0,0,182,0,244, 181,0,210,211,239,0, 178,184,0,264,0,73, 178,0,183,0,0,263
	  DATA 119,186,0,444,0,0, 0,275,0,187,0,185, 0,0,247,0,186,0, 125,0,117,136,0,0
	  DATA 133,0,0,0,0,132, 104,191,0,0,0,0, 190,0,221,0,220,0, 0,0,0,0,170,172
	  DATA 219,304,0,0,172,0, 172,0,0,229,228,0, 296,333,130,0,0,0, 0,163,0,0,245,0
	  DATA 163,0,0,250,0,0, 60,0,99,0,0,0, 145,0,0,0,0,0, 122,0,0,406,0,123
	  DATA 0,0,0,0,14,0, 105,203,0,0,0,149, 0,0,202,0,0,0, 0,93,0,0,0,205
	  DATA 130,204,0,0,0,206, 205,207,0,0,0,0, 206,0,155,209,208,0, 235,236,207,209,297,0
	  DATA 207,0,0,0,208,233, 182,0,0,214,0,0, 0,0,182,212,0,243, 211,213,345,492,0,0
	  DATA 212,214,0,0,253,258, 0,0,0,0,213,210, 174,0,218,232,0,0, 175,0,0,0,217,0
	  DATA 216,0,0,0,0,218, 215,219,217,0,0,0, 193,218,0,0,370,377, 121,191,0,0,0,222
	  DATA 144,0,0,0,191,0, 64,0,220,270,0,0, 0,0,0,0,118,224, 59,113,0,0,0,223
	  DATA 226,229,0,0,227,167, 0,0,171,0,177,225, 225,0,229,0,0,228, 194,374,0,282,227,0
	  DATA 171,194,227,0,225,0, 114,146,0,0,0,0, 0,127,0,0,0,147, 170,0,0,0,0,215
	  DATA 234,0,0,209,0,0, 233,0,237,0,0,236, 0,1,0,208,269,237, 234,0,208,0,0,0
	  DATA 235,234,0,0,0,266, 23,49,0,18,0,0, 182,180,243,0,0,0, 242,0,0,288,243,0
	  DATA 242,0,0,0,0,180, 241,240,0,0,0,0, 0,239,211,293,240,0, 180,0,0,0,0,181
	  DATA 0,196,401,248,415,246, 245,415,276,249,0,0, 0,187,162,249,0,0, 245,0,0,251,277,415
	  DATA 247,0,0,0,246,0, 197,251,0,0,0,0, 250,0,0,367,281,248, 0,0,0,26,0,37
	  DATA 291,492,213,0,0,254, 314,253,257,255,0,0, 0,0,315,0,254,256, 272,255,0,0,0,257
	  DATA 0,254,258,0,0,256, 213,257,0,0,0,0, 156,261,0,0,0,0, 0,0,0,261,0,274
	  DATA 158,0,0,260,259,0, 0,0,0,0,72,265, 0,0,184,264,0,0, 183,0,0,263,265,0
	  DATA 264,0,0,0,262,0, 237,0,0,0,273,270, 269,0,268,0,0,0, 267,0,0,0,0,297
	  DATA 235,273,271,0,0,267, 266,0,0,352,494,271, 270,494,0,299,348,269, 268,0,0,0,256,297
	  DATA 266,269,0,0,0,0, 260,139,0,0,0,0, 444,0,0,0,186,0, 246,0,0,415,278,0
	  DATA 415,0,0,248,306,278, 277,0,276,0,415,280, 490,0,484,0,474,1, 278,0,0,398,308,0
	  DATA 389,0,251,0,366,309, 375,302,284,0,285,228, 227,285,287,329,0,286, 282,0,0,303,301,287
	  DATA 283,0,282,0,0,0, 169,0,0,0,283,366, 283,301,328,0,0,0, 290,0,0,0,0,240
	  DATA 1,0,471,0,0,0, 0,0,288,0,292,347, 253,0,311,0,321,344, 293,0,0,290,0,0
	  DATA 292,424,0,0,243,0, 485,0,0,0,0,0, 272,0,296,0,297,0, 295,0,0,195,0,0
	  DATA 208,268,0,0,295,0, 0,0,0,296,0,0, 0,271,0,0,354,300, 337,0,348,299,0,0
	  DATA 287,284,303,324,313,0, 0,282,0,376,0,305, 284,0,0,305,327,301, 0,0,193,0,374,326
	  DATA 302,303,0,0,0,372, 277,309,310,0,307,398, 0,306,382,0,416,308, 307,416,378,280,0,0
	  DATA 368,0,0,281,0,306, 0,1,0,0,306,382, 291,0,312,317,0,0, 317,311,314,0,0,0
	  DATA 330,301,386,0,0,0, 0,0,312,351,254,316, 0,0,255,0,0,334, 0,0,314,318,0,0
	  DATA 311,312,0,320,322,426, 425,0,316,0,319,0, 318,0,0,0,427,361, 317,0,0,0,394,0
	  DATA 291,0,323,0,0,0, 317,0,391,0,362,0, 0,321,426,338,342,341, 301,327,0,0,0,395
	  DATA 327,397,0,1,0,372, 304,369,0,0,0,364, 0,303,0,325,396,324, 287,0,0,363,0,330
	  DATA 0,0,363,283,0,0, 0,328,0,0,385,313, 0,0,0,0,130,0, 0,0,94,0,0,0
	  DATA 0,195,0,0,0,0, 315,0,0,0,349,335, 0,337,0,0,334,0, 0,356,1,0,0,337
	  DATA 336,0,0,0,335,300, 391,0,323,0,339,0, 338,1,393,0,0,340, 339,0,0,0,342,343
	  DATA 323,346,0,0,343,0, 323,0,0,0,0,340, 341,0,340,496,0,347, 346,0,345,0,492,291
	  DATA 212,344,0,0,0,0, 344,0,0,341,347,0, 346,0,0,0,343,290, 271,349,0,0,300,0
	  DATA 348,334,350,0,0,0, 0,0,349,0,0,0, 314,0,0,0,0,0, 0,270,0,0,494,353
	  DATA 354,494,0,0,352,0, 0,0,0,353,355,299, 0,354,0,360,357,430, 357,0,0,336,0,0
	  DATA 429,356,0,0,0,355, 0,0,490,0,0,359, 358,272,0,0,0,0, 0,0,0,0,355,0
	  DATA 319,428,0,0,0,0, 322,0,0,392,0,0, 366,329,328,365,0,0, 326,0,0,371,0,372
	  DATA 363,0,384,0,0,0, 281,367,0,286,363,0, 0,0,0,251,0,366, 0,309,0,0,0,383
	  DATA 326,0,0,370,0,371, 0,0,0,219,414,369, 364,0,369,373,0,0, 0,325,305,364,373,0
	  DATA 371,0,0,0,372,412, 0,0,0,228,304,0, 282,0,0,0,0,0, 0,0,0,302,0,0
	  DATA 0,0,219,0,0,0, 0,308,380,0,0,0, 380,0,416,382,409,406, 378,416,379,0,402,0
	  DATA 0,0,486,0,0,482, 0,307,0,310,379,409, 382,0,0,368,0,387, 0,0,365,385,0,387
	  DATA 330,0,390,384,0,0, 399,0,0,388,0,417, 0,0,383,384,409,410, 386,417,418,408,390,0
	  DATA 0,0,0,281,0,0, 0,0,385,388,0,407, 0,338,426,0,322,392, 362,0,0,0,393,391
	  DATA 392,0,495,0,0,339, 428,320,0,0,0,493, 0,0,324,0,404,0, 327,0,0,397,0,405
	  DATA 0,396,0,325,411,413, 0,0,280,306,0,0, 0,0,0,386,0,0, 0,0,0,0,150,0
	  DATA 245,0,0,0,0,0, 380,0,0,431,0,403, 402,0,0,406,0,0, 0,395,0,405,422,421
	  DATA 396,404,0,0,0,423, 0,379,409,410,435,403, 390,0,0,0,408,440, 388,407,0,0,441,0
	  DATA 379,382,406,0,0,387, 0,406,0,436,0,387, 397,438,0,0,0,439, 373,0,0,0,413,0
	  DATA 0,0,397,412,438,0, 370,0,0,0,0,0, 246,245,248,277,278,276, 0,308,0,307,380,379
	  DATA 388,386,0,420,0,419, 388,0,0,419,432,0, 418,417,0,0,433,0, 417,0,0,0,0,421
	  DATA 420,404,0,0,434,0, 404,423,0,437,0,0, 0,405,0,422,0,439, 0,0,0,293,0,0
	  DATA 0,0,318,0,0,0, 317,391,323,0,0,0, 0,319,0,0,0,428, 427,0,0,394,0,361
	  DATA 357,0,0,0,0,0, 0,0,0,0,355,0, 402,0,445,0,0,435, 418,453,0,433,0,0
	  DATA 0,432,419,434,0,452, 0,421,0,0,0,433, 406,0,0,0,447,431, 410,0,0,440,0,0
	  DATA 422,0,0,451,0,443, 413,0,0,411,0,0, 0,0,423,0,411,443, 0,407,441,0,448,436
	  DATA 408,446,449,0,0,440, 0,0,0,0,0,115, 461,0,437,0,0,439, 0,0,0,275,0,185
	  DATA 431,0,0,457,0,0, 441,450,0,0,0,0, 0,435,459,458,0,0, 440,0,469,487,459,0
	  DATA 441,0,0,450,0,0, 446,0,449,0,0,468, 0,0,437,455,467,461, 433,0,0,453,455,460
	  DATA 0,0,0,432,452,454, 0,453,0,0,480,0, 0,452,451,0,0,456, 466,455,0,0,467,0
	  DATA 445,0,0,465,0,462, 447,0,0,0,487,464, 447,0,0,0,0,448, 0,0,0,0,452,0
	  DATA 451,0,443,0,0,483, 457,463,0,0,0,471, 464,0,0,462,0,473, 463,0,458,0,0,472
	  DATA 0,0,0,0,0,457, 0,0,0,0,481,456, 456,451,483,0,0,482, 475,0,450,0,0,0
	  DATA 448,478,470,0,0,0, 477,0,0,0,469,475, 289,0,462,0,0,0, 0,473,464,477,488,0
	  DATA 0,0,0,463,472,0, 279,0,0,0,0,486, 0,489,0,470,476,0, 0,0,0,475,0,479
	  DATA 0,0,487,472,470,0, 0,0,469,0,0,0, 490,0,0,0,480,476, 0,454,0,0,481,479
	  DATA 0,480,0,0,466,484, 0,0,484,467,0,381, 467,0,0,0,461,485, 481,0,482,279,0,0
	  DATA 483,0,294,0,0,0, 0,0,474,0,381,0, 458,0,0,448,0,477, 0,0,472,489,0,0
	  DATA 488,0,0,0,0,475, 0,0,358,0,479,279, 0,0,49,0,0,0, 253,344,0,0,212,0
	  DATA 0,0,0,394,0,0, 0,353,352,0,270,271, 0,0,0,393,0,0, 0,0,0,0,343,0
	  DATA 0,0,157,0,93,0, 0,0,0,0,0,51, 65,0,0,0,0,0, 0,0,39,0,0,0

	  REM **/ Data for star systems /**
	  REM **/ Name,position,class, productivity(Ore,Org,Equ)   /**
	  REM **/ Deduction/1000 units(Ore,Org,Equ)                /**

	  DATA Sol, 1, 1, 0, 0, 0, 0, 0, 0
	  DATA Alpha Centauri, 3, 2, 50, 150, 25,-25, 22,-21
	  DATA Wolf 359, 5, 3, 200, 75, 75, 20,-23,-21
	  DATA Sirius, 7, 1, 150, 75, 100,-15,-23, 26
	  DATA Centaurus, 10, 2, 20, 40, 5,-50, 50,-50
	  DATA Arcturus, 18, 2, 30, 15, 5,-48, 50,-50
	  DATA Rigel, 19, 1, 60, 10, 80,-72,-75, 80
	  DATA T'xe, 21, 2, 30, 40, 6,-60, 60,-60
	  DATA New Paris, 22, 2, 30, 40, 7,-63, 80,-70
	  DATA Palmer's World, 23, 2, 30, 20, 5,-45, 40,-43
	  DATA NGC-33, 35, 3, 40, 20, 10, 40,-30,-29
	  DATA Bourne's Star, 37, 2, 50, 30, 10,-55, 60,-57
	  DATA Aldebaran, 38, 2, 40, 35, 8,-80, 52,-69
	  DATA LNC Nebulae, 39, 2, 40, 50, 40,-40, 40,-50
	  DATA Second Home, 41, 2, 30, 30, 10,-51, 49,-50
	  DATA Daleth, 43, 3, 50, 20, 10, 75,-50,-54
	  DATA Bettelgeuse, 45, 3, 60, 20, 15, 48,-50,-51
	  DATA Koph System, 46, 1, 70, 10, 100,-70,-85, 66
	  DATA Lonely Hole, 48, 3, 20, 15, 12, 48,-30,-34
	  DATA NGC-78, 51, 3, 30, 15, 10, 30,-38,-34
	  DATA NGC-4151, 53, 3, 40, 20, 15, 52,-60,-47
	  DATA Outpost 7, 57, 3, 30, 25, 12, 60,-25,-48
	  DATA Youlose, 58, 3, 50, 35, 15, 50,-70,-86
	  DATA Last Hope, 60, 3, 30, 40, 10, 36,-60,-54
	  DATA Epsilon Erandi, 62, 2, 30, 50, 50,-50, 70,-50
	  DATA Fort Dallas, 66, 2, 50, 60, 50,-70, 80,-70
	  DATA Antares, 69, 2, 20, 40, 30,-70, 40,-70
	  DATA Gateway, 71, 2, 40, 20, 40,-75, 84,-75
	  DATA Sirus II, 73, 1, 70, 40, 50,-70,-70, 60
	  DATA Czar'ak, 77, 1, 80, 60, 70,-69,-75, 50
	  DATA Vega, 88, 2, 40, 30, 40,-50, 30,-40
	  DATA Canopus, 89, 3, 20, 30, 50, 35, -40, -80
	  DATA Ross 154, 93, 3, 30, 40, 30, 70,-60,-60
	  DATA Luyten 789-6, 94, 3, 40, 45, 50, 40,-30,-30
	  DATA Bozone, 97, 3, 30, 30, 20, 20,-40,-25
	  DATA Insearch, 101, 1, 50, 20, 75,-20,-15, 40
	  DATA Ptolemy, 105, 2, 30, 30, 30,-40, 35,-45
	  DATA Frontier 3, 107, 3, 35, 35, 20, 51,-30,-45
	  DATA Pleione, 111, 3, 40, 10, 35, 30,-20,-20
	  DATA Orion, 113, 1, 30, 40, 50,-10,-30, 45
	  DATA Triffid 4, 116, 1, 130, 70, 85,-15,-36, 50
	  DATA Diamond, 118, 3, 30, 30, 15, 40,-52,-30
	  DATA Last Chance, 120, 1, 110, 15, 60,-10,-75, 32
	  DATA Polaris, 122, 3, 30, 16, 26, 80,-30,-45
	  DATA Yaxa, 124, 2, 40, 30, 20,-75, 10,-35
	  DATA Jorgasnovara, 125, 2, 30, 40, 75,-40, 40,-40
	  DATA Quilst, 130, 1, 15, 75, 100,-12,-26, 55
	  DATA Qeegiz, 133, 3, 86, 45, 37,70,-23,-12
	  DATA Zumerbalt, 138, 2, 20, 30, 50,-85, 30,-10
	  DATA Felgercarb, 139, 1, 20, 20, 20,-28,-25, 50
	  DATA Nodi, 141, 2, 30, 25, 12,-23, 42,-36
	  DATA Dazzel, 144, 3, 40, 25, 5, 41,-50,-43
	  DATA Yimx, 145, 3, 40, 30, 10, 12,-36,-71
	  DATA Gedemondas, 149, 2, 17, 27, 37,-85, 50,-40
	  DATA Crill, 153, 2, 10, 15, 50,-41, 37,-18
	  DATA Samaara, 155, 2, 40, 50, 90,-30,30,-50
	  DATA Lofz, 157, 2, 20, 30, 85,-20, 24,-73
	  DATA Tsunamis, 161, 3, 18, 29, 46, 36,-50,-64
	  DATA Furiaz, 165, 1, 19, 39, 44,-70,-65, 66
	  DATA ZTX 4, 166, 1, 70, 40, 50,-84,-40, 35
	  DATA Ilik-69, 169, 3, 80, 20, 60, 45,-40,-55
	  DATA Marsh System, 171, 1, 40, 110, 70,-40,-55, 75
	  DATA Stargate, 173, 3, 50, 30, 10, 60,-42,-43
	  DATA Ridiculous 1, 176, 2, 20, 40, 30,-70, 50,-60
	  DATA Ceti-Alpha VI, 182, 2, 30, 15, 30,-40, 40,-65
	  DATA Frontier 8, 183, 2, 20, 30, 25,-63, 80,-70
	  DATA New America, 186, 3, 60, 20, 15, 40,-40,-50
	  DATA Criton, 190, 1, 30, 25, 20,-70,-60, 40
	  DATA Moskva, 191, 2, 200, 10, 15,-75, 75,-45
	  DATA Ahzdar, 192, 1, 90, 90, 90,-80,-80, 70
	  DATA Sukered-ya, 194, 3, 10, 200, 99, 49,-50,-53
	  DATA Gamma 12, 196, 1, 42, 16, 50,-70,-70, 60
	  DATA Sqeel, 199, 2, 70, 69, 70,-75, 95,-65
	  DATA Darion, 214, 3, 75, 45, 45, 80,-51,-53
	  DATA Caspan, 218, 2, 50, 150, 25,-25, 22,-21
	  DATA Paegan, 225, 2, 80, 100, 68,-50, 61,-40
	  DATA Mongo, 236, 3, 80, 40, 45, 85,-50,-50
	  DATA Tori, 242, 1, 190, 120, 190,-20,-25, 45
	  DATA Fomalhaut, 247, 2, 40, 30, 20,-50, 50,-50
	  DATA Xanthii, 248, 2, 30, 55, 50,-60, 75,-61
	  DATA Sinuiji, 250, 3, 70, 30, 30, 30,-40,-35
	  DATA Sardar, 254, 3, 90, 50, 40, 50,-52,-49
	  DATA Behr'ak, 256, 2, 40, 60, 55,-50, 70,-50
	  DATA Bellatrix, 259, 3, 30, 40, 15, 35,-70,-55
	  DATA Acrux, 262, 3, 100, 60, 28, 70,-45,-50
	  DATA Horok, 263, 3, 80, 30, 50, 50,-40,-45
	  DATA K'ushui, 273, 1, 80, 120, 200,-72,-80, 90
	  DATA Mialplacidas, 276, 3, 110, 80, 105, 50,-30,-35
	  DATA Rigil Kentaurus, 280, 2, 50, 70, 50,-75, 85,-75
	  DATA Jassan, 285, 2, 155, 200, 110,-45, 50,-45
	  DATA Oomaru, 290, 2, 45, 70, 40,-40, 42,-50
	  DATA Eskiis, 293, 3, 90, 50, 60, 84,-65,-35
	  DATA Taan, 295, 2, 80, 90, 60,-60, 65,-60
	  DATA Saaula, 298, 3, 75, 40, 45, 75,-40,-35
	  DATA Kep Salu, 301, 1, 80, 100, 130,-40,-40, 50
	  DATA Alam'ak, 304, 2, 95, 150, 100,-40, 50,-33
	  DATA Wezen, 305, 3, 70, 25, 25, 30,-40,-25
	  DATA Ghondr, 307, 3, 110, 50, 50, 60,-40,-35
	  DATA K'hotan, 311, 3, 70, 40, 30, 51,-30,-45
	  DATA Androcus, 316, 1, 110, 120, 180,-71,-65, 85
	  DATA Regulus, 319, 3, 50, 35, 20, 80,-50,-30
	  DATA Proxima, 329, 2, 125, 115, 100,-45, 70,-60
	  DATA Achernar B, 334, 2, 30, 90, 45,-50, 70,-55
	  DATA Ndele, 337, 3, 190, 90, 100, 25,-25,-20
	  DATA Tali, 341, 2, 40, 80, 40,-50, 30,-45
	  DATA Tutakai, 342, 3, 50, 35, 20, 60,-80,-85
	  DATA Dubhe, 344, 1, 200, 110, 195,-25,-20, 38
	  DATA Annobon, 348, 2, 40, 70, 65,-60, 80,-60
	  DATA Lux, 350, 3, 50, 30, 20, 53,-62,-46
	  DATA Mirfak, 356, 2, 95, 115, 90,-63, 85,-70
	  DATA Zindar, 360, 2, 100, 160, 45,-26, 27,-21
	  DATA Adao, 362, 3, 60, 40, 30, 30,-40,-35
	  DATA Theseus 2, 366, 2, 80, 60, 45,-55, 60,-50
	  DATA Azrael, 369, 3, 80, 40, 40, 50,-40,-35
	  DATA Etzel, 373, 2, 80, 155, 85,-36, 50,-28
	  DATA Xerxes, 379, 2, 90, 75, 50,-80, 52,-70
	  DATA Loki, 383, 2, 50, 160, 30,-28, 30,-20
	  DATA Hashishyun, 384, 3, 60, 20, 25, 70,-30,-50
	  DATA Akbar, 386, 3, 120, 100, 110, 50,-30,-35
	  DATA Abu Bekr, 387, 3, 50, 30, 20, 40,-27,-23
	  DATA Appollyon, 389, 1, 110, 60, 150,-71,-83, 65
	  DATA Iblis, 390, 2, 105, 115, 70,-61, 85,-75
	  DATA Siva, 393, 2, 30, 60, 30,-40, 40,-45
	  DATA Tyr, 394, 1, 110, 120, 180,-78,-69, 81
	  DATA Alaric, 395, 2, 90, 50, 45,-50, 65,-57
	  DATA Aeolus, 397, 2, 40, 20, 40,-75, 85,-75
	  DATA Tonti, 408, 3, 40, 30, 25, 85,-45,-50
	  DATA Canus Minor, 418, 2, 20, 40, 35,-75, 75,-75
	  DATA Equulus, 421, 3, 60, 40, 20, 80,-40,-50
	  DATA Monoceros,423, 2, 30, 50, 15,-40, 45,-80
	  DATA Corvus, 426, 2, 80, 110, 75,-60, 65,-75
	  DATA Phoenix, 429, 3, 86, 60, 45, 90,-80,-60
	  DATA Tucana, 435, 1, 70, 40, 85,-80,-85, 65
	  DATA Apian, 438, 3, 80, 70, 65, 65,-65,-65
	  DATA Doppler, 440, 3, 86, 45, 37, 85,-40,-78
	  DATA Palitzsch, 443, 2, 30, 60, 40,-40, 65,-40
	  DATA Seagate, 445, 3, 45, 15, 25, 85,-60,-40
	  DATA Chandrasekher, 446, 2, 80, 140, 90,-70, 55,-62
	  DATA Cratuer, 452, 1, 130, 40, 180,-65,-80, 70
	  DATA Ibn Duad, 459, 2, 30, 60, 50,-85, 85,-85
	  DATA Pilar, 463, 2, 40, 80, 25,-75, 80,-40
	  DATA Ketoi, 466, 2, 30, 60, 30,-40, 65,-65
	  DATA Tulan, 468, 3, 40, 16, 30, 75,-25,-40
	  DATA Alvarado, 469, 2, 90, 120, 80,-60, 70,-35
	  DATA Hrok 1, 471, 1, 90, 50, 150,-45,-75, 75
	  DATA Ramusik, 472, 3, 50, 30, 10, 65,-35,-35
	  DATA Canus Major, 482, 3, 70, 35, 35, 70,-70,-65
	  DATA H12, 483, 2, 90, 180, 100,-40, 50,-25
	  DATA Spica, 489, 2, 50, 60, 40,-70, 59,-36
	  DATA Yugonislom, 490, 3, 46, 29, 20, 50,-50,-45
	  DATA Alfr, 492, 2, 20, 85, 30,-20, 50,-35
	  DATA Tall Grass, 494, 3, 65, 40, 30, 60,-60,-60
	  DATA H9, 496, 3, 110, 60, 50, 80,-40,-65
