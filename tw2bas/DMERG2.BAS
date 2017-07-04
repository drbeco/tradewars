 '/** DoorMerge2 (c) 1986-87 John Morris   I/O, Time check, Err routine
 '    06/30/87  v.4.0       RBBS-PC 15.1A comp.

'9001 GOTO In.put      '/** Make Door merge compatible with Nochange pgm's
'9100 GOTO Out.put     '/** Remove rem marker

In.put:                                                ' Input ARG$
     ARG$ = ""
     P.LINE = 0
     NOFSX$ = ""
     LOG.OFF$ = "5"
	 ENDTIME = FNTI! + (VAL(LOG.OFF$)*60)

In.put1:                                               ' Idle user check
	 IF FNTI! > ENDTIME AND L.OCAL <> 1 THEN
		X.LN$ = "NO INPUT IN " + _
				LOG.OFF$ + _
				" MINUTES! YOU ARE LOGGED OFF! BYE!"
		GOSUB X.lnprint
		GOTO Fast.exit
	 END IF

In.put2:                                       'IN.PUT2 is one of two
     IF ZX$ = "" THEN _                        'stack command routines
        GOTO In.put3                           'IN.RET is the other
     A$ = LEFT$(ZX$,1)
     IF LEN(ZX$) < 1 THEN
        ZX$ = ""
        GOTO In.put3
     END IF
     ZX$ = RIGHT$(ZX$,LEN(ZX$)-1)
     GOTO In.put4

In.put3:
     GOSUB Car.test
     A$ = INKEY$

In.put4:
     IF A$ <> "" THEN _
        GOTO In.put5
     IF L.OCAL <> 1 THEN _
        IF NOT(EOF(3)) THEN _
           A$ = INPUT$(1,3)

In.put5:
     IF A$ = CR$ THEN
        NORET = 0
        LN$ = ""
        GOSUB Out.put
        GOTO In.ret
     END IF
     IF A$ = CHR$(8) OR A$ = CHR$(7) THEN _
        GOTO In.put8
     IF A$ < CHR$(32) THEN _
        GOTO In.put1
     IF L.OCAL <> 1 AND NOT SNOOP THEN _
        GOTO In.put6
     PRINT A$;

In.put6:
     IF L.OCAL = 1 THEN _
        GOTO In.put7
     PRINT #3,A$;

In.put7:
     IF LEN(ARG$) > 254 THEN _
        GOTO In.put1
     ARG$ = ARG$ + A$
     LN$ = A$
     NORET = 1
     GOSUB Car.test
     GOTO In.put1

In.put8:                                       ' Backspace
     IF LEN(ARG$) = 0 THEN _
        GOTO In.put1
     ARG$ = LEFT$(ARG$,LEN(ARG$)-1)
     PRINT BK.ARW$;
     IF L.OCAL = 1 THEN _
        GOTO In.put1
     PRINT #3, BACK.SP$;
     GOTO In.put1

In.ret:                                        'other stack command routine
     ZB = INSTR(ARG$,";")
     IF ZB = 0 THEN
        GOTO In.ret1
      ELSE ZX$ = ZX$ + MID$(ARG$,ZB + 1)
        ARG$ = LEFT$(ARG$,ZB - 1)
        WHILE INSTR(ZX$,";") <> 0
          MID$(ZX$,INSTR(ZX$,";"),1) = CHR$(13)
        WEND
        ZX$ = ZX$ + CHR$(13)
     END IF
In.ret1:
     IF UPLOW = 1 THEN _
        GOSUB Uplow
     NORET = 0
     RETURN

Out.put:         '** Print LN$: If NORET=1 then print without carriage return
	 IF GRAPHICS = 1 THEN _
		GOSUB ANSI.COLOR
	 IF NORET = 1 THEN
        NORET = 0
        GOTO Out.put1
     END IF
     GOTO Out.put3

Out.put1:                                      '** PRINT LN$ W/O C/R
     IF L.OCAL <> 1 AND NOT SNOOP THEN _
        GOTO Out.put2
     PRINT LN$;

Out.put2:
     IF L.OCAL = 1 THEN _
        GOTO Chk.line1
     PRINT #3,LN$;
     GOSUB Car.test
     GOTO Chk.line1

Out.put3:                                      '** PRINT LN$ W/ C/R
     IF L.OCAL <> 1 AND NOT SNOOP THEN _
        GOTO Out.put4
     PRINT LN$

Out.put4:
     IF L.OCAL = 1 THEN _
        GOTO Chk.line1
     PRINT #3,LN$
     IF LFEED = 1 THEN _
        PRINT #3,LF$;
	 GOSUB Car.test

Chk.line1:
     P.LINE = P.LINE + 1
     IF P.LINE = 23 THEN _
        GOSUB Lin.cntr

Chk.line2:
     GOSUB Check.time2
     GOSUB Check.time1
     NORET = 0
     RETURN

Lin.cntr:                                      '** pause if > 23 lns **
     LN$ = "[ENTER] to continue, [E] to end "
     NORET = 1
     GOSUB Out.put
     GOSUB In.put
     NOFSX$ = ARG$
     IF INSTR(NOFSX$,"e") <> 0 OR _
        INSTR(NOFSX$,"E") <> 0 THEN _
        NOFSX$ = "Q"
     P.LINE = 0                                  '** reset line counter
     RETURN

Car.test:                                      '** Check carrier drop
     IF L.OCAL = 1 THEN _
        RETURN
     IF INP(MSR) => 128 THEN _
        RETURN
     IF SNOOP THEN _
        PRINT "(**CARRIER DROPPED**) Returning to RBBS!"
     RETURN Car.drop

Car.drop:
     CLOSE
     GOTO Exit.door

Fast.exit:                                     '** Close and return to system
     CLOSE
     OUT MCR,INP(MCR) OR 1
     SYSTEM

Exit.door:
     IF NO.MONITOR = 1 THEN _
        END
Exit.door1:
	 RUN MON.SUB$ + "MONITOR1.EXE"

Err.routine:                                   '** Error routine
     IF ERR = 57 THEN _                        'Tolerate lousy communications
        RESUME
     IF ERL = 2 THEN
		PRINT "ERROR WITH MAIN MESSAGE FILE  (MESSAGES)"
        RESUME Fast.exit
     END IF
     IF ERL = 64005 AND ERR = 5 THEN _
        RESUME NEXT
     IF ERL = 64002 AND ERR = 53 THEN
        RESUME Indoor.end
     END IF

     IF ERL = 64003 AND ERR = 53 THEN
        PRINT DEF.NAME$;" -- (RBBS-PC.DEF) NOT FOUND!"
        RESUME Fast.exit
     END IF

	 IF (ERR = 5 OR ERR = 6 OR ERR = 13 OR ERR = 15) THEN _
        RESUME

	 IF ERR = 75 THEN
        LN$ = "FILE ACCESS ERROR - STOPPING " + DOOR.NAME$
        GOSUB Out.put
        RESUME Exit.door
     END IF

Write.err:
     CLOSE 4
     LN$ = "Error >" + _
           STR$(ERR) + _
           " Line >" + _
           STR$(ERL) + _
           " In file >" + _
           DOOR.NAME$ + _
           " on date >" + _
           DATE$ + _
           " " + _
           TIME$
     GOSUB Out.put
     OPEN "ERRORS.DOR" FOR APPEND AS #4
     PRINT #4, LN$
     CLOSE #4
     GOTO Exit.door

Check.time1:
     WARNING = TIME.OFF - 180
     IF FNTI! > WARNING AND T.IMER = 1 THEN
        WARNING = WARNING + 60
		X.LN$ = "** YOU HAVE" + _
                STR$(INT((TIME.OFF-FNTI!)/60)) + _
				" MINUTES REMAINING!! **"
		IF L.OCAL <> 1 THEN _
		   PRINT #3, CHR$(7)
        GOSUB X.lnprint
        T.IMER = 0
        RETURN
     END IF
     RETURN

Check.time2:
     IF FNTI! > TIME.OFF THEN
        X.LN$ = "TIME LIMIT EXCEEDED!"
        GOSUB X.lnprint
        RETURN Fast.exit
     END IF
     RETURN

Chatter:                                       '*** F-10 CHAT MODE ***
     SAVETIME = TIME.OFF-FNTI!
     LINE.SAVE$ = LN$
     NORET = 0
     LN$ = ""
     GOSUB Out.put
     GOSUB Out.put
     PRINT "SysOp - Hit ESC to exit chat mode"
     LN$ = "Hi " + _
           FIRST$ + _
           "!  This is " + _
           SYSOP.FIRST$ + _
           " " + _
           SYSOP.LAST$ + _
           ","
     GOSUB Out.put

Remote:
     IF LOC(3) = 0 THEN _
        GOTO Local.test
     CHAT$ = INPUT$(1,3)
     IF ASC(CHAT$) = 8 THEN
        PRINT BK.ARW$;
        PRINT #3, BACK.SP$;
     END IF
     IF ASC(CHAT$) = 27 THEN _
        GOTO Local.inp
     PRINT CHAT$;
     PRINT #3,CHAT$;
     IF ASC(CHAT$) = 13 THEN _
        PRINT #3,CHR$(10);
     GOTO Remote

Local.test:
     GOSUB Car.test
     CHAT$ = INKEY$
     IF LEN(CHAT$) = 0 THEN _
        GOTO Remote

Local.inp:
     IF ASC(CHAT$) = 27 THEN _
        GOTO Chat.end
     IF ASC(CHAT$) = 8 THEN
        PRINT BK.ARW$;
        PRINT #3, BACK.SP$;
     END IF
     PRINT CHAT$;
     PRINT #3,CHAT$;
     IF ASC(CHAT$) = 13 THEN _
        PRINT #3,CHR$(10);
     GOTO Remote

Chat.end:
     TIME.OFF = FNTI! + SAVETIME
     WARNING = TIME.OFF-(3*60)
     ENDTIME = FNTI! + 240
     LN$ = ""
     GOSUB Out.put
     ARG$ = ""
     LN$ = "Chat mode terminated"
     GOSUB Out.put
     LN$ = LINE.SAVE$
     GOSUB Out.put
     RETURN

Forceoff:                                      '*** F-9 FORCE OFF ***
     TEMP.STRING$ = STRING$(15,"*")
     LINE.SAVE$ = LN$
     PRINT "SysOp, how many minutes till user is forced off [";_
           INT((TIME.OFF-FNTI!)/60);"]";
     INPUT MIN.F.O$
     IF MIN.F.O$ = "" THEN _
        RETURN
     FORCE.OFF = VAL(MIN.F.O$)
     LN$ = TEMP.STRING$ + _
           "> IMPORTANT MESSAGE <" + _
           TEMP.STRING$
     GOSUB Out.put
     LN$ = SYSOP.FIRST$ + _
           " " + _
           SYSOP.LAST$ + _
           " needs to use " + _
           RBBS.NAME$ + _
           "!  It will be closing down"
     GOSUB Out.put
     LN$ = "in" + _
           STR$(FORCE.OFF) + _
           " minutes!  Please complete what you are doing within that time"
     GOSUB Out.put
     LN$ = TEMP.STRING$ + _
           ">   END OF MESSAGE  <" + _
           TEMP.STRING$
     GOSUB Out.put
     LN$ = LINE.SAVE$
     GOSUB Out.put
     TIME.OFF = FNTI! + (FORCE.OFF*60)
     WARNING = TIME.OFF - (3*60)
     RETURN

Indoor:
64002 OPEN TIME.OFF.FILE$ FOR INPUT AS #2
	  INPUT #2, TIME.OFF, USERNUM, POINTS#, MIN!, NULLS, GRAPHICS, MON.SUB$, SOUND$
      CLOSE 2
      ORIG.POINTS# = POINTS#
      RETURN

Indoor.end:
      EC = 0
      TIME.OFF = FNTI! + 2700
      NO.MONITOR = 1
      RETURN

Outdoor:
     IF NO.MONITOR = 1 THEN _    ' EXIT IF RUNNING WITHOUT DOOR MONITOR
        RETURN
	 OPEN MON.SUB$ + POINTS.FILE$ FOR OUTPUT AS #2
     MIDSEC! = FNTI!
     NEW.POINTS# = POINTS# - ORIG.POINTS#
     PRINT #2, MIDSEC!
     PRINT #2, NEW.POINTS#
     CLOSE 2
     RETURN

X.lnprint:
     IF SNOOP THEN _
        PRINT X.LN$
     IF L.OCAL<>1 THEN _
        PRINT #3,X.LN$
     RETURN

Read.def:
64003 OPEN DEF.NAME$ FOR INPUT AS #1
      INPUT #1, Q$,Q$,Q$,SYSOP.FIRST$,SYSOP.LAST$,Q,Q,Q,Q!,Q,Q,Q$,Q,Q,Q,Q,Q,Q$,Q$,Q$,Q$,Q$,Q$,Q,Q,Q,Q,Q,Q,Q$
      INPUT #1, MESSAGE.FILE$,Q$,Q$,Q$,Q$,Q$,Q$,Q$,COM.PORT$,Q,Q$,Q$,Q,Q,Q,Q,Q$,Q$,Q
      INPUT #1, Q$,Q$,Q$,Q$,Q$,Q$,Q$,Q$,Q$,Q,Q,Q,Q,Q,Q,Q,Q,Q$
      INPUT #1, Q,Q,Q,Q$,Q,Q$,Q,Q,Q,Q,Q,Q,Q,Q,Q$,Q,Q,Q,Q,Q
      INPUT #1, Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q$
      INPUT #1, Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q,Q!,Q,Q,RBBS.NAME$
      CLOSE 1                              '** Q, Q$, Q!   dummy variables **
      IF COM.PORT$ = "COM1" THEN
         MCR = &H3FC
         MSR = &H3FE
         RETURN
      END IF
      MCR = &H2FC
      MSR = &H2FE
      RETURN

Sysop.name:
     SYS.OP = 1
     IF INP(MSR)<128 THEN _
        L.OCAL = 1
     FIRST$ = SYSOP.FIRST$
     LAST$ = SYSOP.LAST$
     CLOSE 3
     RETURN

Read.command:
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
	 POINTS.FILE$ = "POINTS" + RD$ + ".DOR"
	 TIME.OFF.FILE$ = "TIMEOFF" + RD$ + ".DOR"
Read.end:
	 IF RD$ = "-" THEN
		POINTS.FILE$ = "POINTS0.DOR"
		TIME.OFF.FILE$ = "TIMEOFF0.DOR"
		RD$ = "1"
	 END IF
     RETURN

Snoop:
     IF L.OCAL = 1 THEN _
        GOTO Snoop.end
     IF NOT SNOOP THEN
        LOCATE 24,1,0
        PRINT "SNOOP ON"
        GOSUB Line25
        SNOOP = NOT SNOOP
      ELSE LOCATE ,,0
        SNOOP = FALSE
        CLS
     END IF
Snoop.end:
     RETURN

Line25:
64005   COLOR 11,1
        LOCATE 25,1
        PRINT FIRST$ + " " + LAST$; _
			  TAB(32);DOOR.NAME$; _
			  TAB(55)"Node: " + RD$; _
              TAB(70);ON.AT$ + " "
        COLOR 7,0
        RETURN

Uplow:
	  CONV$ = ARG$
	  FOR Z = 1 TO LEN(CONV$)
		  IF MID$(CONV$,Z,1) > "@" THEN
			 MID$(CONV$,Z,1) = CHR$(ASC(MID$(CONV$,Z,1)) AND 223)
		  END IF
	  NEXT
	  ARG$ = CONV$
	  RETURN

ANSI.COLOR:
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
		RETURN
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
	 RETURN

ANSI.COL:
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
	 RETURN

	 SUB PARSE.NAME(RBBS.USERS.NAME$) STATIC
	 WHILE RIGHT$(RBBS.USERS.NAME$,1) = " "
		RBBS.USERS.NAME$ = LEFT$(RBBS.USERS.NAME$,LEN(RBBS.USERS.NAME$)-1)
	 WEND
	 END SUB



