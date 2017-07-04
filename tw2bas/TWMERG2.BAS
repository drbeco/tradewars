 '/** TWMerge2 (c) 1986-87 John Morris   I/O, Time check, Err routine
 '    09/18/87  v.1.05    RBBS-PC 15.1B comp.    * TW 2 specific *

'9001 GOTO In.put      '/** Make Door merge compatible with Nochange pgm's
'9100 GOTO Out.put     '/** Remove rem marker

In.put:                                                ' Input ARG$
     ARG$ = ""
     P.LINE = 0
     NOFSX$ = ""
	 ENDTIME = FNTI! + (VAL(LOG.OFF$)*60)

In.put1:                                               ' Idle user check
	 IF FNTI! > ENDTIME AND L.OCAL <> 1 THEN
		X.LN$ = "NO INPUT IN " + _
				LOG.OFF$ + _
				" MINUTES! YOU ARE LOGGED OFF! BYE!"
		CALL X.lnprint(X.LN$,SNOOP,L.OCAL)
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
		CALL Uplow(ARG$)
     NORET = 0
     RETURN

Out.put:         '** Print LN$ If NORET = 1 then print without carriage return
	 IF GRAPHICS = 1 THEN _
		CALL ANSI.COLOR(FC,BC,BLINK,HIGH,L.OCAL)
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
	 IF GRAPHICS = 1 THEN _
		PRINT #3, COLOR.NORMAL$;
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
	 IF GRAPHICS = 1 THEN _
		PRINT #3, COLOR.NORMAL$;
     IF LFEED = 1 THEN _
        PRINT #3,LF$;
	 GOSUB Car.test

Chk.line1:
     P.LINE = P.LINE + 1
	 IF P.LINE >= 23 THEN _
        GOSUB Lin.cntr

Chk.line2:
     GOSUB Check.time2
     GOSUB Check.time1
     NORET = 0
     RETURN

Lin.cntr:                                      '** pause if > 23 lns **
	 FCSAVE = FC
	 BCSAVE = BC
	 P.LINE = 0                                '** reset line counter
	 FC = 4
	 BC = 7
     LN$ = "[ENTER] to continue, [E] to end "
     NORET = 1
     GOSUB Out.put
     GOSUB In.put
     NOFSX$ = ARG$
     IF INSTR(NOFSX$,"e") <> 0 OR _
        INSTR(NOFSX$,"E") <> 0 THEN _
		NOFSX$ = "Q"
	 P.LINE = 0
	 FC = FCSAVE
	 BC = BCSAVE
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
	 IF ERL = 5008 AND ERR = 53 THEN
		O.PENFILE$ = "TWOPENG.DAT"
		RESUME NEXT
	 END IF
	 IF ERL = 5005 AND ERR = 53 THEN
        PRINT "PLEASE CREATE 'TWOPENG.DAT'"
        RESUME Exit.door
     END IF
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
	 LN$ = "TWMerg2 1.06 Error >" + _
           STR$(ERR) + _
           " Line >" + _
           STR$(ERL) + _
		   " File >" + _
           DOOR.NAME$ + _
		   " Date >" + _
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
		IF INT((TIME.OFF-FNTI!)/60) < TIME.SAVE THEN
		   WARNING = WARNING + 60
		   X.LN$ = "** YOU HAVE" + _
				   STR$(INT((TIME.OFF-FNTI!)/60)) + _
				   " MINUTES REMAINING!! **"
		   IF L.OCAL <> 1 THEN _
			  PRINT #3, CHR$(7)
		   CALL X.lnprint(X.LN$,SNOOP,L.OCAL)
		   TIME.SAVE = INT((TIME.OFF-FNTI!)/60)
		   T.IMER = 0
		   RETURN
		END IF
     END IF
     RETURN

Check.time2:
	 IF FNTI! > TIME.OFF THEN
		KILL TIME.OFF.FILE$
        X.LN$ = "TIME LIMIT EXCEEDED!"
		CALL X.lnprint(X.LN$,SNOOP,L.OCAL)
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
	 ELSEIF ASC(CHAT$) = 27 THEN
        GOTO Local.inp
	 ELSE
		PRINT CHAT$;
		PRINT #3,CHAT$;
	 END IF
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
	 ELSE
		PRINT CHAT$;
		PRINT #3,CHAT$;
	 END IF
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

Indoor:
64002 OPEN TIME.OFF.FILE$ FOR INPUT AS #2
	  INPUT #2, TIME.OFF, USERNUM, POINTS#, MIN!, NULLS, GRAPHICS, MON.SUB$, SOUND$
      CLOSE 2
      ORIG.POINTS# = POINTS#
      RETURN

Indoor.end:
      EC = 0
	  TIME.OFF = FNTI! + (NM.TIME*60)
	  IF GRP = 2 THEN _
		 GRAPHICS = 1
      NO.MONITOR = 1
      RETURN

Outdoor:
	 CALL OUTDOOR(MON.SUB$,RD$,ORIG.POINTS#,POINTS#,NO.MONITOR)
	 RETURN

Sysop.name:
     SYS.OP = 1
     IF INP(MSR)<128 THEN _
        L.OCAL = 1
     FIRST$ = SYSOP.FIRST$
     LAST$ = SYSOP.LAST$
     CLOSE 3
     RETURN

Snoop:
     IF L.OCAL = 1 THEN _
        GOTO Snoop.end
     IF NOT SNOOP THEN
        LOCATE 24,1,0
        PRINT "SNOOP ON"
		CALL Line25
        SNOOP = NOT SNOOP
      ELSE LOCATE ,,0
        SNOOP = FALSE
        CLS
     END IF
Snoop.end:
     RETURN

Forceoff:
	 CALL FORCEOFF(TIME.OFF,WARNING,TIME.SAVE)
	 GOSUB OUT.PUT
	 RETURN

ALIAS:
	 CALL GET.ALIAS(FIRST$,LAST$,FOUND)
	 IF FOUND = 1 THEN
		FOUND = 0
		RETURN
	 END IF
ALIAS1:
	 LN$ = FIRST$ + ", Do you wish to use an Alias? "
	 NORET = 1
	 GOSUB OUT.PUT
	 GOSUB IN.PUT
	 IF LEFT$(ARG$,1) = "N" OR LEFT$(ARG$,1) = "n" THEN
		LN$ = ""
		GOSUB OUT.PUT
		ALIAS.FIRST$ = FIRST$
		ALIAS.LAST$ = LAST$
		CALL PUT.ALIAS(FIRST$,LAST$,ALIAS.FIRST$,ALIAS.LAST$)
		RETURN
	 END IF
ALIAS2:
	 LN$ = "Enter the FIRST name of your Alias? "
	 NORET = 1
	 GOSUB OUT.PUT
	 GOSUB IN.PUT
	 IF ARG$ = "" THEN _
		GOTO ALIAS1
	 CALL NAM.ADJ(ARG$)
	 IF (ARG$ = "Sysop" OR ARG$ = "Cabal") THEN
		LN$ = "That ALIAS is NOT allowed. Please choose another."
		GOSUB OUT.PUT
		GOTO ALIAS2
	 END IF
	 ALIAS.FIRST$ = ARG$
	 LN$ = ""
	 GOSUB OUT.PUT
ALIAS3:
	 LN$ = "Enter the LAST name of your Alias? "
	 NORET = 1
	 GOSUB OUT.PUT
	 GOSUB IN.PUT
	 IF ARG$ = "" THEN _
		GOTO ALIAS3
	 CALL NAM.ADJ(ARG$)
	 IF (ARG$ = "Sysop" OR ARG$ = "Cabal") THEN
		LN$ = "That Alias is NOT allowed. Please choose another."
		GOSUB OUT.PUT
		GOTO ALIAS3
	 END IF
	 ALIAS.LAST$ = ARG$
	 FULL.ALIAS$ = ALIAS.FIRST$ + " " + ALIAS.LAST$
	 CALL CHECK.ALIAS(FULL.ALIAS$,FOUND)
	 IF FOUND = 1 THEN
		LN$ = "I'm sorry " + FIRST$ + ", but that Alias is already in use"
		GOSUB OUT.PUT
		GOTO ALIAS2
	 END IF
	 LN$ = FIRST$ + " " + LAST$ + "  a.k.a  " + FULL.ALIAS$
	 GOSUB OUT.PUT
	 LN$ = ""
	 GOSUB OUT.PUT
	 LN$ = "Is this correct (Y/[N])? "
	 NORET = 1
	 GOSUB OUT.PUT
	 GOSUB IN.PUT
	 IF LEFT$(ARG$,1) = "Y" OR LEFT$(ARG$,1) = "y" THEN
		CALL PUT.ALIAS(FIRST$,LAST$,ALIAS.FIRST$,ALIAS.LAST$)
		LN$ = ""
		GOSUB OUT.PUT
		LN$ = "Your Alias has been recorded. Have fun!"
		GOSUB OUT.PUT
		LN$ = ""
		GOSUB OUT.PUT
		FIRST$ = ALIAS.FIRST$
		LAST$ = ALIAS.LAST$
		RETURN
	 END IF
	 GOTO ALIAS1

	 SUB PARSE.NAME(RBBS.USERS.NAME$) STATIC
	 WHILE RIGHT$(RBBS.USERS.NAME$,1) = " "
		RBBS.USERS.NAME$ = LEFT$(RBBS.USERS.NAME$,LEN(RBBS.USERS.NAME$)-1)
	 WEND
	 END SUB



