      ******************************************************************
      *PROGRAM : ZIP code master file inquiry                          *
      *AUTHOR  : Ken Pappas                                            *
      *DATE    : 02-08-2015                                            *
      *ABSTRACT: Inquire by city name. Multi-line display.             *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZIPC-INQCITY IS INITIAL.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ZIP-MST      ASSIGN TO "../FILES/ZIPMASTER.DAT"
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS DYNAMIC
                               RECORD KEY    IS ZIP-KEY
                               ALTERNATE KEY IS ZIP-CITY
                                   WITH DUPLICATES
                               FILE STATUS   IS WS-ZIP-STAT.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD  ZIP-MST IS EXTERNAL.
       01  ZIP-REC.
           03  ZIP-KEY.
               05  ZIP-CODE            PIC X(5).
           03  ZIP-CITY                PIC X(30).
           03  ZIP-STATE               PIC XX.
           03  ZIP-COUNTY              PIC X(30).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01  WS-MISC.
           03  WS-ZIP-STAT             PIC XX.
           03  WS-CITY                 PIC X(30)   VALUE SPACES.
           03  EXIT-FLAG               PIC X       VALUE 'N'.
               88  EXIT-REQ                        VALUE 'Y'.
           03  REC-CTR                 PIC 9999    VALUE ZERO.
           03  ROW                     PIC 99.
       COPY WS-COMMON.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       COPY SCR-COMMON.
       01  SCRN-START.
           03  LINE  03 COL 36 VALUE "  CITY INQUIRY".
       01  SCRN-INPUT.
           03  LINE  05 COL 25 VALUE "CITY".
           03  LINE  05 COL 30 PIC X(30)   TO   WS-CITY.
           03  LINE  22 COL 35 VALUE "ENTER 'XXX' TO EXIT".
       01  SCRN-TABLE. 
           03  LINE ROW COL 05 PIC 999     FROM REC-CTR.
           03  LINE ROW COL 07 VALUE SPACE.
           03  LINE ROW COL 08 PIC XXXXX   FROM ZIP-CODE.
           03  LINE ROW COL 13 VALUE SPACE.
           03  LINE ROW COL 14 PIC X(30)   FROM ZIP-CITY.
           03  LINE ROW COL 44 VALUE SPACE.
           03  LINE ROW COL 45 PIC X(30)   FROM ZIP-STATE.
           03  LINE ROW COL 47 VALUE SPACE.
           03  LINE ROW COL 48 PIC X(30)   FROM ZIP-COUNTY.
       01  SCRN-CONT. 
           03  LINE 25  COL 21 VALUE
                   "PRESS ENTER TO CONTINUE, OR 'X' TO EXIT".
           03  LINE 25  COL 61 PIC X       TO   WS-RESP.
       01  SCRN-EOF.
           03  LINE 25  COL 25 VALUE
                   "END OF FILE, PRESS ENTER TO CONTINUE".
           03  LINE 25  COL 61 PIC X       TO   WS-RESP.
       01  SCRN-NOT-FOUND.
           03  LINE 06  COL 30 VALUE "CITY NOT FOUND - PRESS ENTER".
           03           COL 58 PIC X       TO   WS-RESP.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT ZIP-MST.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           MOVE SPACES TO WS-CITY
                          WS-RESP
                          EXIT-FLAG.
           DISPLAY  HEADER, SCRN-START.
           DISPLAY  SCRN-INPUT.
           ACCEPT   SCRN-INPUT.
           DISPLAY WS-BLNK-LN.
           PERFORM  200-REQUEST-LOOP UNTIL WS-CITY = "XXX" OR "xxx".
           CLOSE    ZIP-MST.
           EXIT PROGRAM.
      *-----------------------------------------------------------------
       200-REQUEST-LOOP.
           MOVE SPACES  TO EXIT-FLAG.
           MOVE WS-CITY TO ZIP-CITY
           START   ZIP-MST KEY NOT LESS THAN ZIP-CITY
               INVALID KEY
                   DISPLAY SCRN-NOT-FOUND
                   ACCEPT  SCRN-NOT-FOUND
               NOT INVALID KEY
                   MOVE 0  TO  REC-CTR
                   MOVE 4  TO  ROW
                   DISPLAY HEADER, SCRN-START
                   PERFORM 300-READ-CITY-LOOP UNTIL EXIT-REQ
           END-START
           DISPLAY  HEADER, SCRN-START.
           DISPLAY  SCRN-INPUT.
           ACCEPT   SCRN-INPUT.
      *-----------------------------------------------------------------
       300-READ-CITY-LOOP.
           READ ZIP-MST NEXT RECORD
               AT END
                   MOVE 'Y' TO EXIT-FLAG
                   DISPLAY SCRN-EOF
                   ACCEPT  SCRN-EOF
               NOT AT END
                   ADD 1 TO ROW
                   IF ROW > 20
                       PERFORM 400-NEXT-SCREEN
                   END-IF
           END-READ.
           IF NOT EXIT-REQ
               ADD  1   TO  REC-CTR
               DISPLAY  SCRN-TABLE
           END-IF.
      *-----------------------------------------------------------------
       400-NEXT-SCREEN.
           DISPLAY SCRN-CONT
           ACCEPT  SCRN-CONT
           IF WS-RESP = 'X' OR 'x'
               MOVE 'Y' TO EXIT-FLAG
           ELSE
               MOVE 5   TO ROW
               DISPLAY HEADER, SCRN-START, WS-BLNK-LN
           END-IF.
      *-----------------------------------------------------------------      