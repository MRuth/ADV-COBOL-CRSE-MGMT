       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCHEDULE-INQUIRY.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE      ASSIGN        TO 
                                       '../FILES/SCHEDULE-MASTER.DAT'   
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS SEQUENTIAL
                               RECORD KEY    IS SCHEDULE-ID-I
                               FILE STATUS   IS WS-STAT.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FD  IN-FILE.
       01  IN-REC.
           03  SCHEDULE-ID-I       PIC X(12).
           03  FILLER              PIC X.
           03  COURSE-ID-I.
               05  COURSE-ID-DEPT  PIC X(4).
               05  FILLER          PIC X.
               05  COURSE-ID-REST  PIC X(4).
           03  FILLER              PIC X.
           03  TIMEDAY-I           PIC X(20).
           03  FILLER              PIC X.
           03  BUILDING-ID-I       PIC X(11).
           03  FILLER              PIC X.
           03  INSTRUCTOR-ID-I     PIC X(4).
           03  FILLER              PIC X(3).
           03  OPEN-SEATS-I        PIC X(2).
       WORKING-STORAGE SECTION.
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
           03  WS-COUNTER          PIC 99 VALUE 0.
           03  WS-BLNK-LN          PIC X(80) VALUE SPACES.
           03  WS-ANOTHER          PIC X VALUE 'Y'.
               88  ANOTHER                 VALUE 'N'.
           03  WS-COURSE-DEPT      PIC X(4).
       01  WS-PG-BREAK.
           03  FILLER              PIC X(15) VALUE 'PRESS ENTER TO '.
           03  FILLER              PIC X(16) VALUE 'DISPLAY 10 MORE '.
           03  FILLER              PIC X(49) VALUE 'RECORDS'.
       01  WS-HEADER.
           03  FILLER              PIC X(13) VALUE 'SCHEDULE ID'.
           03  FILLER              PIC X(10) VALUE 'COURSE ID'.
           03  FILLER              PIC X(21)  VALUE 'TIME        DAYS'.
           03  FILLER              PIC X(12) VALUE 'BLD/ROOM'.
           03  FILLER              PIC X(5) VALUE 'INST'.
           03  FILLER              PIC X(5) VALUE 'SEATS'. 

       SCREEN SECTION.
       01  BLNK-SCRN.
           03  BLANK SCREEN.
       01  SCRN-TITLE.
           03  LINE 1  COL 30  VALUE 'SCHEDULE BY DEPARTMENT'.
       01  SCRN-DISPLAY.
       
       01  SCRN-ANOTHER.
           03  LINE 7  COL 32  VALUE 'LOOK UP ANOTHER? (Y/N)'.
           03          COL 30  PIC X TO WS-ANOTHER.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
      *     PERFORM UNTIL ANOTHER
      *         DISPLAY BLNK-SCRN
           MOVE 'BIOL' TO WS-COURSE-DEPT.    
           PERFORM 100-LIST.
           DISPLAY 'PRESS ENTER TO GO BACK TO MENU'
           ACCEPT WS-RESP.
           CLOSE IN-FILE.
           EXIT PROGRAM.
           
           
       100-LIST.
           OPEN INPUT IN-FILE
           MOVE 'N' TO WS-EOF.
           MOVE 0 TO WS-COUNTER.
           DISPLAY BLNK-SCRN.
           DISPLAY WS-HEADER.
           DISPLAY WS-BLNK-LN.
           PERFORM UNTIL EOF
               READ IN-FILE 
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF COURSE-ID-DEPT EQUALS WS-COURSE-DEPT THEN
                           DISPLAY IN-REC
                           DISPLAY WS-BLNK-LN
                           ADD 1 TO WS-COUNTER
                           IF WS-COUNTER = 10 THEN
                               DISPLAY WS-PG-BREAK
                               ACCEPT WS-RESP
                               DISPLAY BLNK-SCRN
                               DISPLAY WS-HEADER
                               DISPLAY WS-BLNK-LN
                               MOVE 0 TO WS-COUNTER
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE IN-FILE.