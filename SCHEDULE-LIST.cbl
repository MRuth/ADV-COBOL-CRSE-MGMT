       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCHEDULE-LIST.
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
           03  COURSE-ID-I         PIC X(9).
           03  FILLER              PIC X.
           03  COURSE-NAME-I       PIC X(30).
           03  FILLER              PIC X.
           03  COURSE-CREDIT-I     PIC X(3).
           03  FILLER              PIC X.
           03  I-TIMEDAY-I         PIC X(20).
           03  FILLER              PIC X.
           03  BUILDING-ID-I       PIC X(11).
           03  FILLER              PIC X.
           03  INSTRUCTOR-NAME-I   PIC X(22).
           03  FILLER              PIC X.
           03  OPEN-SEATS-I        PIC X(2).
       WORKING-STORAGE SECTION.
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
           03  WS-COUNTER          PIC 99 VALUE 0.
           03  WS-BLNK-LN          PIC X(80) VALUE SPACES.
       01  WS-PG-BREAK.
           03  FILLER              PIC X(15) VALUE 'PRESS ENTER TO '.
           03  FILLER              PIC X(16) VALUE 'DISPLAY 10 MORE '.
           03  FILLER              PIC X(49) VALUE 'RECORDS'.
       01  WS-HEADER.
           03  FILLER              PIC X(11) VALUE 'COURSE ID'.
           03  FILLER              PIC X(37) VALUE 'COURSE NAME'.
           03  FILLER              PIC X(7)  VALUE 'CREDIT'.
           03  FILLER              PIC X(25) VALUE 'STATUS'.

       SCREEN SECTION.
       01  BLNK-SCREEN.
           03  BLANK SCREEN.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT IN-FILE.
           
           MOVE 'N' TO WS-EOF.
           MOVE 0 TO WS-COUNTER.
           DISPLAY BLNK-SCREEN.
           DISPLAY WS-HEADER.
           DISPLAY WS-BLNK-LN.
           PERFORM UNTIL EOF
               READ IN-FILE 
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                           DISPLAY IN-REC

                           ADD 1 TO WS-COUNTER
                           IF WS-COUNTER = 5
                               THEN
                                   DISPLAY WS-PG-BREAK
                                   ACCEPT WS-RESP
                                   DISPLAY BLNK-SCREEN
                                   DISPLAY WS-HEADER
                                   DISPLAY WS-BLNK-LN
                                   MOVE 0 TO WS-COUNTER
                           END-IF           
               END-READ
           END-PERFORM.
           
           DISPLAY 'PRESS ENTER TO GO BACK TO MENU'
           ACCEPT WS-RESP.
           CLOSE IN-FILE.
           EXIT PROGRAM.