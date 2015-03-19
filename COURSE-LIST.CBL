       IDENTIFICATION DIVISION.
       PROGRAM-ID. COURSE-LIST.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE      ASSIGN        TO 
                                       '../FILES/COURSE-MASTER-SORT.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS SEQUENTIAL
                               RECORD KEY    IS I-COURSE-ID
                               FILE STATUS   IS WS-STAT.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FD  IN-FILE.
       01  IN-REC.
           03  I-COURSE-ID        PIC X(9).
           03  I-COURSE-NAME      PIC X(35).
           03  I-COURSE-CREDIT    PIC X(4).
           03  I-COURSE-STAT      PIC X.
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
       01  WS-DTL-LN.
           03  WS-COURSE-ID        PIC X(9).
           03  FILLER              PIC XX.
           03  WS-COURSE-NAME      PIC X(35).
           03  FILLER              PIC XX.
           03  WS-COURSE-CREDIT    PIC X(4).
           03  FILLER              PIC X(5).
           03  WS-COURSE-STAT      PIC X.
           03  FILLER              PIC X(24).
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
                           MOVE I-COURSE-ID     TO WS-COURSE-ID
                           MOVE I-COURSE-NAME   TO WS-COURSE-NAME
                           MOVE I-COURSE-CREDIT TO WS-COURSE-CREDIT
                           MOVE I-COURSE-STAT   TO WS-COURSE-STAT
                           DISPLAY WS-DTL-LN
                           ADD 1 TO WS-COUNTER
                           IF WS-COUNTER = 10
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
           
           STOP RUN.