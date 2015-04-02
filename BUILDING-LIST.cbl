       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUILDING-LIST.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BLD-MASTER   ASSIGN        TO 
                               '../FILES/BUILDING-MASTER.DAT'   
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS SEQUENTIAL
                               RECORD KEY    IS BLD-BUILDING-ROOM
                               FILE STATUS   IS WS-STAT.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       FD  BLD-MASTER.
       01  BLD-REC.
           03  BLD-BUILDING-ROOM   PIC X(12).
           03  BLD-MAX-SEAT        PIC 99.
       WORKING-STORAGE SECTION.
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
           03  WS-SAVE             PIC X   VALUE 'N'.
               88  SAVE                    VALUE 'Y'.
           03  WS-ANOTHER          PIC X   VALUE 'Y'.
               88  ANOTHER                 VALUE 'N'.
           03  WS-COUNTER          PIC 99 VALUE 0.
           03  WS-BLNK-LN          PIC X(80) VALUE SPACES.
       01  WS-PG-BREAK.
           03  FILLER              PIC X(15) VALUE 'PRESS ENTER TO '.
           03  FILLER              PIC X(16) VALUE 'DISPLAY 10 MORE '.
           03  FILLER              PIC X(49) VALUE 'RECORDS'.
       01  WS-HEADER.
           03  FILLER              PIC X(15) VALUE 'BUILDING-ROOM'.
           03  FILLER              PIC X(65) VALUE 'MAX SEAT'.
       01  WS-DTL-LN.
           03  WS-BLD-ROOM         PIC X(12).
           03  FILLER              PIC X(3) VALUE SPACES.
           03  WS-MAX-SEAT         PIC Z9.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  BLNK-SCRN.
           03  BLANK SCREEN.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT BLD-MASTER.
           
           MOVE 'N' TO WS-EOF.
           MOVE 0 TO WS-COUNTER.
           DISPLAY BLNK-SCRN.
           DISPLAY WS-HEADER.
           DISPLAY WS-BLNK-LN.
           PERFORM UNTIL EOF
               READ BLD-MASTER
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                           MOVE BLD-BUILDING-ROOM   TO WS-BLD-ROOM
                           MOVE BLD-MAX-SEAT        TO WS-MAX-SEAT
                           DISPLAY WS-DTL-LN
                           DISPLAY WS-BLNK-LN
                           ADD 1 TO WS-COUNTER
                           IF WS-COUNTER = 10
                               THEN
                                   DISPLAY WS-PG-BREAK
                                   ACCEPT WS-RESP
                                   DISPLAY BLNK-SCRN
                                   DISPLAY WS-HEADER
                                   DISPLAY WS-BLNK-LN
                                   MOVE 0 TO WS-COUNTER
                           END-IF           
               END-READ
           END-PERFORM.
           
           DISPLAY 'PRESS ENTER TO GO BACK TO MENU'
           ACCEPT WS-RESP.
           CLOSE BLD-MASTER.
           EXIT PROGRAM.