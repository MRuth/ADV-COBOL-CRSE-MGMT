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
       COPY WS-COMMON.
       01  WS-PG-BREAK.
           03  FILLER              PIC X(15) VALUE 'PRESS ENTER TO '.
           03  FILLER              PIC X(13) VALUE 'DISPLAY MORE '.
           03  FILLER              PIC X(47) VALUE 'RECORDS'.
       01  WS-HEADER.
           03  FILLER              PIC X(15) VALUE 'BUILDING-ROOM'.
           03  FILLER              PIC X(65) VALUE 'MAX SEATS'.
       01  WS-DTL-LN.
           03  WS-BLD-ROOM         PIC X(12).
           03  FILLER              PIC X(3) VALUE SPACES.
           03  WS-MAX-SEAT         PIC Z9.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       COPY SCR-COMMON. 
       01  SCR-TITLE.
           03 LINE 03 COL 37 VALUE 'LIST BUILDINGS'.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT BLD-MASTER.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME
           DISPLAY HEADER.
           DISPLAY SCR-TITLE.
           MOVE 'N' TO WS-EOF.
           MOVE 0 TO WS-COUNTER.
           DISPLAY WS-BLNK-LN.
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
                           ADD 1 TO WS-COUNTER
                           IF WS-COUNTER = 15
                               THEN
                                   DISPLAY WS-BLNK-LN
                                   DISPLAY WS-PG-BREAK
                                   ACCEPT WS-RESP
                                   ACCEPT WS-DATE FROM DATE
                                   ACCEPT WS-TIME FROM TIME 
                                   DISPLAY HEADER
                                   DISPLAY SCR-TITLE
                                   DISPLAY WS-BLNK-LN
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