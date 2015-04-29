       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUILDING-ADD.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BLD-MASTER   ASSIGN        TO 
                               '../FILES/BUILDING-MASTER.DAT'   
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
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
           03  WS-BLD-ROOM         PIC X(12).
       01  WS-DTL-LN.
           03  WS-BLD-NAME         PIC X(8).
           03  WS-SPACE            PIC X VALUE SPACE.
           03  WS-ROOM-NO          PIC X(4).
           03  WS-MAX-SEAT         PIC 99.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       COPY SCR-COMMON.
       01  SCRN-TITLE.
           03  LINE 3  COL 38  VALUE 'ADD BUILDING'.
       01  SCRN-DATA.
           03  SCRN-BLD-NAME.
               05  LINE 7  COL 30  VALUE   'BUILDING NAME: '.
               05          COL 45  PIC X(8) USING WS-BLD-NAME          
                                            AUTO REQUIRED.
           03  SCRN-ROOM-NO.
               05  LINE 9  COL 30  VALUE   'ROOM NUMBER  : '.
               05          COL 45  PIC X(4) USING WS-ROOM-NO
                                             AUTO REQUIRED.
           03  SCRN-MAX-SEAT.
               05  LINE 11  COL 30  VALUE   'MAX SEAT     : '.
               05          COL 45  PIC Z9  USING WS-MAX-SEAT
                                            AUTO REQUIRED.
       
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN I-O BLD-MASTER.

           MOVE 'Y' TO WS-ANOTHER.
           PERFORM UNTIL ANOTHER
               MOVE SPACE TO WS-SAVE
               PERFORM UNTIL WS-SAVE = 'Y' OR WS-SAVE = 'N'
                   ACCEPT WS-DATE FROM DATE 
                   ACCEPT WS-TIME FROM TIME
                   DISPLAY HEADER
                   DISPLAY SCRN-TITLE
                   DISPLAY SCRN-DATA
                   ACCEPT  SCRN-BLD-NAME
                   ACCEPT  SCRN-ROOM-NO
                   ACCEPT  SCRN-MAX-SEAT
                   DISPLAY SCRN-SAVE
                   ACCEPT  SCRN-SAVE
               END-PERFORM                  
               IF SAVE
                   THEN
                       STRING
                           WS-BLD-NAME DELIMITED BY SPACE
                           WS-SPACE    DELIMITED BY SIZE
                           WS-ROOM-NO  DELIMITED BY SPACE
                           INTO WS-BLD-ROOM
                           MOVE WS-BLD-ROOM TO BLD-BUILDING-ROOM
                           MOVE WS-MAX-SEAT TO BLD-MAX-SEAT
                       WRITE BLD-REC
                           INVALID KEY
                               DISPLAY SCRN-SAVE-ERROR
                               DISPLAY SCRN-ANOTHER
                               ACCEPT  SCRN-ANOTHER
                           NOT INVALID KEY
                               DISPLAY SCRN-SAVED
                               DISPLAY SCRN-ANOTHER
                               ACCEPT  SCRN-ANOTHER
               ELSE
                   DISPLAY SCRN-CANCEL
                   DISPLAY SCRN-ANOTHER
                   ACCEPT SCRN-ANOTHER
               END-IF
           END-PERFORM.
           
           CLOSE BLD-MASTER.           

           EXIT PROGRAM.