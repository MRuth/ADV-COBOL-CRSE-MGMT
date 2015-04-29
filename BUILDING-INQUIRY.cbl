       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUILDING-INQUIRY.
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
       01  WS-BLD.
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
           03  LINE 3  COL 37  VALUE 'INQUIRY BUILDING'.
       01  SCRN-BLD-ROOM. 
           03  SCRN-BLD-NAME.
               05  LINE 8  COL 33  VALUE   'BUILDING NAME: '.
               05          COL 50  PIC X(8) TO WS-BLD-NAME          
                                            AUTO REQUIRED.
           03  SCRN-ROOM-NO.
               05  LINE 10  COL 33  VALUE   'ROOM NUMBER  : '.
               05          COL 50  PIC X(4) TO WS-ROOM-NO
                                             AUTO REQUIRED.
       01  SCRN-DATA.
           03  LINE 12  COL 33  VALUE   'MAX SEATS    :'.
           03          COL 50  PIC Z9  FROM WS-MAX-SEAT.
       01  SCRN-ERR.
           03  LINE 12  COL 38  VALUE 'ROOM NOT FOUND'.    

      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT BLD-MASTER.

           MOVE 'Y' TO WS-ANOTHER.
           PERFORM UNTIL ANOTHER
                   ACCEPT WS-DATE FROM DATE 
                   ACCEPT WS-TIME FROM TIME
                   DISPLAY HEADER
                   DISPLAY SCRN-TITLE
                   DISPLAY SCRN-BLD-ROOM
                   
                   ACCEPT  SCRN-BLD-NAME
                   ACCEPT  SCRN-ROOM-NO
                   
                   STRING
                       WS-BLD-NAME DELIMITED BY SPACE
                       WS-SPACE    DELIMITED BY SIZE
                       WS-ROOM-NO  DELIMITED BY SPACE
                       INTO WS-BLD-ROOM
                   MOVE WS-BLD-ROOM TO BLD-BUILDING-ROOM

                   READ BLD-MASTER
                       INVALID KEY
                           DISPLAY SCRN-ERR
                           DISPLAY SCRN-ANOTHER
                           ACCEPT SCRN-ANOTHER
                       NOT INVALID KEY
                           MOVE BLD-MAX-SEAT TO WS-MAX-SEAT
                           DISPLAY SCRN-DATA
                           DISPLAY SCRN-ANOTHER
                           ACCEPT SCRN-ANOTHER
                   END-READ
           END-PERFORM.
           
           CLOSE BLD-MASTER.           

           EXIT PROGRAM.