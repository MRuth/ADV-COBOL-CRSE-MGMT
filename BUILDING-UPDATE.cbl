       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUILDING-UPDATE.
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
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
           03  WS-SAVE             PIC X   VALUE 'N'.
               88  SAVE                    VALUE 'Y'.
           03  WS-ANOTHER          PIC X   VALUE 'Y'.
               88  ANOTHER                 VALUE 'N'.
           03  WS-BLD-ROOM         PIC X(12).
       01  WS-DTL-LN.
           03  WS-BLD-NAME         PIC X(8).
           03  WS-SPACE            PIC X VALUE SPACE.
           03  WS-ROOM-NO          PIC X(4).
           03  WS-MAX-SEAT         PIC 99.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  BLNK-SCRN.
           03  BLANK SCREEN.
       01  SCRN-TITLE.
           03  LINE 1  COL 30  VALUE 'ADD BUILDING'.
       01  SCRN-DATA.
           03  SCRN-BLD-NAME.
               05  LINE 3  COL 25  VALUE   'BUILDING NAME: '.
               05          COL 40  PIC X(8) TO WS-BLD-NAME          
                                            AUTO REQUIRED.
           03  SCRN-ROOM-NO.
               05  LINE 4  COL 25  VALUE   'ROOM NUMBER  : '.
               05          COL 40  PIC X(4) TO WS-ROOM-NO
                                             AUTO REQUIRED.
           03  SCRN-MAX-SEAT.
               05  LINE 5  COL 25  VALUE   'MAX SEAT     : '.
               05          COL 40  PIC Z9  TO WS-MAX-SEAT
                                            AUTO REQUIRED.
           03  SCRN-SAVE.
               05  LINE 7  COL 32  VALUE   'SAVE (Y/N)'.
               05          COL 30  PIC X     TO WS-SAVE.
       01  SCRN-WRITE-ERR.
           03  LINE 1  COL 30  VALUE 'ROOM IS ALREADY EXIST'.
       01  SCRN-WRITE-SUC.
           03  LINE 1  COL 30  VALUE 'ROOM IS ADDED'.
       01  SCRN-WRITE-NOT-SAVE.
           03  LINE 1  COL 30  VALUE 'ROOM IS NOT ADDED'.           
       01  SCRN-ANOTHER.
           03  LINE 3  COL 32  VALUE 'ADD ANOTHER? (Y/N)'.
           03          COL 30  PIC X TO WS-ANOTHER.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN I-O BLD-MASTER.

           MOVE 'Y' TO WS-ANOTHER.
           PERFORM UNTIL ANOTHER
                   DISPLAY BLNK-SCRN
                   DISPLAY SCRN-TITLE
                   DISPLAY SCRN-DATA
                   
                   ACCEPT  SCRN-BLD-NAME
                   ACCEPT  SCRN-ROOM-NO
                   ACCEPT  SCRN-MAX-SEAT
                   
                   DISPLAY SCRN-SAVE
                   ACCEPT  SCRN-SAVE
                   
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
                                   DISPLAY BLNK-SCRN
                                   DISPLAY SCRN-WRITE-ERR
                                   DISPLAY SCRN-ANOTHER
                                   ACCEPT  SCRN-ANOTHER
                               NOT INVALID KEY
                                   DISPLAY BLNK-SCRN
                                   DISPLAY SCRN-WRITE-SUC
                                   DISPLAY SCRN-ANOTHER
                                   ACCEPT  SCRN-ANOTHER
                   ELSE 
                       DISPLAY BLNK-SCRN
                       DISPLAY SCRN-WRITE-NOT-SAVE
                       DISPLAY SCRN-ANOTHER
                       ACCEPT SCRN-ANOTHER
                   END-IF
           END-PERFORM.
           
           CLOSE BLD-MASTER.           

           EXIT PROGRAM.