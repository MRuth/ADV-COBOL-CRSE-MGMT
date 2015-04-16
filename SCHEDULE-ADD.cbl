      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID SCHEDULE-ADD.
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SCHED-MST    ASSIGN        TO 
                                       '../FILES/SCHEDULE-MST.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS SCHEDULE-ID-O
                               FILE STATUS   IS WS-STAT.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       FD  SCHED-MST.
       01  OUT-REC.
           03  SCHEDULE-ID-O       PIC X(12).
           03  FILLER              PIC X.
           03  COURSE-ID-O         PIC X(9).
           03  FILLER              PIC X.
           03  TIMEDAY-O           PIC X(20).
           03  FILLER              PIC X.
           03  BUILDING-ID-O       PIC X(11).
           03  FILLER              PIC X.
           03  INSTRUCTOR-ID-O     PIC X(4).
           03  FILLER              PIC X(3).
           03  OPEN-SEATS-O        PIC X(2).
       WORKING-STORAGE SECTION.
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
           03  WS-SAVE             PIC X   VALUE SPACE.
               88  SAVE                    VALUE 'Y'.
           03  WS-ANOTHER          PIC X   VALUE 'Y'.
               88  ANOTHER                 VALUE 'N'.
           03  WS-VALIDATE         PIC X.     
       01  WS-DTL-LN.
           03  WS-COURSE-ID.
               05  WS-COURSE-DEPT  PIC X(4).
               05  FILLER          PIC X.
               04  WS-COURSE-NUM   PIC X(4).
           03  WS-YEAR             PIC 9(4).
           03  WS-SEM              PIC 99.
           03  WS-TIMEDAY          PIC X(20).
           03  WS-BUILDING         PIC X(11).
           03  WS-INST-ID          PIC 9(4).
           03  WS-SEATS            PIC 99.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  BLNK-SCRN.
           03  BLANK SCREEN.
       01  SCRN-TITLE.
           03  LINE 1  COL 30  VALUE 'ADD CLASS TO SCHEULE'.
       01  SCRN-DATA.
           03  SCRN-YEAR.
               05  LINE 3  COL 25  VALUE   'YEAR:'.
               05          COL 40  PIC X(4) USING WS-YEAR          
                                            AUTO REQUIRED.
           03  SCRN-SEM.
               05  LINE 3  COL 50  VALUE   'SEM  :'.
               05          COL 60  PIC 99 USING WS-SEM 
                                             AUTO REQUIRED.
           03  SCRN-CRSE-DEPT.
               05  LINE 5  COL 25  VALUE   'COURSE DEPT:'.
               05          COL 40  PIC X(4) USING WS-COURSE-DEPT 
                                            AUTO REQUIRED.
           03  SCRN-CRSE-ID.
               05  LINE 5  COL 50  VALUE   'COURSE ID:'. 
               05          COL 60  PIC X(4) USING WS-COURSE-NUM
                                            AUTO REQUIRED.
           03  SCRN-TIME.
               05  LINE 7  COL 25  VALUE   'TIME / DAY:'.
               05          COL 40  PIC X(20) USING WS-TIMEDAY
                                            AUTO REQUIRED.
                                            
           03  SCRN-BUILD.
               05  LINE 9  COL 25  VALUE   'BUILDING:'.
               05          COL 40  PIC X(12) USING WS-BUILDING
                                            AUTO REQUIRED.
           03  SCRN-INST.
               05  LINE 11 COL 25  VALUE   'INSTRUCTOR ID:'.
               05          COL 40  PIC 9999 USING WS-INST-ID
                                            AUTO REQUIRED.
           03  SCRN-SEATS.
               05  LINE 9  COL 50  VALUE   'MAX SEATS:'.
               05          COL 60  PIC 99   USING WS-SEATS 
                                            AUTO REQUIRED.
          
       01  SCRN-SV.
           03  SCRN-SAVE.
               05  LINE 15  COL 32  VALUE   'SAVE (Y/N)'.
               05          COL 30  PIC X     TO WS-SAVE.
       01  SCRN-ANOTHER.
           03  LINE 7  COL 32  VALUE 'ADD ANOTHER? (Y/N)'.
           03          COL 30  PIC X TO WS-ANOTHER.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
       
           OPEN I-O SCHED-MST.
           
           MOVE 'Y' TO WS-ANOTHER.
           PERFORM UNTIL ANOTHER
               PERFORM UNTIL WS-SAVE = 'Y' OR WS-SAVE = 'N'
                   DISPLAY BLNK-SCRN
                   DISPLAY SCRN-TITLE
                   DISPLAY SCRN-DATA
                   
                   ACCEPT  SCRN-YEAR
                   ACCEPT  SCRN-SEM
                   ACCEPT  SCRN-CRSE-DEPT
                   ACCEPT  SCRN-CRSE-ID
                   ACCEPT  SCRN-TIME
                   ACCEPT  SCRN-BUILD
                   ACCEPT  SCRN-INST
                   ACCEPT  SCRN-SEATS
                   
                   DISPLAY SCRN-SAVE
                   ACCEPT  SCRN-SAVE
               END-PERFORM          
                   IF SAVE
                       THEN

                           WRITE OUT-REC
                               INVALID KEY
                                   DISPLAY BLNK-SCRN
                                   DISPLAY SCRN-ANOTHER
                                   ACCEPT  SCRN-ANOTHER
                               NOT INVALID KEY
                                   DISPLAY BLNK-SCRN
                                   DISPLAY SCRN-ANOTHER
                                   ACCEPT  SCRN-ANOTHER
                   ELSE 
                       DISPLAY BLNK-SCRN
                       
                       DISPLAY SCRN-ANOTHER
                       ACCEPT SCRN-ANOTHER
                   END-IF
           END-PERFORM.
           
           CLOSE SCHED-MST.
           
           EXIT PROGRAM.
           