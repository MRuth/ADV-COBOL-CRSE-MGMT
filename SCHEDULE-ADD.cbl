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
           03  WS-COURSE-ID        PIC X(9).
           03  WS-COURSE-NAME      PIC X(35).
           03  WS-COURSE-CREDIT    PIC X(4).
           03  WS-COURSE-STAT      PIC X.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  BLNK-SCRN.
           03  BLANK SCREEN.
       01  SCRN-TITLE.
           03  LINE 1  COL 30  VALUE 'ADD COURSE'.
       01  SCRN-DATA.
           03  SCRN-CRSE-ID.
               05  LINE 3  COL 25  VALUE   'COURSE NUMBER:'.
               05          COL 40  PIC X(9) USING WS-COURSE-ID          
                                            AUTO REQUIRED.
           03  SCRN-CRSE-NAME.
               05  LINE 4  COL 25  VALUE   'COURSE NAME  :'.
               05          COL 40  PIC X(35) USING WS-COURSE-NAME 
                                             AUTO REQUIRED.
           03  SCRN-CRSE-CREDIT.
               05  LINE 5  COL 25  VALUE   'COURSE CREDIT:'.
               05          COL 40  PIC X(4) USING WS-COURSE-CREDIT 
                                            AUTO REQUIRED.
           03  SCRN-SAVE.
               05  LINE 7  COL 32  VALUE   'SAVE (Y/N)'.
               05          COL 30  PIC X     TO WS-SAVE.
       01  SCRN-WRITE-ERR.
           03  LINE 5  COL 30  VALUE 'COURSE IS ALREADY EXIST'.
       01  SCRN-WRITE-SUC.
           03  LINE 5  COL 30  VALUE 'COURSE IS ADDED'.
       01  SCRN-WRITE-NOT-SAVE.
           03  LINE 5  COL 30  VALUE 'COURSE IS NOT ADDED'.           
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
                   
                   ACCEPT  SCRN-CRSE-ID
                   ACCEPT  SCRN-CRSE-NAME
                   ACCEPT  SCRN-CRSE-CREDIT
                   
                   DISPLAY SCRN-SAVE
                   ACCEPT  SCRN-SAVE
               END-PERFORM          
                   IF SAVE
                       THEN

                           WRITE OUT-REC
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
           
           CLOSE SCHED-MST.
           
           EXIT PROGRAM.
           