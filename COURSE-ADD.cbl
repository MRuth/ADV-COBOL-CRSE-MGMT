      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID COURSE-ADD.
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CRSE-MASTER  ASSIGN        TO 
                                       '../FILES/COURSE-MASTER-SORT.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS CRSE-ID
                               FILE STATUS   IS WS-STAT.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       FD  CRSE-MASTER.
       01  CRSE-REC.
           03  CRSE-ID        PIC X(9).
           03  CRSE-NAME      PIC X(35).
           03  CRSE-CREDIT    PIC X(4).
           03  CRSE-STAT      PIC X.
       WORKING-STORAGE SECTION.
       COPY WS-COMMON.   
       01  WS-DTL-LN.
           03  WS-COURSE-ID        PIC X(9).
           03  WS-COURSE-NAME      PIC X(35).
           03  FILLER              PIC X VALUE SPACE.
           03  WS-COURSE-CREDIT    PIC X(4).
           03  WS-COURSE-STAT      PIC X.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       COPY SCR-COMMON.
       01  SCRN-TITLE.
           03  LINE 3  COL 47  VALUE 'ADD COURSE'.
       01  SCRN-DATA. 
           03  SCRN-CRSE-ID.
               05  LINE 7  COL 25  VALUE   'COURSE ID:'.
               05          COL 45  PIC X(9) USING WS-COURSE-ID          
                                            AUTO REQUIRED.
           03  SCRN-CRSE-NAME.
               05  LINE 9  COL 25  VALUE   'COURSE DESCRIPTION:'.
               05          COL 45  PIC X(35) USING WS-COURSE-NAME 
                                            AUTO REQUIRED.
           03  SCRN-CRSE-CREDIT. 
               05  LINE 11  COL 25  VALUE   'COURSE CREDIT:'.
               05          COL 45  PIC X(4) USING WS-COURSE-CREDIT 
                                            AUTO REQUIRED.

       01  SCRN-WRITE-ERR.
           03  LINE 5  COL 30  VALUE 'COURSE IS ALREADY EXIST'.
       01  SCRN-WRITE-SUC.
           03  LINE 5  COL 30  VALUE 'COURSE IS ADDED'.
       01  SCRN-WRITE-NOT-SAVE.
           03  LINE 5  COL 30  VALUE 'COURSE IS NOT ADDED'.           

      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
       
           OPEN I-O CRSE-MASTER.
           
           MOVE 'Y' TO WS-ANOTHER.
           PERFORM UNTIL ANOTHER
               MOVE SPACES TO WS-COURSE-ID
               MOVE SPACES TO WS-COURSE-NAME
               MOVE SPACES TO WS-COURSE-CREDIT
               MOVE SPACE TO WS-SAVE
               PERFORM UNTIL WS-SAVE = 'Y' OR WS-SAVE = 'N'
                   ACCEPT WS-DATE FROM DATE
                   ACCEPT WS-TIME FROM TIME
                   DISPLAY HEADER
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
                           MOVE WS-COURSE-ID TO CRSE-ID
                           MOVE WS-COURSE-NAME TO CRSE-NAME
                           MOVE WS-COURSE-CREDIT TO CRSE-CREDIT
                           MOVE 'A' TO CRSE-STAT
                           WRITE CRSE-REC
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
           
           CLOSE CRSE-MASTER.
           
           EXIT PROGRAM.
           