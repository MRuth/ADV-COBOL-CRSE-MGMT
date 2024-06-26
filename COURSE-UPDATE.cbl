      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID COURSE-UPDATE.
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CRSE-MASTER  ASSIGN  TO 
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
           03  WS-OLD-NAME         PIC X(35).
           03  WS-OLD-CREDIT       PIC X(4).
           03  WS-OLD-STAT         PIC X.
       01  WS-DTL.
           03  WS-CRSE-ID          PIC X(9).
           03  WS-CRSE-NAME        PIC X(35).
           03  WS-CRSE-CREDIT      PIC X(4).
           03  WS-CRSE-STAT        PIC X.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       COPY SCR-COMMON.
       01  SCRN-TITLE.
           03  LINE 3  COL 38  VALUE 'COURSE UPDATE'.
       01  SCRN-ID.
            05  LINE 5  COL 25  VALUE        'COURSE NUMBER:'.
            05          COL 41  PIC X(9)     TO WS-CRSE-ID          
                                             AUTO REQUIRED.       
       01  SCRN-NAME.
           03  SCRN-CRSE-OLD-NAME.
           03  SCRN-CRSE-NAME.
               05  LINE 7  COL 25  VALUE     'COURSE NAME  :'.
               05          COL 41  PIC X(35) USING WS-CRSE-NAME
                                             AUTO REQUIRED.
       01  SCRN-CREDIT.
           03  SCRN-CRSE-CREDIT.
               05  LINE 8  COL 25  VALUE     'COURSE CREDIT:'.
               05          COL 41  PIC X(4)  USING WS-CRSE-CREDIT
                                             AUTO REQUIRED.
       01  SCR-STATUS.
           03  SCRN-CRSE-STAT.
               05  LINE 9  COL 25  VALUE     'COURSE STATUS:'.
               05          COL 41  PIC X     USING WS-CRSE-STAT
                                             AUTO REQUIRED.
       01  SCRN-ERR.
           03  LINE 5  COL 30  VALUE 'RECORD NOT FOUND'.    
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       000-MAIN. 
           OPEN I-O CRSE-MASTER.
           
           MOVE 'Y' TO WS-ANOTHER.
           PERFORM UNTIL ANOTHER
                   ACCEPT WS-TIME FROM TIME
                   ACCEPT WS-DATE FROM DATE
                   DISPLAY HEADER
                   DISPLAY SCRN-TITLE
                   DISPLAY SCRN-ID
                   ACCEPT  SCRN-ID
                   MOVE WS-CRSE-ID TO CRSE-ID
                   
                   READ CRSE-MASTER
                       INVALID KEY
                           DISPLAY SCRN-ERR
                           DISPLAY SCRN-ANOTHER
                           ACCEPT SCRN-ANOTHER
                       NOT INVALID KEY
                           MOVE CRSE-NAME   TO WS-CRSE-NAME
                           MOVE CRSE-CREDIT TO WS-CRSE-CREDIT
                           MOVE CRSE-STAT   TO WS-CRSE-STAT
                           MOVE SPACE TO WS-SAVE
                           PERFORM UNTIL WS-SAVE = 'Y' OR WS-SAVE = 'N'
                               DISPLAY SCRN-NAME
                               ACCEPT SCRN-CRSE-NAME
                               DISPLAY SCRN-CREDIT
                               ACCEPT SCRN-CRSE-CREDIT
                               DISPLAY SCR-STATUS
                               ACCEPT SCRN-CRSE-STAT
                               DISPLAY SCRN-SAVE
                               ACCEPT SCRN-SAVE
                               MOVE WS-CRSE-NAME TO CRSE-NAME           
                               MOVE WS-CRSE-CREDIT TO CRSE-CREDIT
                               MOVE WS-CRSE-STAT TO CRSE-STAT
                               IF SAVE
                                   THEN
                                       REWRITE CRSE-REC
                                       DISPLAY SCRN-SAVED
                                       DISPLAY SCRN-ANOTHER
                                       ACCEPT SCRN-ANOTHER
                               ELSE
                                   DISPLAY SCRN-CANCEL
                                   DISPLAY SCRN-ANOTHER
                                   ACCEPT SCRN-ANOTHER
                               END-IF
                           END-PERFORM             
                   END-READ
           END-PERFORM.

           CLOSE CRSE-MASTER.
           EXIT PROGRAM.