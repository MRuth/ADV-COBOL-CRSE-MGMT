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
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-ANOTHER          PIC X   VALUE 'Y'.
               88  ANOTHER                 VALUE 'N'.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
           03  WS-SAVE             PIC X   VALUE 'N'.
               88  SAVE                    VALUE 'Y'.     
       01  WS-DTL.
           03  WS-CRSE-ID          PIC X(9).
           03  WS-CRSE-NAME        PIC X(35).
           03  WS-CRSE-CREDIT      PIC X(4).
           03  WS-CRSE-STAT        PIC X.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  BLNK-SCRN.
           03  BLANK SCREEN.
       01  SCRN-TITLE.
           03  LINE 1  COL 30  VALUE 'COURSE UPDATE'.
       01  SCRN-ID.
            05  LINE 3  COL 25  VALUE   'COURSE NUMBER:'.
            05          COL 40  PIC X(9) TO WS-CRSE-ID          
                                         AUTO REQUIRED.       
       01  SCRN-DATA.
           03  SCRN-CRSE-NAME.
               05  LINE 4  COL 25  VALUE   'NEW NAME:'.
               05          COL 40  PIC X(35) TO WS-CRSE-NAME
                                                AUTO REQUIRED.
           03  SCRN-CRSE-CREDIT.
               05  LINE 5  COL 25  VALUE   'NEW CREDIT:'.
               05          COL 40  PIC X(4) TO WS-CRSE-CREDIT
                                               AUTO REQUIRED.
           03  SCRN-CRSE-STAT.
               05  LINE 6  COL 25  VALUE   'COURSE STAT:'.
               05          COL 40  PIC X    TO WS-CRSE-STAT
                                               AUTO REQUIRED. 
           03  SCRN-SAVE.
               05  LINE 7  COL 32  VALUE   'SAVE (Y/N)'.
               05          COL 30  PIC X    TO WS-SAVE
                                               REQUIRED.
       01  SCRN-CONFIRM1.
           03  LINE 8  COL 30  VALUE 'RECORD IS UPDATED'.
       01  SCRN-CONFIRM2.
           03  LINE 8  COL 30  VALUE 'RECORD IS NOT UPDATED'.                                                                              
       01  SCRN-ANOTHER.
           03  LINE 9  COL 32  VALUE 'ENTER ANOTHER? (Y/N)'.
           03          COL 30  PIC X TO WS-ANOTHER.
       01  SCRN-ERR.
           03  LINE 8  COL 30  VALUE 'RECORD NOT FOUND'.    
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       000-MAIN. 
           OPEN I-O CRSE-MASTER.

           PERFORM UNTIL ANOTHER
           
               DISPLAY BLNK-SCRN
               DISPLAY SCRN-TITLE
               DISPLAY SCRN-ID
               ACCEPT  SCRN-ID
               
               MOVE WS-CRSE-ID TO CRSE-ID
               
               READ CRSE-MASTER
                   INVALID KEY
                       DISPLAY BLNK-SCRN
                       DISPLAY SCRN-ERR
                       DISPLAY SCRN-ANOTHER
                       ACCEPT SCRN-ANOTHER
                   NOT INVALID KEY
                       DISPLAY SCRN-DATA
                       ACCEPT SCRN-CRSE-NAME
                       ACCEPT SCRN-CRSE-CREDIT
                       ACCEPT SCRN-CRSE-STAT
                       ACCEPT SCRN-SAVE
                       IF SAVE
                           THEN                                         
                               MOVE WS-CRSE-NAME TO CRSE-NAME           
                               MOVE WS-CRSE-CREDIT TO CRSE-CREDIT
                               MOVE WS-CRSE-STAT TO CRSE-STAT
                               REWRITE CRSE-REC
                               DISPLAY BLNK-SCRN
                               DISPLAY SCRN-CONFIRM1
                               DISPLAY SCRN-ANOTHER
                               ACCEPT SCRN-ANOTHER
                       ELSE
                           DISPLAY BLNK-SCRN
                           DISPLAY SCRN-CONFIRM2
                           DISPLAY SCRN-ANOTHER
                           ACCEPT SCRN-ANOTHER
                       END-IF    
               END-READ
           END-PERFORM.

           CLOSE CRSE-MASTER.
           STOP RUN.