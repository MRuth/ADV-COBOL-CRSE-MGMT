      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID INSTRUCTOR-INQUIRY.
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INSTR-MASTER         ASSIGN        TO 
                                       '../FILES/INSTR-MASTER.DAT'
                                       ORGANIZATION  IS INDEXED
                                       ACCESS        IS RANDOM
                                       RECORD KEY    IS INSTR-ID
                                       ALTERNATE KEY IS INSTR-NAME
                                       FILE STATUS   IS WS-STAT.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       FD  INSTR-MASTER.
       01  INSTR-REC.
           03  INSTR-ID    PIC 9999.
           03  INSTR-NAME  PIC X(22).
       WORKING-STORAGE SECTION.
       COPY WS-COMMON.
             
       01  WS-DTL.
           03  WS-INSTR-ID         PIC 9999.
           03  WS-INSTR-NAME       PIC X(22).

      *-----------------------------------------------------------------
       SCREEN SECTION.
       COPY SCR-COMMON.
       01  SCRN-TITLE.
           03  LINE 3  COL 36  VALUE 'INSTRUCTOR INQUIRY'.
       01  SCRN-ID.
            05  LINE 7  COL 33  VALUE   'INSTRUCTOR ID  :'.
            05          COL 51  PIC ZZZZ TO WS-INSTR-ID          
                                         AUTO REQUIRED.       
       01  SCRN-DATA.
           03  SCRN-INSTR-NAME.
               05  LINE 9  COL 33  VALUE   'INSTRUCTOR NAME:'.
               05          COL 51  PIC X(35) FROM WS-INSTR-NAME.

       01  SCRN-ERR.
           03  LINE 22  COL 30  VALUE 'INSTRUCTOR NOT FOUND'.    
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN. 
           OPEN INPUT INSTR-MASTER.
           
           
           MOVE 'Y' TO WS-ANOTHER.
           PERFORM UNTIL ANOTHER
           
               ACCEPT WS-DATE FROM DATE
               ACCEPT WS-TIME FROM TIME
               DISPLAY HEADER
               DISPLAY SCRN-TITLE
               DISPLAY SCRN-ID
               ACCEPT  SCRN-ID
               
               MOVE WS-INSTR-ID TO INSTR-ID
               
               READ INSTR-MASTER
                   INVALID KEY
                       DISPLAY SCRN-ERR
                       DISPLAY SCRN-ANOTHER
                       ACCEPT  SCRN-ANOTHER
                   NOT INVALID KEY
                       MOVE INSTR-NAME TO WS-INSTR-NAME
                       DISPLAY SCRN-DATA
                       DISPLAY SCRN-ANOTHER
                       ACCEPT  SCRN-ANOTHER
               END-READ
               
           END-PERFORM.
           
           CLOSE INSTR-MASTER.
           EXIT PROGRAM.
           