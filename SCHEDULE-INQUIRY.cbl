      ******************************************************************
      *PROGRAM : Group Project SCHEDULE-INQUIRY                        *
      *AUTHOR  : Cory Bailey                                           *
      *DATE    : 02-03-2014                                            *
      *ABSTRACT:                                                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCHEDULE-INQUIRY.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE      ASSIGN        TO 
                                       '../FILES/SCHEDULE-MST.DAT'   
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS SEQUENTIAL 
                               RECORD KEY    IS SCHEDULE-ID-I
                               FILE STATUS   IS WS-STAT.
      *-----------------------------------------------------------------
       DATA DIVISION.  
      *-----------------------------------------------------------------
       FD  IN-FILE.
       01  IN-REC.
           03  SCHEDULE-ID-I.
               05  SCHEDULE-YEAR   PIC X(4).
               05  FILLER          PIC X.
               05  SCHEDULE-SEM    PIC XX.
               05  FILLER          PIC X.
               05  SCHEDULE-CRN    PIC X(4).
           03  FILLER              PIC X.  
           03  COURSE-ID-I.
               05  COURSE-ID-DEPT  PIC X(4).
               05  FILLER          PIC X.
               05  COURSE-ID-REST  PIC X(4).
           03  FILLER              PIC X.
           03  TIMEDAY-I           PIC X(20).
           03  FILLER              PIC X.
           03  BUILDING-ID-I       PIC X(11).
           03  FILLER              PIC X.
           03  INSTRUCTOR-ID-I     PIC X(4).
           03  FILLER              PIC X(3).
           03  OPEN-SEATS-I        PIC X(2).
       WORKING-STORAGE SECTION.
       COPY WS-COMMON.
           03  WS-COURSE-DEPT      PIC X(4).
           03  WS-YEAR             PIC 9(4).
           03  WS-SEM              PIC 99.
       01  WS-PG-BREAK.
           03  FILLER              PIC X(15) VALUE 'PRESS ENTER TO '.
           03  FILLER              PIC X(13) VALUE 'DISPLAY MORE '.
           03  FILLER              PIC X(49) VALUE 'RECORDS'.
       01  WS-HEADER.
           03  FILLER              PIC X(13) VALUE 'YEAR SEM CRN '. 
           03  FILLER              PIC X(10) VALUE 'COURSE ID'.
           03  FILLER              PIC X(21) VALUE 'TIME        DAYS'.
           03  FILLER              PIC X(12) VALUE 'BLD/ROOM'.
           03  FILLER              PIC X(5)  VALUE 'INST'.
           03  FILLER              PIC X(5)  VALUE 'SEATS'. 

       SCREEN SECTION.
       COPY SCR-COMMON.
       01  SCRN-TITLE.
           03  LINE 3  COL 35  VALUE 'SCHEDULE BY DEPARTMENT'.
       01  SCRN-DISPLAY.
           03  LINE 5  COL 10  VALUE 'YEAR:'.
           03          COL 20  PIC 9999 USING WS-YEAR
                                       AUTO REQUIRED.
           03  LINE 5  COL 30  VALUE 'SEMESTER'.
           03          COL 40  PIC 99 USING WS-SEM
                                       AUTO REQUIRED.
           03  LINE 5  COL 50  VALUE 'DEPT'.
           03          COL 60  PIC XXXX USING WS-COURSE-DEPT
                                       AUTO REQUIRED.

      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN. 
           PERFORM UNTIL ANOTHER
               MOVE ZEROS TO WS-YEAR,WS-SEM
               MOVE SPACES TO WS-COURSE-DEPT
               ACCEPT WS-TIME FROM TIME
               ACCEPT WS-DATE FROM DATE
               DISPLAY HEADER
               DISPLAY SCRN-TITLE, SCRN-DISPLAY
               ACCEPT  SCRN-DISPLAY
               PERFORM 100-LIST
               DISPLAY WS-BLNK-LN
               DISPLAY 'PRESS ENTER TO GO BACK TO MENU'
               ACCEPT WS-RESP

               DISPLAY SCRN-ANOTHER
               ACCEPT  SCRN-ANOTHER
              
           END-PERFORM.

           EXIT PROGRAM.
           
           
       100-LIST.
           OPEN INPUT IN-FILE
           MOVE 'N' TO WS-EOF.
           MOVE 0 TO WS-COUNTER.
           DISPLAY WS-BLNK-LN. 
           DISPLAY WS-HEADER.
           DISPLAY WS-BLNK-LN.
           PERFORM UNTIL EOF
               READ IN-FILE 
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END 
                       IF SCHEDULE-YEAR EQUALS WS-YEAR
                           AND SCHEDULE-SEM  EQUALS WS-SEM 
                           AND COURSE-ID-DEPT EQUALS WS-COURSE-DEPT
                       THEN
                           DISPLAY IN-REC
                           ADD 1 TO WS-COUNTER
                           IF WS-COUNTER = 10 THEN
                               DISPLAY WS-BLNK-LN
                               DISPLAY WS-PG-BREAK
                               ACCEPT WS-RESP
                               DISPLAY HEADER
                               DISPLAY SCRN-TITLE, SCRN-DISPLAY, 
                                           WS-HEADER
                               DISPLAY WS-BLNK-LN
                               MOVE 0 TO WS-COUNTER
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE IN-FILE.