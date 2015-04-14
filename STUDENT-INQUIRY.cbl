	  ******************************************************************
	  *PROGRAM :  STUDENT-INQUIRY                                      *
	  *AUTHOR  : MONTANA RUTH                                          *
	  *DATE    : Apr 14, 2015                                          *
	  *ABSTRACT:                                                       *
	  ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-INQUIRY.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STU-MST      ASSIGN TO '../FILES/SUDENT-MASTER.DAT'
                               ORGANIZATION IS INDEXED
                               RECORD KEY IS STU-ID
                               FILE STATUS IS WS-STU-STAT.
                                           
           SELECT ZIP-MST      ASSIGN TO "../FILES/ZIPMASTER.DAT"
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS DYNAMIC
                               RECORD KEY    IS ZIP-KEY
                               ALTERNATE KEY IS ZIP-CITY
                                   WITH DUPLICATES
                               FILE STATUS   IS WS-ZIP-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY STU-MST-DEF.
       COPY ZIP-MST-DEF.
       
       WORKING-STORAGE SECTION.
       01  WS-STU-STAT             PIC 99.
       01  WS-ZIP-STAT             PIC 99.
       01  WS-RESP                 PIC X       VALUE 'Y'.
           88  ANOTHER                         VALUE 'N'.
       
       
       SCREEN SECTION.
       01  NEW-SCREEN.
           03  BLANK SCREEN.
           03  LINE 2  COL 34                  VALUE 'STUDENT INQUIRY'.
       01  SCRN-INQUIRE.
           03  SCRN-STU-ID.
               05  LINE 4  COL 25                  VALUE
                                                   'STUDENT ID    : '.
               05          COL 43      PIC 9999    USING STU-ID
                                                   REQUIRED AUTO.
           03  SCRN-STU-NAME.
               05  LINE 6  COL 25                  VALUE
                                                   'STUDENT L-NAME: '.
               05          COL 43      PIC X(15)   FROM STU-L-NAME.
               
               05  LINE 7  COL 25                  VALUE
                                                   'STUDENT F-NAME: '.
               05          COL 43      PIC X(15)   FROM STU-F-NAME.
           03  SCRN-STU-ADDR.
               05  LINE 9  COL 25                  VALUE
                                                   'STUDENT STREET: '.
               05          COL 43      PIC X(25)   FROM STU-STREET.
           
               05  LINE 10 COL 25                  VALUE
                                                   'STUDENT ZIP   : '.
               05          COL 43      PIC 99999   FROM STU-ZIP.
           
               05  LINE 12 COL 35                  VALUE 'CITY: '.
               05          COL 43      PIC X(30)   FROM ZIP-CITY.
               
               05  LINE 13 COL 35                  VALUE 'ST  : '.
               05          COL 43      PIC XX      FROM ZIP-STATE.
           03  SCRN-STU-PHONE.
               05  LINE 15 COL 25                  VALUE
                                                   'STUDENT PHONE : '.
               05          COL 43      PIC X(10)   FROM STU-PHONE.
       01  SCRN-ANOTHER.
           03      LINE 18 COL 35                  VALUE
                                       'PERFORM ANOTHER INQUIRY (Y/N)'.
           03              COL 33      PIC X       TO  WS-RESP
                                                   AUTO REQUIRED.

       PROCEDURE DIVISION.
       000-MAIN.
           MOVE 'Y'    TO WS-RESP.
           
           PERFORM UNTIL ANOTHER
               MOVE ZEROS TO STU-ID
               MOVE SPACES TO ZIP-KEY
               DISPLAY NEW-SCREEN
               DISPLAY SCRN-INQUIRE
               ACCEPT SCRN-STU-ID
               DISPLAY SCRN-ANOTHER
               ACCEPT SCRN-ANOTHER
           END-PERFORM.
       EXIT PROGRAM.
       
       100-GET-STUDENT.
       200-GET-CITY-ST.
       
