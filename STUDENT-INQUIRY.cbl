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
           SELECT STU-MST      ASSIGN TO '../FILES/STUDENT-MASTER.DAT'
                               ORGANIZATION    IS INDEXED
                               ACCESS          IS DYNAMIC
                               RECORD      KEY IS STU-ID
                               ALTERNATE   KEY IS STU-NAME
                                   WITH DUPLICATES
                               FILE STATUS     IS WS-STU-STAT.
                                           
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
       COPY WS-DATE-TIME.
       
       01  WS-STU-STAT             PIC 99.
       01  WS-ZIP-STAT             PIC 99.
       01  WS-RESP                 PIC X       VALUE SPACES.
           88  NONE                            VALUE 'N'.
           88  ANOTHER                         VALUE 'Y'.
       
       
       SCREEN SECTION.
       COPY SCR-HEADER.
       
       01  HEADER-2.
           03  LINE 03     COL 37                  VALUE
                                                   'STUDENT INQUIRY'.
       01  SCRN-INQUIRE.
           03  SCRN-STU-ID.
               05  LINE 05 COL 25                  VALUE
                                                   'STUDENT ID    : '.
               05          COL 43      PIC 9999    USING STU-ID
                                                   REQUIRED AUTO.
           03  SCRN-STU-NAME.
               05  LINE 07 COL 25                  VALUE
                                                   'STUDENT L-NAME: '.
               05          COL 43      PIC X(15)   FROM STU-L-NAME.
               
               05  LINE 08 COL 25                  VALUE
                                                   'STUDENT F-NAME: '.
               05          COL 43      PIC X(15)   FROM STU-F-NAME.
           03  SCRN-STU-ADDR.
               05  LINE 10 COL 25                  VALUE
                                                   'STUDENT STREET: '.
               05          COL 43      PIC X(25)   FROM STU-STREET.
           
               05  LINE 11 COL 25                  VALUE
                                                   'STUDENT ZIP   : '.
               05          COL 43      PIC 99999   FROM STU-ZIP.
           
               05  LINE 13 COL 35                  VALUE 'CITY: '.
               05          COL 43      PIC X(30)   FROM ZIP-CITY.
               
               05  LINE 14 COL 35                  VALUE 'ST  : '.
               05          COL 43      PIC XX      FROM ZIP-STATE.
           03  SCRN-STU-PHONE.
               05  LINE 16 COL 25                  VALUE
                                                   'STUDENT PHONE : '.
               05          COL 43      PIC XXX     FROM STU-PHONE-1.
               05          COL 46                  VALUE '-'.
               05          COL 47      PIC XXX     FROM STU-PHONE-2.
               05          COL 50                  VALUE '-'.
               05          COL 51      PIC XXXX    FROM STU-PHONE-3.
       01  SCRN-ANOTHER.
           03      LINE 18 COL 35                  VALUE
                                       'PERFORM ANOTHER INQUIRY (Y/N)'.
           03              COL 33      PIC X       TO  WS-RESP
                                                   AUTO REQUIRED.
       01  SCRN-NOT-FOUND.
           03      LINE 11 COL 25                  VALUE
                                       'STUDENT RECORD NOT FOUND!'.

       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT  STU-MST,
                       ZIP-MST.
                       
           MOVE SPACES TO WS-RESP.
           
           PERFORM UNTIL NONE
               MOVE SPACES TO WS-RESP
               MOVE SPACES TO STU-REC
               MOVE SPACES TO ZIP-REC
               PERFORM 100-DISP-SCREEN
               PERFORM 200-GET-STUDENT
           END-PERFORM.
           
           CLOSE   STU-MST,
                   ZIP-MST.
                   
       EXIT PROGRAM.
       
       100-DISP-SCREEN.
           MOVE ZEROS TO STU-ID.
           MOVE SPACES TO ZIP-KEY.
           PERFORM 999-DISP-HEADERS.
           DISPLAY SCRN-INQUIRE.
           ACCEPT SCRN-STU-ID.
           
       200-GET-STUDENT.
           START STU-MST KEY EQUAL TO STU-ID
               INVALID KEY
                   PERFORM 999-DISP-HEADERS
                   DISPLAY SCRN-STU-ID
                   DISPLAY SCRN-NOT-FOUND
               NOT INVALID KEY
                   READ STU-MST
                   PERFORM 300-GET-CITY-ST
                   DISPLAY    SCRN-INQUIRE
           END-START.
           
           PERFORM UNTIL ANOTHER OR NONE
               DISPLAY    SCRN-ANOTHER
               ACCEPT     SCRN-ANOTHER
           END-PERFORM.
           
       300-GET-CITY-ST.
           MOVE STU-ZIP    TO  ZIP-KEY.
           START ZIP-MST KEY EQUAL TO ZIP-KEY
               INVALID KEY
                   MOVE "RECORD NOT FOUND" TO ZIP-CITY
                   MOVE SPACES TO ZIP-COUNTY
                                  ZIP-STATE
               NOT INVALID KEY
                   READ ZIP-MST
           END-START.
           
       400-DISPLAY.
           DISPLAY SCRN-INQUIRE.
           DISPLAY SCRN-ANOTHER.
           ACCEPT  SCRN-ANOTHER.
           
       999-DISP-HEADERS.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           DISPLAY HEADER,HEADER-2.    