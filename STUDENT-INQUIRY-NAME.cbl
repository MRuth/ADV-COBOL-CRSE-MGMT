	  ******************************************************************
	  *PROGRAM :  STUDENT-INQUIRY-NAME                                 *
	  *AUTHOR  : MONTANA RUTH                                          *
	  *DATE    : Apr 23, 2015                                          *
	  *ABSTRACT:                                                       *
	  ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-INQUIRY-NAME.
       
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
       COPY WS-DATE-TIME.
       01  WS-STU-NAME.
           03  WS-STU-L-NAME       PIC X(15).
           03  WS-STU-F-NAME       PIC X(15).
       01  WS-RESP                 PIC X.
           88  ANOTHER                         VALUE 'Y'.
           88  NOT-ANOTHER                     VALUE 'N'.
       SCREEN SECTION.
       COPY SCR-HEADER.
       01  HEADER-2.
           03  LINE 03     COL 35              VALUE
                                               'STUDENT NAME INQUIRY'.
       01  SCRN-NAME.
           03  LINE 05     COL 25              VALUE                    
                                               'STUDENT L-NAME'.        
           03              COL 43  PIC X(15)   TO WS-STU-L-NAME. 
           03  LINE 06     COL 25              VALUE
                                               'STUDENT F-NAME'.
           03              COL 43  PIC X(15)   TO WS-STU-F-NAME.
           
       01  SCRN-ANOTHER.
           03      LINE 14 COL 35                  VALUE
                                       'PERFORM ANOTHER INQUIRY (Y/N)'.
           03              COL 33      PIC X       TO  WS-RESP
                                                   AUTO REQUIRED.

       PROCEDURE DIVISION.
       000-MAIN.
           
           MOVE SPACES TO WS-RESP.
           
           PERFORM UNTIL NOT-ANOTHER
               MOVE SPACES TO WS-RESP
               MOVE SPACES TO WS-STU-NAME
               
               PERFORM 999-DISP-HEADERS
               DISPLAY SCRN-NAME
               ACCEPT  SCRN-NAME
               PERFORM 200-SEARCH-STUDENTS
               
               PERFORM UNTIL ANOTHER OR NOT-ANOTHER
                   PERFORM 999-DISP-HEADERS
                   DISPLAY SCRN-ANOTHER
                   ACCEPT  SCRN-ANOTHER
               END-PERFORM
           END-PERFORM.
           EXIT PROGRAM.
       
       200-SEARCH-STUDENTS.
       
       999-DISP-HEADERS.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           DISPLAY HEADER,HEADER-2.
