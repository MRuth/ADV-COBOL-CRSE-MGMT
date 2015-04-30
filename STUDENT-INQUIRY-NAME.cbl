	  ******************************************************************
	  *PROGRAM :  STUDENT-INQUIRY-NAME                                 *
	  *AUTHOR  : MONTANA RUTH                                          *
	  *DATE    : Apr 23, 2015                                          *
	  *ABSTRACT:                                                       *
	  ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-INQUIRY-NAME.
       
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

       DATA DIVISION.
       FILE SECTION.
       COPY STU-MST-DEF.
       
       WORKING-STORAGE SECTION.
       COPY WS-DATE-TIME.
       01  WS-CTR                  PIC 99      VALUE 01.
       01  WS-STU-STAT             PIC 99.
       01  WS-ROW                  PIC 99      VALUE 6.
       01  WS-STU-NAME.
           03  WS-STU-L-NAME       PIC X(15).
           03  WS-STU-F-NAME       PIC X(15).
       01  WS-RESP                 PIC X.
           88  ANOTHER                         VALUE 'Y'.
           88  NOT-ANOTHER                     VALUE 'N'.
       01  WS-RESP-2               PIC X.
           88  NO-CONTINUE                     VALUE 'X'.
       01  WS-EOF                  PIC X       VALUE 'N'.
           88  EOF                             VALUE 'Y'.
       SCREEN SECTION.
       COPY SCR-HEADER.
       01  HEADER-2.
           03  LINE 03     COL 35              VALUE
                                               'STUDENT NAME INQUIRY'.
       01  HEADER-3.
           03  LINE 05     COL 01  VALUE 'ID'.
           03              COL 06  VALUE 'LAST NAME'.
           03              COL 22  VALUE 'FIRST NAME'.
           03              COL 38  VALUE 'STREET'.
           03              COL 64  VALUE 'ZIP'.
           03              COL 70  value 'PHONE'.
       01  SCRN-NAME.
           03  SCRN-STU-L-NAME.
               05  LINE 05 COL 25              VALUE                        
                                               'STUDENT L-NAME'.        
               05          COL 43  PIC X(15)   TO WS-STU-L-NAME. 
       01  SCRN-LIST.
           
       01  SCRN-ANOTHER.
           03      LINE 14 COL 35                  VALUE
                                       'PERFORM ANOTHER INQUIRY (Y/N)'.
           03              COL 33      PIC X       TO  WS-RESP
                                                   AUTO REQUIRED.

       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT STU-MST.
           
           MOVE SPACES TO WS-RESP.
           
           PERFORM UNTIL NOT-ANOTHER
               MOVE SPACES TO WS-RESP
               MOVE SPACES TO WS-STU-NAME
               MOVE SPACES TO WS-EOF
               MOVE SPACES TO WS-RESP-2
               
               PERFORM 999-DISP-HEADERS
               DISPLAY SCRN-NAME
               ACCEPT  SCRN-STU-L-NAME
               PERFORM 200-SEARCH-STUDENTS
               
               IF NOT NO-CONTINUE THEN
                   DISPLAY SPACES
                   DISPLAY 'PRESS ENTER TO CONTINUE'
                   ACCEPT WS-RESP-2
               END-IF
               
               PERFORM UNTIL ANOTHER OR NOT-ANOTHER
                   PERFORM 999-DISP-HEADERS
                   DISPLAY SCRN-ANOTHER
                   ACCEPT  SCRN-ANOTHER
               END-PERFORM
           END-PERFORM.
           
           CLOSE STU-MST.
           
           EXIT PROGRAM.
       
       200-SEARCH-STUDENTS.
           MOVE WS-STU-NAME TO STU-NAME.
           START STU-MST KEY NOT LESS THAN STU-NAME
               NOT INVALID KEY
                   MOVE 1 TO WS-CTR
                   PERFORM 999-DISP-HEADERS
                   DISPLAY HEADER-3
                   DISPLAY SPACES
                   DISPLAY SPACES
                   PERFORM 300-DISPLAY-STUDENTS UNTIL EOF OR NO-CONTINUE
           END-START.
       300-DISPLAY-STUDENTS.
           READ STU-MST NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   ADD 1 TO WS-CTR
                   IF WS-CTR > 15 THEN
                      DISPLAY SPACES
                      DISPLAY 'PRESS ENTER TO CONTINUE OR X TO EXIT'
                      ACCEPT WS-RESP-2
                      PERFORM 999-DISP-HEADERS
                      DISPLAY HEADER-3
                      DISPLAY SPACES
                      DISPLAY SPACES
                      MOVE 1 TO WS-CTR                                 
                   END-IF
               IF NOT NO-CONTINUE THEN                                  
                   DISPLAY STU-ID, " ", STU-L-NAME, " ", STU-F-NAME," ",
                   STU-STREET, " ", STU-ZIP, " ", STU-PHONE
               END-IF
           END-READ.
           
           
       
       999-DISP-HEADERS.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           DISPLAY HEADER,HEADER-2.
