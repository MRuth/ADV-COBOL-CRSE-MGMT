      ******************************************************************
      *PROGRAM :  STU-LIST                                             *
      *AUTHOR  : MONTANA RUTH                                          *
      *DATE    :    Mar 17, 2015                                       *
      *ABSTRACT:                                                       *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STU-LIST.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
           SELECT STU-MST ASSIGN TO'../FILES/STUDENT-MASTER.DAT'
               ORGANIZATION IS INDEXED
               ACCESS          IS SEQUENTIAL
               RECORD      KEY IS STU-ID
               ALTERNATE   KEY IS STU-NAME.
   
       DATA DIVISION.
       FILE SECTION.
       
       COPY STU-MST-DEF.   
       
       WORKING-STORAGE SECTION.
       COPY WS-DATE-TIME.
       01  WS-CTR                      PIC 99      VALUE 0.
       01  WS-RESP                     PIC X.
       01  WS-EOF                      PIC X       VALUE 'N'.
           88  EOF                                 VALUE 'Y'.
       
       SCREEN SECTION.
       COPY SCR-HEADER.
       01  HEADER-2.
           03  LINE 03 COL 37  VALUE 'STUDENT LISTING'.
           03  LINE 05 COL 01  VALUE 'ID'.
           03          COL 06  VALUE 'LAST NAME'.
           03          COL 22  VALUE 'FIRST NAME'.
           03          COL 38  VALUE 'STREET'.
           03          COL 64  VALUE 'ZIP'.
           03          COL 70  value 'PHONE'.
               
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT STU-MST.
           MOVE 'N'    TO WS-EOF.
           MOVE 0      TO WS-CTR.
           
           PERFORM 999-DISP-HEADERS.
           
           DISPLAY SPACES.
           DISPLAY SPACES.
           
           PERFORM UNTIL EOF
               READ STU-MST 
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM 100-DISPLAY
               END-READ
           END-PERFORM.
           
           CLOSE STU-MST.
       
           DISPLAY "PRESS ENTER TO EXIT" WITH NO ADVANCING.
           ACCEPT WS-RESP.
           EXIT PROGRAM.
       
       100-DISPLAY.
           ADD 1 TO WS-CTR
           IF WS-CTR > 15 THEN
               DISPLAY SPACES
               DISPLAY 'PRESS ENTER TO CONTINUE'
               ACCEPT WS-RESP
               PERFORM 999-DISP-HEADERS
               DISPLAY SPACES
               DISPLAY SPACES
               MOVE 1 TO WS-CTR                                 
           END-IF                                                       
           DISPLAY STU-ID, " ", STU-L-NAME, " ", STU-F-NAME," ",
           STU-STREET, " ", STU-ZIP, " ", STU-PHONE.
              
       999-DISP-HEADERS.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           DISPLAY HEADER,HEADER-2.
