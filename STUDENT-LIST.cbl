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
           ACCESS IS SEQUENTIAL
           RECORD KEY IS STU-ID.
   
       DATA DIVISION.
       FILE SECTION.
       
       COPY STU-MST-DEF.   
       
       WORKING-STORAGE SECTION.
       01  WS-CTR                      PIC 99      VALUE 0.
       01  WS-RESP                     PIC X.
       01  WS-EOF                      PIC X       VALUE 'N'.
           88  EOF                                 VALUE 'Y'.
       
       SCREEN SECTION.
       01  NEW-SCREEN.
           03  BLANK SCREEN.
           03  HEADER.
               05  LINE 01 COL 01  VALUE 'ID'.
               05  LINE 01 COL 06  VALUE 'LAST NAME'.
               05  LINE 01 COL 22  VALUE 'FIRST NAME'.
               05  LINE 01 COL 38  VALUE 'STREET'.
               05  LINE 01 COL 64  VALUE 'ZIP'.
               05  LINE 01 COL 70  value 'PHONE'.
               
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT STU-MST.
           MOVE 'N'    TO WS-EOF.
           MOVE 0      TO WS-CTR.
           
           DISPLAY NEW-SCREEN.
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
           IF WS-CTR = 10 THEN
               DISPLAY SPACES
               DISPLAY 'PRESS ENTER TO CONTINUE'
               ACCEPT WS-RESP
               DISPLAY NEW-SCREEN
               DISPLAY SPACES
               DISPLAY SPACES
               MOVE 1 TO WS-CTR                                 
           END-IF                                                       
           DISPLAY STU-ID, " ", STU-L-NAME, " ", STU-F-NAME," ",
           STU-STREET, " ", STU-ZIP, " ", STU-PHONE.
           DISPLAY SPACES.
              

