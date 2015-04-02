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
       
       SELECT IN-FILE ASSIGN TO'../FILES/STUDENT-MASTER.DAT'
           ORGANIZATION IS INDEXED
           ACCESS IS SEQUENTIAL
           RECORD KEY IS IN-STU-ID.
   
       DATA DIVISION.
       FILE SECTION.
       
       FD  IN-FILE.
           01  IN-REC.
               03  IN-STU-ID           PIC 9999.
               03  IN-NAME.
                   05  IN-L-NAME       PIC X(15).
                   05  IN-F-NAME       PIC X(15).
               03  IN-ADDR.
                   05  IN-STREET       PIC X(25).
                   05  IN-ZIP          PIC XXXXX.
               03  IN-PHONE            PIC X(10).
               03  IN-STATUS           PIC X.       
       
       WORKING-STORAGE SECTION.
       01  WS-CTR                      PIC 99      VALUE 0.
       01  WS-RESP                     PIC X.
       01  WS-EOF                      PIC X       VALUE 'N'.
           88  EOF                                 VALUE 'Y'.
       01  WS-REC.
           03  WS-STU-ID               PIC 9999.
           03  WS-NAME.
               05  WS-L-NAME           PIC X(15).
               05  WS-F-NAME           PIC X(15).
           03  WS-ADDR.
               05  WS-STREET           PIC X(25).
               05  WS-ZIP              PIC XXXXX.
           03  WS-PHONE                PIC X(10).
       
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
       OPEN INPUT IN-FILE.
       
       DISPLAY NEW-SCREEN.
       DISPLAY SPACES.
       DISPLAY SPACES.
       
       PERFORM UNTIL EOF
           READ IN-FILE 
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM 100-DISPLAY
           END-READ
       END-PERFORM.
       
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
           DISPLAY IN-STU-ID, " ", IN-L-NAME, " ", IN-F-NAME,   
               " ",IN-STREET, " ", IN-ZIP, " ", IN-PHONE.
           DISPLAY SPACES.
              

