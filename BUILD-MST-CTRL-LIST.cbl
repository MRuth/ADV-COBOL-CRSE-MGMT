       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUILD-MST-CTRL-LIST.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                                                                        
           SELECT MST-CTRL-LIST    ASSIGN TO 
                                       "./FILES/MST-CTRL-LIST.DAT"
                                       ORGANIZATION IS RELATIVE
                                       ACCESS IS RANDOM
                                       RELATIVE KEY IS WS-REC-KEY
                                       FILE STATUS IS WS-STAT.
                                       
       
   
       DATA DIVISION.
       FILE SECTION.
       COPY MST-CTRL-LIST-RECS.
       
       
       WORKING-STORAGE SECTION.
       01  WS-REC-KEY                  PIC 9(4).
       01  WS-STAT                     PIC XX.
   
       PROCEDURE DIVISION.
       000-MAIN.
       
       OPEN OUTPUT MST-CTRL-LIST.
       
       MOVE 1 TO WS-REC-KEY.
       MOVE 'UNIVERSITY OF ARKANSAS - FORT SMITH' TO UNIV-NAME.
       WRITE UNIV-NAME.
       
       CLOSE MST-CTRL-LIST.
       
       
       STOP RUN.
              

