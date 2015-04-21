       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUILD-MST-CTRL-LIST.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                                                                        
           SELECT MST-CTRL-LIST    ASSIGN TO 
                                       "../Files/MST-CTRL-LST.DAT"
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
       
       DISPLAY WS-STAT.
       
       MOVE 1 TO WS-REC-KEY.
       MOVE "UNIVERSITY OF ARKANSAS - FORT SMITH" TO MST-UNIV-NAME.
       WRITE MST-UNIV-NAME.
       
       MOVE 2 TO WS-REC-KEY.
       MOVE SPACES TO MST-UNIV-ADDR.
       MOVE "5210 Grand Avenue. P0 BOX 3649" TO MST-UNIV-ADDR-ST.
       MOVE 72913 TO MST-UNIV-ADDR-ZIP.
       WRITE MST-UNIV-ADDR.

       MOVE 3 TO WS-REC-KEY.
       MOVE SPACES TO MST-NEXT-CRNS.
       MOVE 0000 TO MST-NEXT-CRN-YR.
       MOVE 00 TO MST-NEXT-CRN-SEM.
       MOVE 0000 TO MST-NEXT-CRN-CRN.
       WRITE MST-NEXT-CRNS.
       
       MOVE 4 TO WS-REC-KEY.
       MOVE SPACES TO MST-NEXT-CRNS.
       MOVE 0000 TO MST-NEXT-CRN-YR.
       MOVE 00 TO MST-NEXT-CRN-SEM.
       MOVE 0000 TO MST-NEXT-CRN-CRN.
       WRITE MST-NEXT-CRNS.
       
       MOVE 5 TO WS-REC-KEY.
       MOVE SPACES TO MST-NEXT-INST.
       MOVE 0000 TO MST-INST-ID.
       WRITE MST-NEXT-INST.
       
       MOVE 6 TO WS-REC-KEY.
       MOVE SPACES TO MST-NEXT-STU.
       MOVE 0000 TO MST-STU-ID.
       WRITE MST-NEXT-STU.
       
       MOVE 7 TO WS-REC-KEY.
       MOVE SPACES TO MST-NEXT-CRNS.
       MOVE 0000 TO MST-NEXT-CRN-YR.
       MOVE 00 TO MST-NEXT-CRN-SEM.
       MOVE 0000 TO MST-NEXT-CRN-CRN.
       WRITE MST-NEXT-CRNS.
       
       MOVE 8 TO WS-REC-KEY.
       MOVE SPACES TO MST-NEXT-CRNS.
       MOVE 0000 TO MST-NEXT-CRN-YR.
       MOVE 00 TO MST-NEXT-CRN-SEM.
       MOVE 0000 TO MST-NEXT-CRN-CRN.
       WRITE MST-NEXT-CRNS.
       
       CLOSE MST-CTRL-LIST.
       
       
       STOP RUN.
              

