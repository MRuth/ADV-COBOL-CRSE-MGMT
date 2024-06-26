      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID SCHEDULE-UPDATE.
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY MST-SELECTS.
           
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       COPY MST-FD.

       
       WORKING-STORAGE SECTION.
       COPY WS-COMMON.
       
           
       01  WS-REC.
           03  WS-SCHED-ID.
               05 WS-YEAR          PIC 9(4) VALUE 2015.
               05 FILLER           PIC X.
               05 WS-SEM           PIC 99.
               05 FILLER           PIC X.
               05 WS-CRN           PIC 9(4). 
           03  FILLER              PIC X.
           03  WS-COURSE-ID. 
               05  WS-COURSE-DEPT  PIC X(4). 
               05  FILLER          PIC X.
               04  WS-COURSE-NUM   PIC X(4).
           03  FILLER              PIC X.
           03  WS-TIMEDAY          PIC X(20).
           03  FILLER              PIC X.
           03  WS-BUILDING         PIC X(11).
           03  FILLER              PIC X.
           03  WS-INST-ID          PIC 9(4).
           03  FILLER              PIC X(3).
           03  WS-SEATS            PIC 99.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       COPY SCR-COMMON.
       01  BLNK-SCRN.
           03  BLANK SCREEN.
       01  SCRN-TITLE. 
           03  LINE 3  COL 30  VALUE 'UPDATE CLASS FROM SCHEDULE'.
       01  SCRN-DATA.
           03  SCRN-YEAR.
               05  LINE 6  COL 10  VALUE   'YEAR:'.
               05          COL 25  PIC X(4) USING WS-YEAR          
                                            AUTO REQUIRED.
           03  SCRN-SEM.
               05  LINE 6  COL 35  VALUE   'SEM  :'.
               05          COL 45  PIC 99   USING WS-SEM 
                                             AUTO REQUIRED.
           03  SCRN-CRN.
               05  LINE 6  COL 50 VALUE    'CRN:'.
               05          COL 55 PIC 9(4)  USING  WS-CRN
                                             AUTO REQUIRED.
           03  SCRN-CRSE-DEPT. 
               05  LINE 8  COL 10  VALUE   'COURSE DEPT:'.
               05          COL 25  PIC X(4) USING WS-COURSE-DEPT 
                                            AUTO REQUIRED.
           03  SCRN-CRSE-ID.
               05  LINE 8  COL 35  VALUE   'COURSE ID:'. 
               05          COL 45  PIC X(4) USING WS-COURSE-NUM
                                            AUTO REQUIRED.
           03  SCRN-CRS-NAME.
               05  LINE 10 COL 10 VALUE  'COURSE:'.
               05          COL 18 FROM CRSE-NAME.
           03  SCRN-TIME.
               05  LINE 12  COL 10  VALUE   'TIME / DAY:'.
               05          COL 25  PIC X(20) USING WS-TIMEDAY
                                             AUTO.
                                             
           03  SCRN-BUILD.
               05  LINE 14  COL 10  VALUE   'BUILDING:'.
               05          COL 21  PIC X(12) USING WS-BUILDING
                                             AUTO REQUIRED.
           03  SCRN-INST.
               05  LINE 16 COL 10  VALUE   'INSTRUCTOR ID:'.
               05          COL 25  PIC 9999 USING WS-INST-ID
                                             AUTO REQUIRED.
               05          COL 30  VALUE  'INSTRUCTOR:'. 
               05          COL 42  PIC X(22) FROM INST-NAME.
           03  SCRN-SEATS.
               05  LINE 14  COL 35  VALUE   'MAX SEATS:'.
               05          COL 45  PIC 99   USING WS-SEATS 
                                            AUTO REQUIRED.
          

           03          COL 75 USING WS-RESP.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN I-O SCHED-MST.
           MOVE 'Y' TO WS-ANOTHER.
           PERFORM 100-UPDATE-SCHED.
           CLOSE MST-CTRL-LIST.
           CLOSE SCHED-MST.
           EXIT PROGRAM.
      *-----------------------------------------------------------------     
           
       100-UPDATE-SCHED.
           PERFORM UNTIL ANOTHER
               MOVE SPACES TO WS-SAVE, WS-REC, INST-REC
               MOVE 2015 TO WS-YEAR
               ACCEPT WS-DATE FROM DATE 
               ACCEPT WS-TIME FROM TIME
               DISPLAY HEADER
               DISPLAY SCRN-TITLE
               DISPLAY SCRN-DATA
               PERFORM 110-VAL-CRN
               PERFORM 120-VAL-CRS
               PERFORM 130-VAL-BLD
               PERFORM 140-VAL-INS
               PERFORM 150-SAVE
           END-PERFORM.          
           
      *-----------------------------------------------------------------     
           110-VAL-CRN. 
               MOVE 'N' TO WS-VALIDATE.
               PERFORM UNTIL VALIDATED
                   MOVE 0 TO WS-MST-REC-KEY
                   MOVE '01-SPRING 02-SUMMER-I 03-SUMMER-II 04-FALL' TO 
                       WS-STATUS
                   DISPLAY SCRN-STATUS
                   ACCEPT  SCRN-SEM
                   EVALUATE WS-SEM 
                       WHEN '01' MOVE 3 TO WS-MST-REC-KEY
                       WHEN '05' MOVE 4 TO WS-MST-REC-KEY
                       WHEN '02' MOVE 7 TO WS-MST-REC-KEY
                       WHEN '04' MOVE 8 TO WS-MST-REC-KEY
                   END-EVALUATE
                   ACCEPT SCRN-CRN
                   MOVE WS-SCHED-ID TO SCHEDULE-ID
                   READ SCHED-MST
                       NOT INVALID KEY
                           MOVE SCHED-REC TO WS-REC
                           MOVE 'Y' TO WS-VALIDATE
                           DISPLAY SCRN-DATA
                   END-READ
               END-PERFORM.
      *-----------------------------------------------------------------
           120-VAL-CRS.
               OPEN INPUT CRSE-MASTER.
               MOVE 'N' TO WS-VALIDATE.
               PERFORM UNTIL VALIDATED
                   MOVE 'ENTER COURSE DEPT' TO WS-STATUS
                   DISPLAY SCRN-STATUS
                   ACCEPT  SCRN-CRSE-DEPT
                   MOVE 'ENTER COURSE ID' TO WS-STATUS
                   DISPLAY SCRN-STATUS
                   ACCEPT  SCRN-CRSE-ID 
                   MOVE WS-COURSE-ID TO CRSE-ID
                   READ CRSE-MASTER
                       INVALID KEY
                           MOVE 'INVALID COURSE' TO WS-STATUS
                           DISPLAY SCRN-STATUS
                       NOT INVALID KEY
                           MOVE CRSE-NAME TO WS-STATUS
                           DISPLAY SCRN-STATUS
                           DISPLAY SCRN-CRS-NAME
                           MOVE 'Y' TO WS-VALIDATE
                   END-READ
               END-PERFORM.
               ACCEPT  SCRN-TIME.
      *-----------------------------------------------------------------         
               
           130-VAL-BLD.
               OPEN INPUT BLD-MASTER.
               MOVE 'N' TO WS-VALIDATE.
               PERFORM UNTIL VALIDATED
                   MOVE 'ENTER ROOM ID' TO WS-STATUS
                   DISPLAY SCRN-STATUS
                   ACCEPT  SCRN-BUILD
                   MOVE WS-BUILDING TO BLD-BUILDING-ROOM
                   READ BLD-MASTER
                       INVALID KEY
                           MOVE 'INVALID BUILDING' TO WS-STATUS
                           DISPLAY SCRN-STATUS
                           ACCEPT WS-RESP
                       NOT INVALID KEY
                           MOVE BLD-MAX-SEAT TO WS-SEATS
                           DISPLAY SCRN-SEATS
                           MOVE 'OVERRIDE SEATS?' TO WS-STATUS
                           DISPLAY SCRN-STATUS
                           ACCEPT SCRN-SEATS
                           MOVE 'Y' TO WS-VALIDATE
                   END-READ
               END-PERFORM.
               CLOSE BLD-MASTER.
                
      *-----------------------------------------------------------------         
           140-VAL-INS.
               OPEN INPUT INST-MASTER.
               MOVE 'N' TO WS-VALIDATE.
               MOVE 'ENTER INSTRUCTOR ID' TO WS-STATUS.
               DISPLAY SCRN-STATUS.
               PERFORM UNTIL VALIDATED
                   ACCEPT  SCRN-INST
                   MOVE WS-INST-ID TO INST-ID
                   READ INST-MASTER
                       INVALID KEY
                           MOVE 'INVALID INSTRUCTOR' TO WS-STATUS
                           DISPLAY SCRN-STATUS
                       NOT INVALID KEY
                           MOVE INST-NAME TO WS-STATUS
                           DISPLAY SCRN-INST
                           MOVE 'Y' TO WS-VALIDATE
                   END-READ
               END-PERFORM.
               CLOSE INST-MASTER.
      *-----------------------------------------------------------------         
          150-SAVE.
               PERFORM UNTIL SAVE OR NOSAVE
                   DISPLAY SCRN-SAVE 
                   ACCEPT SCRN-SAVE
                   IF SAVE THEN
                       MOVE WS-REC TO SCHED-REC
                       REWRITE SCHED-REC
                           INVALID KEY
                               MOVE 'ERROR NOT SAVED' TO WS-STATUS
                               DISPLAY SCRN-STATUS
                               DISPLAY SCRN-ANOTHER
                               ACCEPT  SCRN-ANOTHER
                           NOT INVALID KEY 
                               MOVE SCHED-REC TO WS-STATUS
                               DISPLAY SCRN-STATUS
                               DISPLAY SCRN-ANOTHER
                               ACCEPT  SCRN-ANOTHER
                               ADD 1 TO MST-NEXT-CRN-CRN 
                                   GIVING MST-NEXT-CRN-CRN                      
                               REWRITE MST-NEXT-CRNS
                      END-REWRITE
                   END-IF
              END-PERFORM.