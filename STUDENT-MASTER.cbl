      ******************************************************************
      *PROGRAM :  STU-BUILDER                                          *
      *AUTHOR  : MONTANA RUTH                                          *
      *DATE    :    Feb 26, 2015                                       *
      *ABSTRACT:                                                       *
      ******************************************************************
        
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STU-BUILDER.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
       SELECT IN-FILE ASSIGN TO '../FILES/STUDENT-STARTER.TXT'
           ORGANIZATION IS LINE SEQUENTIAL.
           
       SELECT STU-MST ASSIGN TO'../FILES/STUDENT-MASTER.DAT'
           ORGANIZATION IS INDEXED
           ACCESS IS SEQUENTIAL
           RECORD KEY IS STU-ID.
           
       SELECT MST-CTRL-LIST    ASSIGN TO 
                                       "../Files/MST-CTRL-LST.DAT"
                                       ORGANIZATION IS RELATIVE
                                       ACCESS IS RANDOM
                                       RELATIVE KEY IS WS-MST-REC-KEY
                                       FILE STATUS IS WS-MST-STAT.
           
       SELECT SORT-WORK ASSIGN TO 'SORTWORK.TXT'.
       
       DATA DIVISION.
       
       FILE SECTION.
       
       COPY MST-CTRL-LIST-RECS.
       COPY STU-MST-DEF.
       
       FD  IN-FILE.
           01  IN-REC.
               03  IN-NAME.
                   05  IN-L-NAME       PIC X(15).
                   05  IN-F-NAME       PIC X(15).
               03  IN-ADDR.
                   05  IN-STREET       PIC X(25).
                   05  IN-CITY         PIC X(20).
                   05  IN-ST           PIC XX.
                   05  IN-ZIP          PIC XXXXX.
               03 IN-PHONE             PIC X(10).
               03 FILLER               PIC X(21).
       
       SD  SORT-WORK.
           01  SRT-REC.
               03  SRT-NAME.
                   05  SRT-L-NAME      PIC X(15).
                   05  SRT-F-NAME      PIC X(15).
               03  SRT-ADDR.
                   05  SRT-STREET      PIC X(25).
                   05  SRT-ZIP         PIC XXXXX.
               03  SRT-PHONE           PIC X(10).
               
       WORKING-STORAGE SECTION.
       
           01  WS-EOF                  PIC X       VALUE 'N'.
               88  EOF                             VALUE 'Y'.
           01  WS-STATUS               PIC X       VALUE 'A'.
           01  WS-CURR-ID              PIC 9999    VALUE 0.
           01  WS-RSP                  PIC X.
           01  WS-MST-REC-KEY          PIC 9999.
           01  WS-MST-STAT             PIC XX.
           01  WS-DSP-CTR              PIC 99      VALUE 0.
       
       SCREEN SECTION.
           01  CLEAR.
               03  BLANK SCREEN.
      
       PROCEDURE DIVISION.
       
       000-MAIN.
           
           OPEN INPUT IN-FILE.
           OPEN OUTPUT STU-MST.
           OPEN I-O MST-CTRL-LIST.
           
           MOVE    'N'     TO WS-EOF.
           MOVE    0       TO WS-CURR-ID.
           MOVE    0       TO WS-DSP-CTR.
           DISPLAY CLEAR.
           SORT SORT-WORK
               ON ASCENDING KEY SRT-L-NAME
               ON ASCENDING KEY SRT-F-NAME
               INPUT  PROCEDURE 100-FILE-IN
               OUTPUT PROCEDURE 200-FILE-OUT.
             
           MOVE 6 TO WS-MST-REC-KEY.
           MOVE SPACES TO MST-NEXT-STU.
           MOVE WS-CURR-ID TO MST-STU-ID.
           REWRITE MST-NEXT-STU.
           
           CLOSE IN-FILE,
               STU-MST
               MST-CTRL-LIST.
               
           DISPLAY SPACES.
           DISPLAY "PRESS ENTER TO EXIT" WITH NO ADVANCING.
           ACCEPT WS-RSP.
           EXIT PROGRAM.
           
       100-FILE-IN.
           PERFORM UNTIL EOF
               READ IN-FILE 
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE IN-NAME    TO SRT-NAME
                       MOVE IN-STREET  TO SRT-STREET
                       MOVE IN-ZIP     TO SRT-ZIP
                       MOVE IN-PHONE   TO SRT-PHONE
                       RELEASE SRT-REC
               END-READ
           END-PERFORM.
           
       200-FILE-OUT.
           MOVE 'N' TO WS-EOF.
           PERFORM UNTIL EOF
               RETURN SORT-WORK 
                   AT END 
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE WS-CURR-ID TO STU-ID
                       MOVE SRT-NAME   TO STU-NAME
                       MOVE SRT-STREET TO STU-STREET
                       MOVE SRT-ZIP    TO STU-ZIP
                       MOVE SRT-PHONE  TO STU-PHONE
                       MOVE WS-STATUS  TO STU-STATUS
                       ADD 1           TO WS-CURR-ID
                       WRITE STU-REC
                       PERFORM 300-DISPLAY
               END-RETURN
           END-PERFORM.
           
       300-DISPLAY.
           ADD 1 TO WS-DSP-CTR.
       
           IF WS-DSP-CTR GREATER THAN 10
               DISPLAY SPACES
               DISPLAY 'PRESS ENTER TO CONTINUE'
               ACCEPT WS-RSP
               DISPLAY CLEAR
               MOVE 1 TO WS-DSP-CTR.
               
           DISPLAY STU-ID, " ", STU-NAME, " ", 
                           STU-STATUS
      