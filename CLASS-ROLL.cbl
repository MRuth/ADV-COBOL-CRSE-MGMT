       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLASS-ROLL.
      *-----------------------------------------------------------------
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
           03  WS-STU-ID           PIC 9999.
           03  WS-STU-NAME         PIC X(20).
           03  WS-SPACE            PIC X VALUE SPACE.
           03  WS-YEAR             PIC 9999.
           03  WS-SEM              PIC 99.
           03  WS-CRN              PIC 9999.
           03  WS-CRSE-NAME        PIC X(35).
           03  WS-INSTR-ID         PIC 9999.
           03  WS-INSTR-NAME       PIC X(20).
            
       01  WS-HEADER.
           03  FILLER              PIC X(26) VALUE SPACES.
           03  FILLER              PIC X(25) VALUE 'NAME'.
           03  FILLER              PIC X(29) VALUE 'CLASS ROLE'.
       01  WS-DTL-LN.
           03  FILLER              PIC X(26) VALUE SPACES.
           03  WS-NAME             PIC X(20).
           03  FILLER              PIC X(5)  VALUE SPACES.
           03  WS-ROLE             PIC X(10).
       01  WS-PG-BREAK.
           03  FILLER              PIC X(26) VALUE SPACES.
           03  FILLER              PIC X(15) VALUE 'PRESS ENTER TO '.
           03  FILLER              PIC X(16) VALUE 'DISPLAY 07 MORE '.
           03  FILLER              PIC X(23) VALUE 'STUDENTS'.
       01  WS-ENDING.
           03  FILLER   PIC X(26) VALUE SPACES.
           03  FILLER   PIC X(29) VALUE "PRESS ENTER TO RETURN TO MAIN".
           03  FILLER   PIC X(25) VALUE "MENU".
      *-----------------------------------------------------------------
       SCREEN SECTION.
       COPY SCR-COMMON. 
        
       01  SCRN-TITLE.
           03  LINE 3  COL 40  VALUE 'CLASS ROLL'.
       01  SCRN-DATA. 
           03  SCRN-CRSE.
               05  LINE 5  COL 20   VALUE  'COURSE CRN: '.
               05          COL 33  PIC 9999 USING WS-CRN         
                                            AUTO REQUIRED FULL.
               05          COL 39  VALUE   'YEAR: '.
               05          COL 45  PIC 9999 USING WS-YEAR
                                            AUTO REQUIRED FULL.
               05          COL 54  VALUE   'SEMESTER: '.
               05          COL 64  PIC 99   USING WS-SEM
                                             AUTO REQUIRED FULL.    
           03  SCRN-CRSE-NAME.
               05  LINE 7  COL 20   VALUE   'COURSE NAME: '.
               05          COL 35  PIC X(35) FROM WS-CRSE-NAME. 
       01  SCRN-ERR.
           03  LINE 20  COL 30  VALUE 'COURSE CAN NOT BE FOUND'.     
       01  SCRN-ANOTHER.

      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT REG-MASTER.
           OPEN INPUT STU-MST.
           MOVE ZEROS TO WS-CRN,WS-SEM,WS-YEAR.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           DISPLAY HEADER.
           DISPLAY SCRN-TITLE.
           DISPLAY SCRN-CRSE.
           ACCEPT  SCRN-CRSE.
           PERFORM 100-GET-COURSE-NAME.
           DISPLAY WS-BLNK-LN.
           DISPLAY WS-HEADER
           DISPLAY WS-BLNK-LN
           MOVE 'N' TO WS-EOF
           MOVE ZERO TO WS-COUNTER
           PERFORM UNTIL EOF
           READ REG-MASTER
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF  WS-YEAR = REG-YEAR
                       AND WS-SEM = REG-SEM
                       THEN
                           IF  WS-CRN = FIRST-CRN
                               OR WS-CRN = SECOND-CRN
                               OR WS-CRN = THIRD-CRN
                               OR WS-CRN = FOURTH-CRN
                               OR WS-CRN = FIFTH-CRN
                               THEN
                                   MOVE REG-STU-ID TO WS-STU-ID
                                   PERFORM 200-GET-STU-NAME
                                   PERFORM 300-DISPLAY
                           END-IF
                   END-IF
           END-READ 
           END-PERFORM.

           PERFORM 400-GET-INSTRUCTOR-NAME.
           DISPLAY WS-DTL-LN.
           DISPLAY WS-BLNK-LN.
           DISPLAY WS-ENDING.
           ACCEPT WS-RESP.
           CLOSE REG-MASTER.
           CLOSE STU-MST.

           EXIT PROGRAM.
      *-----------------------------------------------------------------
       100-GET-COURSE-NAME.
           STRING WS-YEAR DELIMITED BY SIZE
                  WS-SPACE DELIMITED BY SIZE
                  WS-SEM DELIMITED BY SIZE
                  WS-SPACE DELIMITED BY SIZE
                  WS-CRN DELIMITED BY SIZE
                  INTO SCHEDULE-ID.

           OPEN INPUT SCHED-MST.
           OPEN INPUT CRSE-MASTER.
           READ SCHED-MST
               INVALID KEY
                   DISPLAY SCRN-ERR
                   ACCEPT  WS-RESP
                   EXIT PROGRAM
               NOT INVALID KEY
                   MOVE COURSE-ID TO CRSE-ID
                   MOVE INSTRUCTOR-ID TO WS-INSTR-ID
                   READ CRSE-MASTER
                       INVALID KEY
                           DISPLAY SCRN-ERR
                           ACCEPT  WS-RESP
                           EXIT PROGRAM
                       NOT INVALID KEY
                          MOVE CRSE-NAME TO WS-CRSE-NAME
                          DISPLAY SCRN-CRSE-NAME
                   END-READ
           END-READ
           
           CLOSE SCHED-MST.
           CLOSE CRSE-MASTER.
      *-----------------------------------------------------------------
       200-GET-STU-NAME.
           MOVE WS-STU-ID TO STU-ID.
           
                   READ STU-MST
                       INVALID KEY
                       NOT INVALID KEY
                           STRING
                               STU-F-NAME DELIMITED BY SPACE
                               WS-SPACE   DELIMITED BY SIZE
                               STU-L-NAME DELIMITED BY SPACE
                               INTO WS-STU-NAME
                   END-READ.
           MOVE WS-STU-NAME TO WS-NAME.
           MOVE 'STUDENT' TO WS-ROLE.
      *-----------------------------------------------------------------
       300-DISPLAY.

           DISPLAY WS-DTL-LN
           ADD 1 TO WS-COUNTER
           IF WS-COUNTER = 10
               THEN
                   DISPLAY WS-BLNK-LN
                   DISPLAY WS-PG-BREAK
                   ACCEPT WS-RESP
                   DISPLAY SCRN-TITLE
                   DISPLAY SCRN-DATA
                   DISPLAY WS-BLNK-LN
                   DISPLAY WS-HEADER 
                   DISPLAY WS-BLNK-LN
                   MOVE 0 TO WS-COUNTER
           END-IF. 
      *-----------------------------------------------------------------
       400-GET-INSTRUCTOR-NAME.
           
           OPEN INPUT INST-MASTER.
           MOVE WS-INSTR-ID TO INST-ID.
           READ INST-MASTER
               INVALID KEY
                   MOVE SPACES TO WS-NAME
                   MOVE SPACES TO WS-ROLE
               NOT INVALID KEY
                   MOVE INST-NAME TO WS-NAME
                   MOVE 'INSTRUCTOR' TO WS-ROLE 
           END-READ.
           CLOSE INST-MASTER.