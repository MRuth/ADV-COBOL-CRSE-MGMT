       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSTRUCTOR-LIST.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INSTR-MASTER         ASSIGN        TO 
                                       '../FILES/INSTR-MASTER.DAT'
                                       ORGANIZATION  IS INDEXED
                                       ACCESS        IS SEQUENTIAL
                                       RECORD KEY    IS INSTR-ID
                                       ALTERNATE KEY IS INSTR-NAME
                                       FILE STATUS   IS WS-STAT.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FD  INSTR-MASTER.
       01  INSTR-REC.
           03  INSTR-ID    PIC 9999.
           03  INSTR-NAME  PIC X(22).
       WORKING-STORAGE SECTION.
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
           03  WS-COUNTER          PIC 99 VALUE 0.
           03  WS-BLNK-LN          PIC X(80) VALUE SPACES.
       01  WS-PG-BREAK.
           03  FILLER              PIC X(15) VALUE 'PRESS ENTER TO '.
           03  FILLER              PIC X(16) VALUE 'DISPLAY 10 MORE '.
           03  FILLER              PIC X(49) VALUE 'RECORDS'.
       01  WS-HEADER.
           03  FILLER              PIC X(15) VALUE 'INSTRUCTOR ID'.
           03  FILLER              PIC X(65) VALUE 'INSTRUCTOR NAME'.
       01  WS-DTL-LN.
           03  WS-INSTR-ID         PIC 9999.
           03  FILLER              PIC X(11) VALUE SPACES.
           03  WS-INSTR-NAME       PIC X(65).
       SCREEN SECTION.
       01  BLNK-SCREEN.
           03  BLANK SCREEN.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT INSTR-MASTER.
           
           MOVE 'N' TO WS-EOF.
           MOVE 0 TO WS-COUNTER.
           DISPLAY BLNK-SCREEN.
           DISPLAY WS-HEADER.
           DISPLAY WS-BLNK-LN.
           PERFORM UNTIL EOF
               READ INSTR-MASTER
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                           MOVE INSTR-ID     TO WS-INSTR-ID
                           MOVE INSTR-NAME   TO WS-INSTR-NAME
                           DISPLAY WS-DTL-LN
                           DISPLAY WS-BLNK-LN
                           ADD 1 TO WS-COUNTER
                           IF WS-COUNTER = 10
                               THEN
                                   DISPLAY WS-PG-BREAK
                                   ACCEPT WS-RESP
                                   DISPLAY BLNK-SCREEN
                                   DISPLAY WS-HEADER
                                   DISPLAY WS-BLNK-LN
                                   MOVE 0 TO WS-COUNTER
                           END-IF           
               END-READ
           END-PERFORM.
           
           DISPLAY 'PRESS ENTER TO GO BACK TO MENU'
           ACCEPT WS-RESP.
           CLOSE INSTR-MASTER.
           EXIT PROGRAM.
           
           STOP RUN.