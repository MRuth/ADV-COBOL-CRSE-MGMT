       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSTRUCTOR-MASTER.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE      ASSIGN        TO 
                                       '../FILES/MASTER-FILE-SORTED.TXT'
                               ORGANIZATION  IS LINE SEQUENTIAL.
           SELECT SORT-WORK    ASSIGN        TO 'SORTWORK.TXT'.
           SELECT OUT-FILE     ASSIGN        TO 'INSTRUCTOR-MASTER.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS SEQUENTIAL
                               RECORD KEY    IS O-INSTRUCTOR-NAME
                               FILE STATUS   IS WS-STAT.
           SELECT OUT-FILEF     ASSIGN        TO 'INSTR-MASTER.DAT'
                                ORGANIZATION  IS INDEXED
                                ACCESS        IS SEQUENTIAL
                                RECORD KEY    IS OF-INSTRUCTOR-ID
                                FILE STATUS   IS WS-STAT.                              
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC.
           03  I-COURSE-ID         PIC X(9).
           03  FILLER              PIC XX VALUE SPACES.
           03  I-COURSE-NAME       PIC X(35).
           03  FILLER              PIC XX VALUE SPACES.
           03  I-COURSE-CREDIT     PIC X(4).
           03  FILLER              PIC XX VALUE SPACES.
           03  I-BUILDING          PIC X(6).
           03  FILLER              PIC XX VALUE SPACES.
           03  I-ROOM              PIC X(6).
           03  FILLER              PIC XX VALUE SPACES.
           03  I-INSTRUCTOR        PIC X(22).
           03  FILLER              PIC XX VALUE SPACES.
           03  I-MAX-SEAT          PIC 99.
       SD  SORT-WORK.
       01  SORT-REC.
           03  S-COURSE-ID        PIC X(9).
           03  S-COURSE-NAME      PIC X(35).
           03  S-COURSE-CREDIT    PIC X(4).
           03  S-BUILDING-ROOM.
               05  S-BUILDING     PIC X(6).
               05  FILLER         PIC X VALUE SPACE.
               05  S-ROOM         PIC X(6).
           03  S-INSTRUCTOR       PIC X(22).
           03  S-MAX-SEAT         PIC 99.
       FD  OUT-FILE.
       01  OUT-REC.
           03  O-INSTRUCTOR-NAME   PIC X(22).
       FD  OUT-FILEF.
       01  OUT-FREC.
           03  OF-INSTRUCTOR-ID    PIC 9999.
           03  OF-INSTRUCTOR-NAME  PIC X(22).
       WORKING-STORAGE SECTION.
       01  WS-COUNTER              PIC 9999 VALUE 7000.
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
       01  WS-DTL-LN.
           03  WS-INSTRUCTOR-ID     PIC 9999.
           03  FILLER               PIC XX.
           03  WS-INSTRUCTOR-NAME   PIC X(22).
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT IN-FILE.
           OPEN OUTPUT OUT-FILE.
           
           SORT SORT-WORK
                ON ASCENDING KEY S-INSTRUCTOR
                INPUT  PROCEDURE 100-FILE-IN
                OUTPUT PROCEDURE 200-FILE-OUT.
           DISPLAY "PROGRAM TERMINATED".
           DISPLAY "PRESS ENTER TO CLOSE".
           
           CLOSE IN-FILE.
           CLOSE OUT-FILE.
           
           OPEN INPUT OUT-FILE.
           OPEN OUTPUT OUT-FILEF
           
           MOVE 'N' TO WS-EOF.
           PERFORM UNTIL EOF
               READ OUT-FILE 
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE WS-COUNTER           TO OF-INSTRUCTOR-ID
                       MOVE O-INSTRUCTOR-NAME    TO OF-INSTRUCTOR-NAME
                       WRITE OUT-FREC
                       ADD 1 TO WS-COUNTER
               END-READ
           END-PERFORM.
           CLOSE OUT-FILE.
           CLOSE OUT-FILEF.
           
           OPEN INPUT OUT-FILEF.
           
           MOVE 'N' TO WS-EOF.
           PERFORM UNTIL EOF
               READ OUT-FILEF 
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE OF-INSTRUCTOR-ID     TO WS-INSTRUCTOR-ID
                       MOVE OF-INSTRUCTOR-NAME   TO WS-INSTRUCTOR-NAME
                       DISPLAY WS-DTL-LN
               END-READ
           END-PERFORM.
           CLOSE OUT-FILEF.           
           
           STOP RUN.
      *-----------------------------------------------------------------
       100-FILE-IN.
           PERFORM UNTIL EOF
               READ IN-FILE 
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE I-COURSE-ID     TO S-COURSE-ID
                       MOVE I-COURSE-NAME   TO S-COURSE-NAME
                       MOVE I-COURSE-CREDIT TO S-COURSE-CREDIT
                       MOVE I-BUILDING      TO S-BUILDING
                       MOVE I-ROOM          TO S-ROOM
                       MOVE I-INSTRUCTOR    TO S-INSTRUCTOR
                       MOVE I-MAX-SEAT      TO S-MAX-SEAT
                       RELEASE SORT-REC
               END-READ
           END-PERFORM.
      *-----------------------------------------------------------------
       200-FILE-OUT.
           MOVE 'N' TO WS-EOF.
           PERFORM UNTIL EOF
               RETURN SORT-WORK 
                   AT END 
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE S-INSTRUCTOR     TO O-INSTRUCTOR-NAME
                       WRITE OUT-REC
               END-RETURN
           END-PERFORM.