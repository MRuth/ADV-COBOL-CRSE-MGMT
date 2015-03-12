       IDENTIFICATION DIVISION.
       PROGRAM-ID. COURSE-MASTER.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE      ASSIGN        TO
                                       '../FILES/COURSE-MASTER-S.TXT'
                               ORGANIZATION  IS LINE SEQUENTIAL.
           SELECT SORT-WORK    ASSIGN        TO 'SORTWORK.TXT'.
           SELECT OUT-FILE     ASSIGN        TO
                                       '../FILES/COURSE-MASTER-SORT.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS SEQUENTIAL
                               RECORD KEY    IS O-COURSE-ID
                               FILE STATUS   IS WS-STAT.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC.
           03  COURSE-ID           PIC X(9).
           03  FILLER              PIC XX VALUE SPACES.
           03  COURSE-NAME         PIC X(35).
           03  FILLER              PIC X  VALUE SPACE.
           03  COURSE-CREDIT       PIC X(4).
           03  FILLER              PIC XX VALUE SPACES.
           03  COURSE-STAT         PIC X.
       SD  SORT-WORK.
       01  SORT-REC.
           03  S-COURSE-ID           PIC X(9).
           03  S-COURSE-NAME         PIC X(35).
           03  S-COURSE-CREDIT       PIC X(4).
           03  S-COURSE-STAT         PIC X.
       FD  OUT-FILE.
       01  OUT-REC.
           03  O-COURSE-ID        PIC X(9).
           03  O-COURSE-NAME      PIC X(35).
           03  O-COURSE-CREDIT    PIC X(4).
           03  O-COURSE-STAT      PIC X.
       WORKING-STORAGE SECTION.
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
       01  WS-DTL-LN.
           03  WS-COURSE-ID        PIC X(9).
           03  FILLER              PIC XX.
           03  WS-COURSE-NAME      PIC X(35).
           03  FILLER              PIC XX.
           03  WS-COURSE-CREDIT    PIC X(4).
           03  FILLER              PIC XX.
           03  WS-COURSE-STAT      PIC X.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT IN-FILE.
           OPEN OUTPUT OUT-FILE.
           
           SORT SORT-WORK
                ON ASCENDING KEY S-COURSE-ID
                INPUT  PROCEDURE 100-FILE-IN
                OUTPUT PROCEDURE 200-FILE-OUT.
           DISPLAY "PROGRAM TERMINATED".
           DISPLAY "PRESS ENTER TO CLOSE".
           
           CLOSE IN-FILE.
           CLOSE OUT-FILE.
           
      *     OPEN INPUT OUT-FILE.
           
      *     MOVE 'N' TO WS-EOF.
      *     PERFORM UNTIL EOF
      *         READ OUT-FILE 
      *             AT END
      *                 MOVE 'Y' TO WS-EOF
      *             NOT AT END
      *                 MOVE O-COURSE-ID     TO WS-COURSE-ID
      *                 MOVE O-COURSE-NAME   TO WS-COURSE-NAME
      *                 MOVE O-COURSE-CREDIT TO WS-COURSE-CREDIT
      *                 MOVE O-COURSE-STAT   TO WS-COURSE-STAT
      *                 DISPLAY WS-DTL-LN
      *         END-READ
      *     END-PERFORM.
      *     CLOSE OUT-FILE.
           
           STOP RUN.
      *-----------------------------------------------------------------
       100-FILE-IN.
           PERFORM UNTIL EOF
               READ IN-FILE 
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE COURSE-ID     TO S-COURSE-ID
                       MOVE COURSE-NAME   TO S-COURSE-NAME
                       MOVE COURSE-CREDIT TO S-COURSE-CREDIT
                       MOVE COURSE-STAT   TO S-COURSE-STAT
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
                       MOVE S-COURSE-ID     TO O-COURSE-ID
                       MOVE S-COURSE-NAME   TO O-COURSE-NAME
                       MOVE S-COURSE-CREDIT TO O-COURSE-CREDIT
                       MOVE S-COURSE-STAT   TO O-COURSE-STAT
                       WRITE OUT-REC
               END-RETURN
           END-PERFORM.