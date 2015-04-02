       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. SCHEDULE-MASTER.                                     
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE      ASSIGN        TO
                                       '../FILES/MASTER-FILE-SORTED.TXT'
                               ORGANIZATION  IS LINE SEQUENTIAL.
           SELECT SORT-WORK    ASSIGN        TO 'SORTWORK.TXT'.
           SELECT OUT-FILE     ASSIGN        TO
                                       '../FILES/SCHEDULE-MASTER.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS SEQUENTIAL
                               RECORD KEY    IS O-SCHEDULE-ID
                               FILE STATUS   IS WS-STAT.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC.
           03  COURSE-ID-IN        PIC X(9).
           03  FILLER              PIC XX VALUE SPACES.
           03  COURSE-NAME-IN      PIC X(35).
           03  FILLER              PIC X  VALUE SPACE.
           03  COURSE-CREDIT-IN    PIC X(4).
           03  FILLER              PIC XX VALUE SPACES.
           03  BUILDING-ID-IN      PIC X(14).
           03  FILLER              PIC XX VALUE SPACES.
           03  INSTRUCTOR-NAME-IN  PIC X(22).
           03  FILLER              PIC XX VALUE SPACES.
           03  OPEN-SEATS-IN       PIC X(2).
       SD  SORT-WORK.
       01  SORT-REC.
           03  S-SCHEDULE-ID           PIC X(9).
           03  S-SCHEDULE-NAME         PIC X(35).
           03  S-SCHEDULE-CREDIT       PIC X(4).
           03  S-SCHEDULE-STAT         PIC X.
       FD  OUT-FILE.
       01  OUT-REC.
           03  O-SCHEDULE-ID        PIC X(9).
           03  O-SCHEDULE-NAME      PIC X(35).
           03  O-SCHEDULE-CREDIT    PIC X(4).
           03  O-SCHEDULE-STAT      PIC X.
       WORKING-STORAGE SECTION.
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
       01  WS-DTL-LN.
           03  WS-SCHEDULE-ID        PIC X(9).
           03  FILLER              PIC XX.
           03  WS-SCHEDULE-NAME      PIC X(35).
           03  FILLER              PIC XX.
           03  WS-SCHEDULE-CREDIT    PIC X(4).
           03  FILLER              PIC XX.
           03  WS-SCHEDULE-STAT      PIC X.
       SCREEN SECTION.
       01  BLNK-SCRN.
           03  BLANK SCREEN.           
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT IN-FILE.
           OPEN OUTPUT OUT-FILE.
           
           SORT SORT-WORK
                ON ASCENDING KEY S-SCHEDULE-ID
                INPUT  PROCEDURE 100-FILE-IN
                OUTPUT PROCEDURE 200-FILE-OUT.
           DISPLAY BLNK-SCRN.
           DISPLAY 'BUILD SUCCESSFULLY'.
           DISPLAY 'PRESS ENTER TO RETURN TO SCHEDULE MENU'.
           ACCEPT WS-RESP.
           
           CLOSE IN-FILE.
           CLOSE OUT-FILE.
           
           EXIT PROGRAM.
      *-----------------------------------------------------------------
       100-FILE-IN.
           PERFORM UNTIL EOF
               READ IN-FILE 
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE COURSE-ID-IN     TO S-SCHEDULE-ID
                       MOVE COURSE-NAME-IN   TO S-SCHEDULE-NAME
                       MOVE COURSE-CREDIT-IN TO S-SCHEDULE-CREDIT
                       MOVE BUILDING-ID-IN   TO S-SCHEDULE-STAT
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
                       MOVE S-SCHEDULE-ID     TO O-SCHEDULE-ID
                       MOVE S-SCHEDULE-NAME   TO O-SCHEDULE-NAME
                       MOVE S-SCHEDULE-CREDIT TO O-SCHEDULE-CREDIT
                       MOVE S-SCHEDULE-STAT   TO O-SCHEDULE-STAT
                       WRITE OUT-REC
               END-RETURN
           END-PERFORM.