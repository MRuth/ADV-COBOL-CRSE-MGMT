      ******************************************************************
      *PROGRAM : Create ZIP code ISAM master file                      *
      *AUTHOR  : Ken Pappas                                            *
      *DATE    : 02-08-2015                                            *
      *ABSTRACT: Builds ISAM file for inquiry programs.                *
      *          '02' status code is dup rec on alt key (allowed)      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZIPD-BUILDISAM IS INITIAL.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ZIP-MST-IN   ASSIGN TO "../FILES/ZIPSTARTER.TXT"
                               ORGANIZATION IS LINE SEQUENTIAL.
      
           SELECT SORT-WORK    ASSIGN TO "SORTWORK.TXT".
      
           SELECT ZIP-MST-OUT  ASSIGN TO "../FILES/ZIPMASTER.DAT"
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS SEQUENTIAL
                               RECORD KEY    IS ZIP-KEY
                               ALTERNATE KEY IS ZIP-CITYO
                                   WITH DUPLICATES
                               FILE STATUS   IS WS-ZIP-STAT.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD  ZIP-MST-IN.
       01  ZIP-RECI.
           03  ZIP-CODEI              PIC X(5).
           03  ZIP-CITYI              PIC X(30).
           03  ZIP-STATEI             PIC XX.
           03  ZIP-COUNTYI            PIC X(30).
      
       SD  SORT-WORK.
       01  SORT-REC.
           03  SORT-CODE              PIC X(5).
           03  SORT-CITY              PIC X(30).
           03  SORT-STATE             PIC XX.
           03  SORT-COUNTY            PIC X(30).
      
       FD  ZIP-MST-OUT.
       01  ZIP-RECO.
           03  ZIP-KEY.
               05  ZIP-CODEO          PIC X(5).
           03  ZIP-CITYO              PIC X(30).
           03  ZIP-STATEO             PIC XX.
           03  ZIP-COUNTYO            PIC X(30).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01  WS-MISC.
           03  EOF-FLAG               PIC X        VALUE 'N'.
               88  EOF                             VALUE 'Y'.
           03  WS-ZIP-STAT            PIC XX.
           03  WS-CTR                 PIC 999999   VALUE ZERO.
           03  WS-RESP                PIC X        VALUE SPACE.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SCRN-WORKING-MSG.
           03  BLANK SCREEN.
           03  LINE 12 COLUMN 32 VALUE "BUILDING ZIP FILE".
       01  SCRN-ERR-MSG.
           03  BLANK SCREEN.
           03  LINE 10 COL 32 VALUE    "ERROR BUILDING FILE".
           03  LINE 12 COL 32 VALUE    "FILE STATUS: ".
           03  LINE 12 COL 46 PIC XX    FROM WS-ZIP-STAT.
           03  LINE 13 COL 32 VALUE    "RECORD KEY:".
           03  LINE 13 COL 46 PIC X(5)  FROM ZIP-KEY.
           03  LINE 15 COL 32 VALUE    "PROGRAM TERMINATED".
           03  LINE 15 COL 50 PIC X     TO WS-RESP.
       01  SCRN-END.
           03  BLANK SCREEN.
      *COPY SCRN-SCALE.    
           03  LINE 10 COL 33 VALUE    "ZIP ISAM BUILT".
           03  LINE 12 COL 33 VALUE    "RECORDS".
           03  LINE 12 COL 41 PIC ZZZ,ZZ9  FROM WS-CTR.
           03  LINE 14 COL 26 VALUE    "PRESS ENTER TO RETURN TO MENU".
           03  LINE 14 COL 55 PIC X        TO WS-RESP.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.
           DISPLAY SCRN-WORKING-MSG.
           OPEN OUTPUT ZIP-MST-OUT.
      
           SORT SORT-WORK
                ON ASCENDING KEY SORT-CODE
                USING            ZIP-MST-IN
                OUTPUT PROCEDURE 200-BUILD-LOOP.
      
           CLOSE ZIP-MST-OUT.
           DISPLAY SCRN-END.
           ACCEPT  SCRN-END.
           EXIT PROGRAM.
      *-----------------------------------------------------------------
       200-BUILD-LOOP.
           PERFORM UNTIL EOF
               RETURN  SORT-WORK 
                   AT END 
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       MOVE  SORT-REC TO ZIP-RECO
                       WRITE ZIP-RECO
                       IF WS-ZIP-STAT > '02'
                           DISPLAY SCRN-ERR-MSG
                           ACCEPT  SCRN-ERR-MSG
                           STOP RUN
                       END-IF
                       ADD   1 TO WS-CTR
               END-RETURN
           END-PERFORM.
      *-----------------------------------------------------------------
