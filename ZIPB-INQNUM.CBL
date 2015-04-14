      ******************************************************************
      *PROGRAM : ZIP code master file inquiry                          *
      *AUTHOR  : Ken Pappas                                            *
      *DATE    : 02-08-2015                                            *
      *ABSTRACT: Inquiry by zip code                                   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZIPB-INQNUM IS INITIAL.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ZIP-MST      ASSIGN TO "../FILES/ZIPMASTER.DAT"
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS DYNAMIC
                               RECORD KEY    IS ZIP-KEY
                               ALTERNATE KEY IS ZIP-CITY
                                   WITH DUPLICATES
                               FILE STATUS   IS WS-ZIP-STAT.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD  ZIP-MST.
       01  ZIP-REC.
           03  ZIP-KEY.
               05  ZIP-CODE          PIC X(5).
           03  ZIP-CITY              PIC X(30).
           03  ZIP-STATE             PIC XX.
           03  ZIP-COUNTY            PIC X(30).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01  WS-MISC.
           03  WS-ZIP-STAT           PIC XX.
           03  WS-ZIP                PIC X(5) VALUE SPACES.
       01  WS-DATE.
           03  WS-SYS-DATE.
               05  WS-SYS-YR           PIC 99.
               05  WS-SYS-MO           PIC 99.
               05  WS-SYS-DAY          PIC 99.
           03  WS-FMT-DATE.
               05  WS-FMT-MO           PIC 99.
               05  FILLER              PIC X   VALUE "/".
               05  WS-FMT-DAY          PIC 99.
               05  FILLER              PIC X   VALUE "/".
               05  WS-FMT-YR           PIC 99.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SCRN-START.
           03  BLANK SCREEN.
           03  LINE 01 COL 01 VALUE "ZIPB-INQNUM".
           03  LINE 01 COL 73 PIC X(8) FROM WS-FMT-DATE.
           03  LINE 06 COL 36 VALUE "ZIP CODE SYSTEM".
           03  LINE 08 COL 36 VALUE "  ZIP INQUIRY".
       01  SCRN-ENTRY.
           03  LINE 10 COL 32 VALUE "ENTER ZIP".
           03  LINE 10 COL 42 PIC X(5)  TO   WS-ZIP AUTO.
           03  LINE 12 COL 32 VALUE "ZIP CODE".
           03  LINE 12 COL 42 PIC X(5)  FROM ZIP-CODE.
           03  LINE 13 COL 32 VALUE "CITY".
           03  LINE 13 COL 42 PIC X(30) FROM ZIP-CITY.
           03  LINE 14 COL 32 VALUE "COUNTY".
           03  LINE 14 COL 42 PIC X(30) FROM ZIP-COUNTY.
           03  LINE 15 COL 32 VALUE "STATE".
           03  LINE 15 COL 42 PIC XX    FROM ZIP-STATE.
           03  LINE 17 COL 32 VALUE "ENTER ZIP OR X TO EXIT".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT ZIP-MST.
           ACCEPT WS-SYS-DATE FROM DATE.
           MOVE   WS-SYS-MO    TO  WS-FMT-MO.
           MOVE   WS-SYS-DAY   TO  WS-FMT-DAY.
           MOVE   WS-SYS-YR    TO  WS-FMT-YR.
           DISPLAY SCRN-START.
           MOVE SPACES TO WS-ZIP
                          ZIP-REC.
           DISPLAY SCRN-ENTRY.
           ACCEPT  SCRN-ENTRY.
           PERFORM  200-READ-LOOP UNTIL WS-ZIP = "X" OR "x". 
           CLOSE ZIP-MST.
           EXIT PROGRAM.
      *-----------------------------------------------------------------
       200-READ-LOOP.
           MOVE  WS-ZIP TO ZIP-KEY
           START ZIP-MST KEY EQUAL TO ZIP-KEY
               INVALID KEY
                   MOVE "RECORD NOT FOUND" TO ZIP-CITY
                   MOVE SPACES TO ZIP-COUNTY
                                  ZIP-STATE
               NOT INVALID KEY
                   READ ZIP-MST
           END-START
           DISPLAY SCRN-ENTRY.
           ACCEPT  SCRN-ENTRY.
      *-----------------------------------------------------------------
      