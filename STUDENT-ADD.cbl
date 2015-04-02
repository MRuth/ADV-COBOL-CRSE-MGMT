      ******************************************************************
      *PROGRAM : STUDENT-ADD                                           *
      *AUTHOR  : MONTANA RUTH                                          *
      *DATE    : Apr 2, 2015                                           *
      *ABSTRACT:                                                       *
      ******************************************************************
        
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-ADD.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STU-FILE     ASSIGN TO '../FILES/STUDENT-MASTER.DAT'
                               ORGANIZATION    IS INDEXED
                               ACCESS          IS RANDOM
                               RECORD KEY      IS STU-ID
                               FILE STATUS     IS WS-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY STU-FILE-DEF.
       
       WORKING-STORAGE SECTION.
       01  WS-RESP                 PIC X.
       01  WS-STAT                 PIC 99.
       01  WS-EOF                  PIC X       VALUE 'N'.
           88  EOF                             VALUE 'Y'.
       01  WS-SAVE                 PIC X       VALUE 'N'.
           88  SAVE                            VALUE 'Y'.
       01  WS-ANOTHER              PIC X       VALUE 'Y'.
           88  ANOTHER                         VALUE 'N'.
       01  WS-DTL-LN.
               03  WS-STU-ID           PIC 9999.
               03  WS-STU-NAME.
                   05  WS-STU-L-NAME       PIC X(15).
                   05  WS-STU-F-NAME       PIC X(15).
               03  WS-STU-ADDR.
                   05  WS-STU-STREET       PIC X(25).
                   05  WS-STU-ZIP          PIC XXXXX.
               03  WS-STU-PHONE            PIC X(10).
               03  WS-STU-STATUS           PIC X.
               
       SCREEN SECTION.
       01  NEW-SCREEN.
           03  BLANK-SCREEN.
           03  LINE 1 COL 34                   VALUE 'ADD NEW STUDENT'.
           03  LINE 2 COL 1                    VALUE SPACES.
       PROCEDURE DIVISION.
       000-MAIN.
       DISPLAY NEW-SCREEN.
       ACCEPT WS-RESP.
       STOP RUN.
       
