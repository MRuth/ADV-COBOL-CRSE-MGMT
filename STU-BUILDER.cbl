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
           
       SELECT SORT-WORK ASSIGN TO 'SORTWORK.TXT'.
       
       SELECT OUT-FILE ASSIGN TO'../FILES/STUDENT-MASTER.DAT'
           ORGANIZATION IS INDEXED
           ACCESS IS SEQUENTIAL
           RECORD KEY IS OUT-STU-ID.
           
       DATA DIVISION.
       
       FILE SECTION.
       
       FD  IN-FILE.
           01  IN-REC.
               03  IN-NAME.
                   05  IN-L-NAME               PIC X(15).
                   05  IN-F-NAME               PIC X(15).
               03  IN-ADDR.
                   05  IN-STREET               PIC X(25).
                   05  IN-CITY                 PIC X(20).
                   05  IN-ST                   PIC XX.
                   05  IN-ZIP                  PIC XXXXX.
               03 IN-PHONE                     PIC X(10).
               03 FILLER                       PIC X(21).
       
       SD  SORT-WORK.
           01  SRT-REC.
               03  SRT-NAME.
                   05  SRT-L-NAME      PIC X(15).
                   05  SRT-F-NAME      PIC X(15).
               03  SRT-ADDR.
                   05  SRT-STREET      PIC X(25).
                   05  SRT-CITY        PIC X(20).
                   05  SRT-ST          PIC XX.
                   05  SRT-ZIP         PIC XXXXX.
               03  SRT-PHONE           PIC X(10).
               
       FD  OUT-FILE.
           01  OUT-REC.
               03  OUT-STU-ID          PIC 9999.
               03  OUT-NAME.
                   05  OUT-F-NAME      PIC X(15).
                   05  OUT-L-NAME      PIC X(15).
               03  OUT-ADDR.
                   05  OUT-STREET      PIC X(25).
                   05  OUT-CITY        PIC X(20).
                   05  OUT-ST          PIC XX.
                   05  OUT-ZIP         PIC XXXXX.
               03  OUT-PHONE           PIC X(10).
               03  OUT-STATUS          PIC X.
               
       WORKING-STORAGE SECTION.
       
       01  WS-EOF              PIC X   VALUE 'N'.
           88  EOF                     VALUE 'Y'.
       01  WS-STATUS                   PIC X       VALUE 'A'.
       01  WS-CURR-ID                  PIC 9999    VALUE 0000.

       PROCEDURE DIVISION.
       
       000-MAIN.
       
       OPEN INPUT IN-FILE.
       OPEN OUTPUT OUT-FILE.
       
       SORT SORT-WORK
                ON ASCENDING KEY SRT-L-NAME
                ON ASCENDING KEY SRT-F-NAME
                INPUT  PROCEDURE 100-FILE-IN
                OUTPUT PROCEDURE 200-FILE-OUT.
       
       100-FILE-IN.
       
           PERFORM UNTIL EOF
               READ IN-FILE 
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE IN-NAME    TO SRT-NAME
                       MOVE IN-ADDR    TO SRT-ADDR
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
                       MOVE WS-CURR-ID TO OUT-STU-ID
                       MOVE SRT-NAME   TO OUT-NAME
                       MOVE SRT-ADDR   TO OUT-ADDR
                       MOVE SRT-PHONE  TO OUT-PHONE
                       MOVE WS-STATUS  TO OUT-STATUS
                       ADD 1           TO WS-CURR-ID
                       WRITE OUT-REC
                       DISPLAY OUT-REC
               END-RETURN
           END-PERFORM.
       
       STOP RUN.
       

