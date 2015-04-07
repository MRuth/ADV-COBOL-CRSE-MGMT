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
           SELECT STU-FILE         ASSIGN TO 
                                   '../FILES/STUDENT-MASTER.DAT'
                                   ORGANIZATION    IS INDEXED
                                   ACCESS          IS RANDOM
                                   RECORD KEY      IS STU-ID
                                   FILE STATUS     IS WS-STAT.
                               
           SELECT MST-CTRL-LIST    ASSIGN TO 
                                   '../Files/MST-CTRL-LST.DAT'
                                   ORGANIZATION  IS RELATIVE
                                   ACCESS IS RANDOM
                                   RELATIVE KEY  IS WS-MST-REC-KEY.

       DATA DIVISION.
       FILE SECTION.
       COPY STU-FILE-DEF.
       COPY MST-CTRL-LIST-RECS.
       
       WORKING-STORAGE SECTION.
       01  WS-RESP                 PIC X.
       01  WS-STAT                 PIC 99.
       01  WS-MST-REC-KEY          PIC 99.
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
           03  BLANK SCREEN.
           03  LINE 1 COL 34                   VALUE 'ADD NEW STUDENT'.
       01  SCRN-FIELDS.
           03  SCRN-STU-ID.
               05  LINE 3  COL 25                  VALUE
                                                   'STUDENT ID: '.
               05          COL 43      PIC 9999    FROM WS-STU-ID.
           03  SCRN-STU-NAME.
               05  SCRN-STU-L-NAME.
                   07  LINE 5   COL 25              VALUE
                                                   'STUDENT L-NAME: '.
                   07          COL 43  PIC X(15)   TO WS-STU-L-NAME
                                                   REQUIRED.
               05  SCRN-STU-F-NAME.
                   07  LINE 6  COL 25              VALUE
                                                   'STUDENT F-NAME: '.
                   07          COL 43  PIC X(15)   TO WS-STU-F-NAME
                                                   REQUIRED.
           03  SCRN-STU-ADDR.
               05  SCRN-STU-STREET.
                   07  LINE 8  COL 25              VALUE
                                                   'STUDENT STREET: '.
                   07          COL 43  PIC X(25)   TO WS-STU-STREET
                                                   REQUIRED.
               05  SCRN-STU-ZIP.
                   07  LINE 9  COL 25              VALUE
                                                   'STUDENT ZIP   : '.
                   07          COL 43  PIC XXXXX   TO WS-STU-ZIP
                                                   AUTO REQUIRED.
           03  SCRN-STU-PHONE.
               05  LINE 11 COL 25                  VALUE
                                                   'STUDENT PHONE : '.
               05          COL 43      PIC X(10)   TO WS-STU-PHONE
                                                   AUTO REQUIRED.
           03  SCRN-STU-STATUS.                                         
               05  LINE 13 COL 25                  VALUE                
                                                   'STATUS (A/I)  : '.
               05          COL 43      PIC X       TO WS-STU-STATUS
                                                   AUTO REQUIRED.
           03  SCRN-SAVE.
               05  LINE 15 COL 32                  VALUE 'SAVE (Y/N)'.
               05          COL 30      PIC X       TO WS-SAVE.
       01  SCRN-WRITE-ERR.
           03  LINE 3  COL 30  VALUE 'STUDENT ALREADY EXISTS'.
       01  SCRN-WRITE-SUC.
           03  LINE 3  COL 30  VALUE 'STUDENT HAS BEEN ADDED'.
       01  SCRN-WRITE-NOT-SAVE.
           03  LINE 3  COL 30  VALUE 'STUDENT HAS NOT BEEN ADDED'.      
       01  SCRN-ANOTHER.
           03  LINE 5  COL 32  VALUE 'ADD ANOTHER? (Y/N)'.
           03          COL 30  PIC X TO WS-ANOTHER.
           
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN I-O STU-FILE.
           OPEN I-O MST-CTRL-LIST.
           
           MOVE 6 TO WS-MST-REC-KEY.
           READ MST-CTRL-LIST
               NOT INVALID KEY
                   MOVE MST-STU-ID TO WS-STU-ID
           END-READ.
           
           MOVE 'Y' TO WS-ANOTHER.
           PERFORM UNTIL ANOTHER
           
               DISPLAY NEW-SCREEN
               DISPLAY SCRN-FIELDS
               ACCEPT SCRN-STU-L-NAME
               ACCEPT SCRN-STU-F-NAME
               ACCEPT SCRN-STU-STREET
               ACCEPT SCRN-STU-ZIP
               ACCEPT SCRN-STU-PHONE
               ACCEPT SCRN-STU-STATUS
               
               DISPLAY SCRN-SAVE
               ACCEPT  SCRN-SAVE
               
               IF SAVE THEN
                   MOVE WS-STU-ID          TO STU-ID
                   MOVE WS-STU-NAME        TO STU-NAME
                   MOVE WS-STU-ADDR        TO STU-ADDR
                   MOVE WS-STU-PHONE       TO STU-PHONE
                   MOVE 'A'                TO STU-STATUS
                   
                   WRITE STU-REC
                       INVALID KEY
                           DISPLAY NEW-SCREEN
                           DISPLAY SCRN-WRITE-ERR
                           DISPLAY SCRN-ANOTHER
                           ACCEPT  SCRN-ANOTHER
                       NOT INVALID KEY
                           ADD 1 TO WS-STU-ID
                           DISPLAY NEW-SCREEN
                           DISPLAY SCRN-WRITE-SUC
                           DISPLAY SCRN-ANOTHER
                           ACCEPT  SCRN-ANOTHER
               ELSE
                   DISPLAY NEW-SCREEN
                   DISPLAY SCRN-WRITE-NOT-SAVE
                   DISPLAY SCRN-ANOTHER
                   ACCEPT  SCRN-ANOTHER
               END-IF
           END-PERFORM.
           
           MOVE WS-STU-ID TO MST-STU-ID
           REWRITE MST-NEXT-STU.
           
           CLOSE STU-FILE.
           CLOSE MST-CTRL-LIST.
           
           EXIT PROGRAM.
       
