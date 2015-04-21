	  ******************************************************************
	  *PROGRAM :  STUDENT-UPDATE                                       *
	  *AUTHOR  : MONTANA RUTH                                          *
	  *DATE    : Apr 21, 2015                                          *
	  *ABSTRACT:                                                       *
	  ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-UPDATE.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STU-MST         ASSIGN TO 
                                   '../FILES/STUDENT-MASTER.DAT'
                                   ORGANIZATION    IS INDEXED
                                   ACCESS          IS DYNAMIC
                                   RECORD KEY      IS STU-ID
                                   FILE STATUS     IS WS-STAT.
                                   
           SELECT ZIP-MST      ASSIGN TO "../FILES/ZIPMASTER.DAT"
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS DYNAMIC
                               RECORD KEY    IS ZIP-KEY
                               ALTERNATE KEY IS ZIP-CITY
                                   WITH DUPLICATES
                               FILE STATUS   IS WS-ZIP-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY STU-MST-DEF.
       COPY ZIP-MST-DEF.
       
       WORKING-STORAGE SECTION.
       01  WS-RESP                 PIC X.
       01  WS-STAT                 PIC 99.
       01  WS-MST-REC-KEY          PIC 99.
       01  WS-ZIP-STAT             PIC 99.
       01  WS-EOF                  PIC X       VALUE 'N'.
           88  EOF                             VALUE 'Y'.
       01  WS-SAVE                 PIC X       VALUE SPACES.
           88  SAVE                            VALUE 'Y'.
           88  NO-SAVE                         VALUE 'N'.
       01  WS-ANOTHER              PIC X       VALUE SPACES.
           88  ANOTHER                         VALUE 'Y'.
           88  NONE                            VALUE 'N'.
       01  WS-DTL-LN.
               03  WS-STU-ID               PIC 9999.
               03  WS-STU-NAME.
                   05  WS-STU-L-NAME       PIC X(15).
                   05  WS-STU-F-NAME       PIC X(15).
               03  WS-STU-ADDR.
                   05  WS-STU-STREET       PIC X(25).
                   05  WS-STU-ZIP          PIC XXXXX.
               03  WS-STU-PHONE.
                   05  WS-STU-PHONE-1      PIC XXX.
                   05  WS-STU-PHONE-2      PIC XXX.
                   05  WS-STU-PHONE-3      PIC XXXX.
               
       SCREEN SECTION.
       01  NEW-SCREEN.
           03  BLANK SCREEN.
           03  LINE 1 COL 34                   VALUE 'ADD NEW STUDENT'.
       01  SCRN-STU-ID-INQUIRE.
           03      LINE 3  COL 25                  VALUE 'STUDENT ID:'.
           03              COL 43      PIC 9999    TO WS-STU-ID.
       01  SCRN-FIELDS.
           03  SCRN-STU-ID.
               05  LINE 3  COL 25                  VALUE
                                                   'STUDENT ID: '.
               05          COL 43      PIC 9999    FROM WS-STU-ID.
           03  SCRN-STU-NAME.
               05  SCRN-STU-L-NAME.
                   07  LINE 5   COL 25              VALUE
                                                   'STUDENT L-NAME: '.
                   07          COL 43  PIC X(15)   USING WS-STU-L-NAME
                                                   REQUIRED.
               05  SCRN-STU-F-NAME.
                   07  LINE 6  COL 25              VALUE
                                                   'STUDENT F-NAME: '.
                   07          COL 43  PIC X(15)   USING WS-STU-F-NAME
                                                   REQUIRED.
           03  SCRN-STU-ADDR.
               05  SCRN-STU-STREET.
                   07  LINE 8  COL 25              VALUE
                                                   'STUDENT STREET: '.
                   07          COL 43  PIC X(25)   USING WS-STU-STREET
                                                   REQUIRED.
               05  SCRN-STU-ZIP.
                   07  LINE 9  COL 25              VALUE
                                                   'STUDENT ZIP   : '.
                   07          COL 43  PIC XXXXX   USING WS-STU-ZIP
                                                   AUTO REQUIRED.
               05  SCRN-STU-CITY-ST.
                   07  LINE 11 COL 35              VALUE 'CITY: '.
                   07          COL 43  PIC X(30)   FROM ZIP-CITY.
                   
                   07  LINE 12 COL 35              VALUE 'ST  : '.
                   07          COL 43  PIC XX      FROM ZIP-STATE.
                   
           03  SCRN-STU-PHONE.
               05  LINE 14 COL 25                  VALUE
                                                   'STUDENT PHONE : '.
               05          COL 43      PIC XXX     USING WS-STU-PHONE-1
                                                   AUTO REQUIRED.
               05          COL 46                  VALUE '-'.
               05          COL 47      PIC XXX     USING WS-STU-PHONE-2
                                                   AUTO REQUIRED.
               05          COL 50                  VALUE '-'.
               05          COL 51      PIC XXXX    USING WS-STU-PHONE-3
                                                   AUTO REQUIRED.
           03  SCRN-SAVE.
               05  LINE 16 COL 32                  VALUE 'SAVE (Y/N)'.
               05          COL 30      PIC X       TO WS-SAVE
                                                   AUTO.
       01  SCRN-WRITE-ERR-1.
           03  LINE 3  COL 30  VALUE 
                   'A STUDENT ALREADY EXISTS WITH THIS ID'.
       01  SCRN-WRITE-SUC.
           03  LINE 3  COL 30  VALUE 'STUDENT HAS BEEN UPDATED'.
       01  SCRN-WRITE-NOT-SAVE.
           03  LINE 3  COL 30  VALUE 'STUDENT HAS NOT BEEN UPDATED'.    
       01  SCRN-ANOTHER.
           03  LINE 5  COL 32  VALUE 'UPDATE ANOTHER? (Y/N)'.
           03          COL 30          PIC X       TO WS-ANOTHER
                                                   AUTO.
           
       PROCEDURE DIVISION.
       000-MAIN.
       
           OPEN I-O STU-MST.
           OPEN INPUT ZIP-MST.
           
           MOVE SPACES TO WS-ANOTHER.
           
           PERFORM UNTIL NONE

               MOVE SPACES TO WS-ANOTHER
               MOVE SPACES TO WS-SAVE
               MOVE SPACES TO WS-DTL-LN
               
               
               
               PERFORM UNTIL SAVE OR NO-SAVE
               
                   DISPLAY NEW-SCREEN
                   DISPLAY SCRN-FIELDS
                   PERFORM 200-GET-STU-REC
                   ACCEPT SCRN-STU-L-NAME
                   ACCEPT SCRN-STU-F-NAME
                   ACCEPT SCRN-STU-STREET
                   ACCEPT SCRN-STU-ZIP
                   PERFORM 100-GET-CITY-ST
                   ACCEPT SCRN-STU-PHONE
                   
                   
                   DISPLAY SCRN-SAVE
                   ACCEPT  SCRN-SAVE
               
               END-PERFORM
               
               IF SAVE THEN
                   MOVE WS-STU-ID          TO STU-ID
                   MOVE WS-STU-NAME        TO STU-NAME
                   MOVE WS-STU-ADDR        TO STU-ADDR
                   MOVE WS-STU-PHONE       TO STU-PHONE
                   MOVE 'A'                TO STU-STATUS
                   
                   REWRITE STU-REC
                       INVALID KEY
                           DISPLAY NEW-SCREEN
                           DISPLAY SCRN-WRITE-ERR-1
                       NOT INVALID KEY
                           ADD 1 TO WS-STU-ID
                           DISPLAY NEW-SCREEN
                           DISPLAY SCRN-WRITE-SUC
               ELSE
                   DISPLAY NEW-SCREEN
                   DISPLAY SCRN-WRITE-NOT-SAVE
               END-IF
               PERFORM UNTIL ANOTHER OR NONE
                   DISPLAY SCRN-ANOTHER
                   ACCEPT  SCRN-ANOTHER
               END-PERFORM
           END-PERFORM.
           
           
           CLOSE   STU-MST,
                   ZIP-MST.
           
           EXIT PROGRAM.
       
       
       100-GET-CITY-ST.
       MOVE WS-STU-ZIP TO ZIP-KEY.
       START ZIP-MST KEY EQUAL TO ZIP-KEY
               INVALID KEY
                   MOVE "RECORD NOT FOUND" TO ZIP-CITY
                   MOVE SPACES TO ZIP-COUNTY
                                  ZIP-STATE
               NOT INVALID KEY
                   READ ZIP-MST
       END-START
       DISPLAY SCRN-FIELDS.
       
       200-GET-STU-REC.
           DISPLAY SCRN-STU-ID-INQUIRE.
           ACCEPT  SCRN-STU-ID-INQUIRE.
           MOVE WS-STU-ID  TO STU-ID.
           
           START STU-MST KEY EQUAL TO STU-ID
               INVALID KEY
               NOT INVALID KEY
           END-START.
           
       