      ******************************************************************
      *PROGRAM : Group Project Menu                                    *
      *AUTHOR  : Cory Bailey                                           *
      *DATE    : 02-03-2014                                            *
      *ABSTRACT:                                                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-MENU.
      *----------------------------------------------------------------- 
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *----------------------------------------------------------------- 
       FILE SECTION.
      *----------------------------------------------------------------- 
       WORKING-STORAGE SECTION.
       01  WS-VARS.
           03  WS-SEL                  PIC X.
           03  WS-EXIT                 PIC X   VALUE 'N'.
      
       01 WS-DATE-TIME.
           03  WS-DATE.
               05  WS-DATE-YY        PIC 99.
               05  WS-DATE-MM        PIC 99.
               05  WS-DATE-DD        PIC 99.
           03  WS-TIME.
               05 WS-TIME-HH         PIC 99.
               05 WS-TIME-MM         PIC 99.
               05 WS-TIME-SS         PIC 99.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01 CLEAR.
           03 BLANK SCREEN.
       01 HEADER.
           03 BLANK SCREEN.
           03 LINE 01 COL 01 VALUE "TEAM MASTER CONTROL".
           
           03 LINE 01 COL 71 FROM   WS-DATE-MM.
           03 LINE 01 COL 73 VALUE  "/".
           03 LINE 01 COL 74 FROM   WS-DATE-DD.
           03 LINE 01 COL 76 VALUE  "/20".
           03 LINE 01 COL 79 FROM   WS-DATE-YY.
           03 LINE 02 COL 76 FROM   WS-TIME-HH.
           03 LINE 02 COL 78 VALUE ":".
           03 LINE 02 COL 79 FROM  WS-TIME-MM.
       01 MAIN.
           03  MENU.
               05 LINE 01 COL 38 VALUE "MAIN MENU".
               05  LINE 06 COL 32 VALUE " 1) STUDENT RECORDS".
               05  LINE 08 COL 32 VALUE " 2) COURSE RECORDS".
               05  LINE 10 COL 32 VALUE " 3) SCHEDULE".
               05  LINE 12 COL 32 VALUE " 4) INSTRUCTOR RECORDS".
               05  LINE 14 COL 32 VALUE " 5) BUILDING RECORDS".
               05  LINE 16 COL 32 VALUE " 6) REPORTS".
               05  LINE 18 COL 32 VALUE " X) Exit".
               05  LINE 20 COL 37 VALUE "Selection".
               05  LINE 20 COL 35 PIC X TO WS-SEL AUTO.
       
       01 STU-MENU.
          03  STUMENU.
               05  LINE 01 COL 38 VALUE "STUDENT MENU".
               05  LINE 06 COL 32 VALUE " 1) BUILD MASTER".
               05  LINE 08 COL 32 VALUE " 2) ADD STUDENT".
               05  LINE 10 COL 32 VALUE " 3) UPDATE STUDENT".
               05  LINE 12 COL 32 VALUE " 4) INQUIRE BY ID".
               05  LINE 14 COL 32 VALUE " 5) LIST STUDENTS".
               05  LINE 16 COL 32 VALUE " R) RETURN TO MAIN MENU".
               05  LINE 20 COL 37 VALUE "Selection".
               05  LINE 20 COL 35 PIC X TO WS-SEL AUTO.
       
       01 CRS-MENU.
          03  CRSMENU.
               05  LINE 01 COL 38 VALUE "COURSE MENU".
               05  LINE 06 COL 32 VALUE " 1) BUILD MASTER".
               05  LINE 08 COL 32 VALUE " 2) ADD COURSE".
               05  LINE 10 COL 32 VALUE " 3) UPDATE COURSE".
               05  LINE 12 COL 32 VALUE " 4) INQUIRE BY CRN".
               05  LINE 14 COL 32 VALUE " 5) INQUIRE BY COURSE".
               05  LINE 16 COL 32 VALUE " 6) LIST COURSES".
               05  LINE 18 COL 32 VALUE " R) RETURN TO MAIN MENU".
               05  LINE 20 COL 37 VALUE "Selection".
               05  LINE 20 COL 35 PIC X TO WS-SEL AUTO.
               
       01 SCHED-MENU.
          03  SCHEDMENU.
               05  LINE 01 COL 38 VALUE "SCHEDULE MENU".
               05  LINE 06 COL 32 VALUE " 1) BUILD MASTER".
               05  LINE 08 COL 32 VALUE " 2) ADD STUDENT".
               05  LINE 10 COL 32 VALUE " 3) UPDATE STUDENT".
               05  LINE 12 COL 32 VALUE " 4) INQUIRE BY ID".
               05  LINE 14 COL 32 VALUE " 5) LIST STUDENTS".
               05  LINE 16 COL 32 VALUE " R) RETURN TO MAIN MENU".
               05  LINE 20 COL 37 VALUE "Selection".
               05  LINE 20 COL 35 PIC X TO WS-SEL AUTO.
               
       01 INST-MENU.
          03  INSTMENU.
               05  LINE 01 COL 38 VALUE "INSTRUCTOR MENU".
               05  LINE 06 COL 32 VALUE " 1) BUILD MASTER".
               05  LINE 08 COL 32 VALUE " 2) ADD INSTRUCTOR".
               05  LINE 10 COL 32 VALUE " 3) UPDATE INSTRUCTOR".
               05  LINE 12 COL 32 VALUE " 4) INQUIRE BY ID".
               05  LINE 14 COL 32 VALUE " 5) LIST INSTRUCTORS".
               05  LINE 16 COL 32 VALUE " R) RETURN TO MAIN MENU".
               05  LINE 20 COL 37 VALUE "Selection".
               05  LINE 20 COL 35 PIC X TO WS-SEL AUTO.
       01 BLD-MENU.
          03  BLDMENU.
               05  LINE 01 COL 38 VALUE "BUILDING MENU".
               05  LINE 06 COL 32 VALUE " 1) BUILD MASTER".
               05  LINE 08 COL 32 VALUE " 2) ADD ROOM".
               05  LINE 10 COL 32 VALUE " 3) UPDATE ROOM".
               05  LINE 12 COL 32 VALUE " 4) VIEW ROOM".
               05  LINE 14 COL 32 VALUE " 5) LIST ROOMS".
               05  LINE 16 COL 32 VALUE " R) RETURN TO MAIN MENU".
               05  LINE 20 COL 37 VALUE "Selection".
               05  LINE 20 COL 35 PIC X TO WS-SEL AUTO.
       
       01 RPT-MENU.
          03  RPTMENU.
               05  LINE 01 COL 38 VALUE "REPORT MENU".
               05  LINE 06 COL 32 VALUE " 1) BUILD MASTER".
               05  LINE 08 COL 32 VALUE " 2) ADD STUDENT".
               05  LINE 10 COL 32 VALUE " 3) UPDATE STUDENT".
               05  LINE 12 COL 32 VALUE " 4) INQUIRE BY ID".
               05  LINE 14 COL 32 VALUE " 5) LIST STUDENTS".
               05  LINE 16 COL 32 VALUE " R) RETURN TO MAIN MENU".
               05  LINE 20 COL 37 VALUE "Selection".
               05  LINE 20 COL 35 PIC X TO WS-SEL AUTO.
      *----------------------------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.

          
           PERFORM UNTIL WS-EXIT = "Y"
               DISPLAY CLEAR
               PERFORM 200-MAIN
           END-PERFORM.
           
           STOP RUN.
                
                   
        
                    
       200-MAIN.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           DISPLAY HEADER,MAIN.
           ACCEPT MAIN.
           EVALUATE WS-SEL
                       WHEN '1' PERFORM 210-STUDENT
                       WHEN '2' PERFORM 220-COURSE
                       WHEN '3' PERFORM 230-SCHEDULE
                       WHEN '4' PERFORM 240-INSTRUCTOR                      
                       WHEN '5' PERFORM 250-BUILDING
                       WHEN '6' PERFORM 260-REPORTS
                       WHEN 'X' MOVE 'Y' TO WS-EXIT
           END-EVALUATE.
                   
                   
           210-STUDENT.
               ACCEPT WS-DATE FROM DATE.
               ACCEPT WS-TIME FROM TIME.
               PERFORM UNTIL WS-SEL = "R"
                   DISPLAY HEADER, STU-MENU
                   ACCEPT STU-MENU
                   EVALUATE WS-SEL
                       WHEN '1' CALL 'STUDENT-MASTER'
                       WHEN '2' CALL 'STUDENT-ADD'
                       WHEN '3' CALL 'STUDENT-UPDATE'
                       WHEN '4' CALL 'STUDENT-INQUIRY'
                       WHEN '5' CALL 'STUDENT-LIST'
                   END-EVALUATE
              END-PERFORM.
           
           
       220-COURSE.                                                      
           ACCEPT WS-DATE FROM DATE.                                    
           ACCEPT WS-TIME FROM TIME.                                    
           PERFORM UNTIL WS-SEL = "R"                                   
               DISPLAY HEADER, CRS-MENU                                 
               ACCEPT CRS-MENU                                          
               EVALUATE WS-SEL                                          
                       WHEN '1' CALL 'COURSE-MASTER'
                       WHEN '2' CALL 'COURSE-ADD'                       
                       WHEN '3' CALL 'COURSE-UPDATE'                    
                       WHEN '5' CALL 'COURSE-INQUIRY'
                       WHEN '6' CALL 'COURSE-LIST'
               END-EVALUATE
           END-PERFORM.
                 
                 
       230-SCHEDULE.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           PERFORM UNTIL WS-SEL = "R"
               DISPLAY HEADER, SCHED-MENU
               ACCEPT SCHED-MENU
               EVALUATE WS-SEL
                       WHEN '1' PERFORM 210-STUDENT
               END-EVALUATE
           END-PERFORM.
           
       240-INSTRUCTOR.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           PERFORM UNTIL WS-SEL = "R"
               DISPLAY HEADER, INST-MENU
               ACCEPT INST-MENU
               EVALUATE WS-SEL
                       WHEN '1' CALL 'INSTRUCTOR-MASTER'
                       WHEN '2' CALL 'INSTRUCTOR-ADD'
                       WHEN '3' CALL 'INSTRUCTOR-UPDATE'
                       WHEN '4' CALL 'INSTRUCTOR-INQUIRY'
                       WHEN '5' CALL 'INSTRUCTOR-LIST'
               END-EVALUATE
           END-PERFORM.    
                 
       250-BUILDING.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           PERFORM UNTIL WS-SEL = "R"
               DISPLAY HEADER, BLD-MENU
               ACCEPT BLD-MENU
               EVALUATE WS-SEL
                       WHEN '1' CALL 'BUILDING-MASTER'
                       WHEN '2' CALL 'BUILDING-ADD'
                       WHEN '3' CALL 'BUILDING-UPDATE'
                       WHEN '4' CALL 'BUILDING-INQUIRY'
                       WHEN '5' CALL 'BUILDING-LIST'
               END-EVALUATE
           END-PERFORM.
                 
       260-REPORTS.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           PERFORM UNTIL WS-SEL = "R"
               DISPLAY HEADER, RPT-MENU
               ACCEPT RPT-MENU
               EVALUATE WS-SEL
                       WHEN '1' PERFORM 210-STUDENT
               END-EVALUATE
           END-PERFORM.
                           
                 
                 
       
                 
