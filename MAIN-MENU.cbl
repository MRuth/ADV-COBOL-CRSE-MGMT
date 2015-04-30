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
      
       COPY WS-DATE-TIME.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01 CLEAR.
           03 BLANK SCREEN.
       COPY SCR-HEADER.
       01 MAIN.
           03  MENU.
               05  LINE 03 COL 40 VALUE "MAIN MENU".
               05  LINE 05 COL 32 VALUE " 1) STUDENT RECORDS".
               05  LINE 06 COL 32 VALUE " 2) COURSE RECORDS".
               05  LINE 07 COL 32 VALUE " 3) SCHEDULE".
               05  LINE 08 COL 32 VALUE " 4) INSTRUCTOR RECORDS".
               05  LINE 09 COL 32 VALUE " 5) BUILDING RECORDS".
               05  LINE 10 COL 32 VALUE " 6) ZIPCODE RECORDS".
               05  LINE 11 COL 32 VALUE " 7) REPORTS".
               05  LINE 12 COL 32 VALUE " 8) DEBUG".
               05  LINE 13 COL 32 VALUE " X) Exit".
               05  LINE 15 COL 37 VALUE "Selection".
               05  LINE 15 COL 35 PIC X TO WS-SEL AUTO.
       
       01 STU-MENU.
          03  STUMENU.
               05  LINE 03 COL 38 VALUE "STUDENT MENU".
               05  LINE 05 COL 32 VALUE " 1) LIST STUDENTS".
               05  LINE 06 COL 32 VALUE " 2) ADD STUDENT".
               05  LINE 07 COL 32 VALUE " 3) UPDATE STUDENT".
               05  LINE 08 COL 32 VALUE " 4) INQUIRE BY ID".
               05  LINE 08 COL 32 VALUE " 5) INQUIRE BY NAME".
               05  LINE 09 COL 32 VALUE " 6) REGISTER CLASS". 
               05  LINE 10 COL 32 VALUE " 7) DROP CLASS".
               05  LINE 11 COL 32 VALUE " R) RETURN TO MAIN MENU".
               05  LINE 13 COL 37 VALUE "Selection".
               05  LINE 13 COL 35 PIC X TO WS-SEL AUTO.
       
       01 CRS-MENU.
          03  CRSMENU.
               05  LINE 03 COL 39 VALUE "COURSE MENU".
               05  LINE 05 COL 32 VALUE " 1) LIST COURSES".
               05  LINE 06 COL 32 VALUE " 2) ADD COURSE".
               05  LINE 07 COL 32 VALUE " 3) UPDATE COURSE".
               05  LINE 08 COL 32 VALUE " 4) INQUIRE BY COURSE".
               05  LINE 09 COL 32 VALUE " R) RETURN TO MAIN MENU".
               05  LINE 11 COL 37 VALUE "Selection".
               05  LINE 11 COL 35 PIC X TO WS-SEL AUTO.
               
       01 SCHED-MENU.
          03  SCHEDMENU.
               05  LINE 03 COL 38 VALUE "SCHEDULE MENU".
               05  LINE 05 COL 32 VALUE " 1) LIST SCHEDULE".
               05  LINE 06 COL 32 VALUE " 2) ADD TO SCHEDULE".
               05  LINE 07 COL 32 VALUE " 3) UPDATE WITHIN SCHEDULE".
               05  LINE 08 COL 32 VALUE " 4) SEARCH BY COURSE DEPT".
               05  LINE 09 COL 32 VALUE " R) RETURN TO MAIN MENU".
               05  LINE 11 COL 37 VALUE "Selection".
               05  LINE 11 COL 35 PIC X TO WS-SEL AUTO.
               
       01 INST-MENU.
          03  INSTMENU.
               05  LINE 03 COL 37 VALUE "INSTRUCTOR MENU".
               05  LINE 05 COL 32 VALUE " 1) LIST INSTRUCTORS".
               05  LINE 06 COL 32 VALUE " 2) ADD INSTRUCTOR".
               05  LINE 07 COL 32 VALUE " 3) UPDATE INSTRUCTOR".
               05  LINE 08 COL 32 VALUE " 4) INQUIRE BY ID".
               05  LINE 09 COL 32 VALUE " R) RETURN TO MAIN MENU".
               05  LINE 11 COL 37 VALUE "Selection".
               05  LINE 11 COL 35 PIC X TO WS-SEL AUTO.
       01 BLD-MENU. 
          03  BLDMENU.
               05  LINE 03 COL 38 VALUE "BUILDING MENU".
               05  LINE 05 COL 32 VALUE " 1) LIST ROOMS".
               05  LINE 06 COL 32 VALUE " 2) ADD ROOM".
               05  LINE 07 COL 32 VALUE " 3) UPDATE ROOM".
               05  LINE 08 COL 32 VALUE " 4) VIEW ROOM".
               05  LINE 09 COL 32 VALUE " R) RETURN TO MAIN MENU".
               05  LINE 11 COL 37 VALUE "Selection".
               05  LINE 11 COL 35 PIC X TO WS-SEL AUTO.
       01  ZIP-MENU.
           03  ZIPMENU.
               05  LINE 03 COL 38 VALUE "ZIPCODE MENU".
               05  LINE 05 COL 32 VALUE " 1) SEARCH BY ZIP".
               05  LINE 06 COL 32 VALUE " 2) SEARCH BY CITY".
               05  LINE 07 COL 32 VALUE " R) RETURN TO MAIN MENU".
               05  LINE 09 COL 37 VALUE "Selection".
               05  LINE 09 COL 35 PIC X TO WS-SEL AUTO.
       01 RPT-MENU.
          03  RPTMENU.
               05  LINE 03 COL 39 VALUE "REPORT MENU".
               05  LINE 05 COL 32 VALUE " 1) COURSES BY INSTRUCTOR".
               05  LINE 06 COL 32 VALUE " 2) CLASS ROLL".

               05  LINE 07 COL 32 VALUE " R) RETURN TO MAIN MENU".
               05  LINE 09 COL 37 VALUE "Selection".
               05  LINE 09 COL 35 PIC X TO WS-SEL AUTO.
       01 DEB-MENU.
          03  DEBMENU.
               05  LINE 03 COL 39 VALUE "DEBUG MENU".
               05  LINE 05 COL 32 VALUE " 1) BUILD STUDENT".
               05  LINE 06 COL 32 VALUE " 2) BUILD COURSE".
               05  LINE 07 COL 32 VALUE " 3) BUILD INSTRUCTOR".
               05  LINE 08 COL 32 VALUE " 4) BUILD SCHEDULE".
               05  LINE 09 COL 32 VALUE " 5) BUILD BUILDING".
               05  LINE 10 COL 32 VALUE " 6) BUILD ZIP".
               05  LINE 11 COL 32 VALUE " 7) BUILD MASTER CONTROL".
               05  LINE 12 COL 32 VALUE " R) RETURN TO MAIN MENU".
               05  LINE 14 COL 37 VALUE "Selection".
               05  LINE 14 COL 35 PIC X TO WS-SEL AUTO.
      *------------------------------R----------------------------------
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
                       WHEN '6' PERFORM 260-ZIPCODE
                       WHEN '7' PERFORM 270-REPORTS
                       WHEN '8' PERFORM 280-DEBUG
                       WHEN 'X' MOVE 'Y' TO WS-EXIT
           END-EVALUATE.
                   
                   
       210-STUDENT.
               ACCEPT WS-DATE FROM DATE.
               ACCEPT WS-TIME FROM TIME.
               PERFORM UNTIL WS-SEL = "R"
                   DISPLAY HEADER, STU-MENU
                   ACCEPT STU-MENU
                   EVALUATE WS-SEL
                       WHEN '1' CALL 'STUDENT-LIST'
                       WHEN '2' CALL 'STUDENT-ADD'
                       WHEN '3' CALL 'STUDENT-UPDATE'
                       WHEN '4' CALL 'STUDENT-INQUIRY'
                       WHEN '5' CALL 'STUDENT-INQUIRY-NAME'
                       WHEN '6' CALL 'REGISTER-ADD'
                       WHEN '7' CALL 'REGISTER-DROP'
                   END-EVALUATE
              END-PERFORM.
           
           
       220-COURSE.                                                      
           ACCEPT WS-DATE FROM DATE.                                    
           ACCEPT WS-TIME FROM TIME.                                    
           PERFORM UNTIL WS-SEL = "R"                                   
               DISPLAY HEADER, CRS-MENU                                 
               ACCEPT CRS-MENU                                          
               EVALUATE WS-SEL                                          
                       WHEN '1' CALL 'COURSE-LIST'
                       WHEN '2' CALL 'COURSE-ADD'                       
                       WHEN '3' CALL 'COURSE-UPDATE'                    
                       WHEN '4' CALL 'COURSE-INQUIRY'
                       
               END-EVALUATE 
           END-PERFORM.
                 
                 
       230-SCHEDULE.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           PERFORM UNTIL WS-SEL = "R"
               DISPLAY HEADER, SCHED-MENU
               ACCEPT SCHED-MENU 
               EVALUATE WS-SEL
                       WHEN '1' CALL 'SCHEDULE-LIST'
                       WHEN '2' CALL 'SCHEDULE-ADD'
                       WHEN '3' CALL 'SCHEDULE-UPDATE'
                       WHEN '4' CALL 'SCHEDULE-INQUIRY'
               END-EVALUATE
           END-PERFORM.
           
       240-INSTRUCTOR.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           PERFORM UNTIL WS-SEL = "R"
               DISPLAY HEADER, INST-MENU
               ACCEPT INST-MENU
               EVALUATE WS-SEL
                       WHEN '1' CALL 'INSTRUCTOR-LIST'
                       WHEN '2' CALL 'INSTRUCTOR-ADD'
                       WHEN '3' CALL 'INSTRUCTOR-UPDATE'
                       WHEN '4' CALL 'INSTRUCTOR-INQUIRY'
               END-EVALUATE
           END-PERFORM.    
                 
       250-BUILDING.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           PERFORM UNTIL WS-SEL = "R"
               DISPLAY HEADER, BLD-MENU
               ACCEPT BLD-MENU
               EVALUATE WS-SEL
                       WHEN '1' CALL 'BUILDING-LIST'
                       WHEN '2' CALL 'BUILDING-ADD'
                       WHEN '3' CALL 'BUILDING-UPDATE'
                       WHEN '4' CALL 'BUILDING-INQUIRY'
               END-EVALUATE
           END-PERFORM.
           
       260-ZIPCODE.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           PERFORM UNTIL WS-SEL = "R"
               DISPLAY HEADER, ZIP-MENU
               ACCEPT ZIP-MENU
               EVALUATE WS-SEL
                       WHEN '1' CALL 'ZIPB-INQNUM'
                       WHEN '2' CALL 'ZIPC-INQCITY'
               END-EVALUATE
           END-PERFORM.
                 
       270-REPORTS.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           PERFORM UNTIL WS-SEL = "R"
               DISPLAY HEADER, RPT-MENU
               ACCEPT RPT-MENU
               EVALUATE WS-SEL
                       WHEN '1' CALL 'REPORT-COURSE-BY-INST'
                       WHEN '2' CALL 'CLASS-ROLL'
               END-EVALUATE
           END-PERFORM.
                           
                 
       280-DEBUG.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           PERFORM UNTIL WS-SEL = "R"
               DISPLAY HEADER, DEB-MENU
               ACCEPT DEB-MENU
               EVALUATE WS-SEL
                       WHEN '1' CALL 'STUDENT-MASTER'
                       WHEN '2' CALL 'COURSE-MASTER'
                       WHEN '3' CALL 'INSTRUCTOR-MASTER'
                       WHEN '4' CALL 'SCHEDULE-MASTER'
                       WHEN '5' CALL 'BUILDING-MASTER'
                       WHEN '6' CALL 'ZIPD-BLDISAM'
                       WHEN '7' CALL 'BUILD-MST-CTRL-LIST'
              END-EVALUATE
           END-PERFORM.         
       
                 
