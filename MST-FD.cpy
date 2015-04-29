       FD  CRSE-MASTER.
       01  CRSE-REC.
           03  CRSE-ID        PIC X(9).
           03  CRSE-NAME      PIC X(35).
           03  CRSE-CREDIT    PIC X(4).
           03  CRSE-STAT      PIC X.
      
       FD  SCHED-MST.
       01  SCHED-REC.
           03  SCHEDULE-ID         PIC X(12).
           03  FILLER              PIC X.
           03  COURSE-ID           PIC X(9).
           03  FILLER              PIC X.
           03  TIMEDAY             PIC X(20).
           03  FILLER              PIC X.
           03  BUILDING-ID         PIC X(11).
           03  FILLER              PIC X.
           03  INSTRUCTOR-ID       PIC X(4).
           03  FILLER              PIC X(3).
           03  OPEN-SEATS          PIC X(2).

       FD  MST-CTRL-LIST.
       01  MST-UNIV-NAME                   PIC X(80).
       01  MST-UNIV-ADDR.
           03  MST-UNIV-ADDR-ST            PIC X(30).
           03  MST-UNIV-ADDR-ZIP           PIC 9(5).
           03  FILLER                      PIC X(45)       VALUE SPACES.
       01  MST-NEXT-CRNS.
           03  MST-NEXT-CRN-YR             PIC 9(4).
           03  MST-NEXT-CRN-SEM            PIC 9(2).
           03  MST-NEXT-CRN-CRN            PIC 9(4).
           03  FILLER                      PIC X(70)       VALUE SPACES.
       01  MST-NEXT-INST.
           03  MST-INST-ID                 PIC 9(4).
           03  FILLER                      PIC X(76)       VALUE SPACES.
       01  MST-NEXT-STU.
           03  MST-STU-ID                  PIC 9(4).
           03  FILLER                      PIC X(76)       VALUE SPACES.
       FD  BLD-MASTER.    
       1  BLD-REC.
           03  BLD-BUILDING-ROOM   PIC X(12).
           03  BLD-MAX-SEAT        PIC 99.
           
       FD  INST-MASTER.
       01  INST-REC.
           03  INST-ID    PIC 9999.
           03  INST-NAME  PIC X(22).
       FD  REG-MASTER.
       01  REG-REC.
           03  REG-KEY.
               05  REG-STU-ID          PIC 9(4).
               05  REG-YEAR            PIC 9999.
               05  REG-SEM             PIC 99.
           03  FIRST-CRN           PIC 9(4).
           03  SECOND-CRN          PIC 9(4).
           03  THIRD-CRN           PIC 9(4).
           03  FOURTH-CRN          PIC 9(4).
           03  FIFTH-CRN           PIC 9(4).
       
       FD  STU-MST.
           01  STU-REC.
               03  STU-ID           PIC 9999.
               03  STU-NAME.
                   05  STU-L-NAME       PIC X(15).
                   05  STU-F-NAME       PIC X(15).
               03  STU-ADDR.
                   05  STU-STREET       PIC X(25).
                   05  STU-ZIP          PIC XXXXX.
               03  STU-PHONE            PIC X(10).
               03  STU-STATUS           PIC X.