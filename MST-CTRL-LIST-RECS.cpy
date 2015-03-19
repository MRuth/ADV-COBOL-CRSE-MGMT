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


