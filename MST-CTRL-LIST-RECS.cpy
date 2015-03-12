       FD  MST-CTRL-LIST.
       01  UNIV-NAME                   PIC X(80).
       01  UNIV-ADDR.
           03  UNIV-ADDR-ST            PIC X(30).
           03  UNIV-ADDR-ZIP           PIC 9(5).
           03  FILLER                  PIC X(45).
       01  NEXT-CRNS.
           03  NEXT-CRN-YR             PIC 9(4).
           03  NEXT-CRN-SEM            PIC 9(2).
           03  NEXT-CRN-CRN            PIC 9(4).
           03  FILLER                  PIC X(70).
       01  NEXT-INST.
           03  INST-ID                 PIC 9(4).
           03  FILLER                  PIC X(76).
       01  NEXT-STU.
           03  STU-ID                  PIC 9(4).
           03  FILLER                  PIC X(76).
               
           
      


