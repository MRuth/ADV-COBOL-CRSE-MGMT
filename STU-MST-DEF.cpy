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