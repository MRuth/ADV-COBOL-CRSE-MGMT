       SELECT BLD-MASTER   ASSIGN        TO
                               '../FILES/BUILDING-MASTER.DAT'   
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS BLD-BUILDING-ROOM
                               FILE STATUS   IS WS-STAT.


       SELECT SCHED-MST    ASSIGN        TO 
                                       '../FILES/SCHEDULE-MST.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS SCHEDULE-ID
                               FILE STATUS   IS WS-STAT.
                               
       SELECT MST-CTRL-LIST    ASSIGN TO
                                       "../Files/MST-CTRL-LST.DAT"
                               ORGANIZATION IS RELATIVE
                               ACCESS IS RANDOM
                               RELATIVE KEY IS WS-MST-REC-KEY 
                               FILE STATUS IS WS-STAT.
       SELECT CRSE-MASTER  ASSIGN  TO 
                                       '../FILES/COURSE-MASTER-SORT.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS CRSE-ID
                               FILE STATUS   IS WS-STAT.
       
      
       SELECT INST-MASTER  ASSIGN        TO 
                                       '../FILES/INSTR-MASTER.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS INST-ID
                               ALTERNATE KEY IS INST-NAME
                               FILE STATUS   IS WS-STAT.
                               
       SELECT REG-MASTER   ASSIGN        TO 
                               '../FILES/REGISTER-MASTER.DAT'   
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS SEQUENTIAL
                               RECORD KEY    IS REG-KEY
                               FILE STATUS   IS WS-STAT.

       SELECT STU-MST         ASSIGN TO 
                                   '../FILES/STUDENT-MASTER.DAT'
                                   ORGANIZATION    IS INDEXED
                                   ACCESS          IS RANDOM
                                   RECORD KEY      IS STU-ID
                                   ALTERNATE KEY   IS STU-NAME
                                       WITH DUPLICATES
                                   FILE STATUS     IS WS-STAT.