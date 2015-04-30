       01  HEADER. 
           03 BLANK SCREEN.
           03 LINE 01 COL 01 VALUE "TEAM MASTER CONTROL".
           03 LINE 01 COL 38 VALUE "UA FORT SMITH".
           03 LINE 01 COL 71 FROM   WS-DATE-MM.
           03 LINE 01 COL 73 VALUE  "/".
           03 LINE 01 COL 74 FROM   WS-DATE-DD.
           03 LINE 01 COL 76 VALUE  "/20".
           03 LINE 01 COL 79 FROM   WS-DATE-YY.
           03 LINE 02 COL 36 VALUE "COURSE MANAGEMENT".
           03 LINE 02 COL 76 FROM   WS-TIME-HH.
           03 LINE 02 COL 78 VALUE ":".
           03 LINE 02 COL 79 FROM  WS-TIME-MM.
       
       01  SCRN-SV.
           03  SCRN-SAVE.
               05  LINE 22  COL 38  VALUE   'SAVE (Y/N)'.
               05           COL  36  PIC X     TO WS-SAVE 
                                             AUTO.
           03  SCRN-SAVED.
               05  LINE 22  COL 32  VALUE   '      INPUT SAVED'.
           03  SCRN-CANCEL. 
               05  LINE 22  COL 32  VALUE   '    INPUT NOT SAVED'.
           03  SCRN-SAVE-ERROR.
               05  LINE 22  COL 32  VALUE   'INVALID INPUT. NOT SAVED'.
       01  SCRN-ANOTHER.
           03  LINE 23  COL 38  VALUE 'ANOTHER? (Y/N)'.
           03           COL 36  PIC X TO WS-ANOTHER
                                            AUTO.
       01  SCRN-STATUS. 
           03  LINE 24 COL 1  FROM WS-STATUS.