      *Skeleton COBOL Copybook
       01 HEADER.
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