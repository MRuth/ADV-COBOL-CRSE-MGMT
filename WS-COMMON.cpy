      *Skeleton COBOL Copybook
       01 WS-DATE-TIME.
           03  WS-DATE.
               05  WS-DATE-YY        PIC 99.
               05  WS-DATE-MM        PIC 99.
               05  WS-DATE-DD        PIC 99.
           03  WS-TIME.
               05 WS-TIME-HH         PIC 99.
               05 WS-TIME-MM         PIC 99.
               05 WS-TIME-SS         PIC 99.
          
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
           03  WS-SAVE             PIC X   VALUE SPACE.
               88  SAVE                    VALUE 'Y'.
               88  NOSAVE                  VALUE 'N'.
           03  WS-ANOTHER          PIC X   VALUE 'Y'.
               88  ANOTHER                 VALUE 'N'.
           03  WS-VALIDATE         PIC X   VALUE 'N'.
               88 VALIDATED                VALUE 'Y'.
           03  WS-MST-REC-KEY      PIC 9.
           03  WS-STATUS           PIC X(60).
           03  WS-COUNTER          PIC 99 VALUE 0.
           03  WS-BLNK-LN          PIC X(80) VALUE SPACES.

