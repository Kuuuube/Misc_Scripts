      *This file is an example of how to call other cobol files
      *The values in USING are passed through
      *To compile with this file as the entry point:
      *cobc -x test.cob numgen_called.cob -O2 -o numgen
      *or to compile a large number of files
      *cobc -x test.cob *.cob -O2 -o numgen
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PREFIX PIC X(50).
       01 SUFFIX PIC X(50).
       01 STARTNUMBERSTRING PIC X(25).
       01 ENDNUMBERSTRING PIC X(25).
       01 STARTNUMBER PIC 9(25).
       01 ENDNUMBER PIC 9(25).
       PROCEDURE DIVISION.
           DISPLAY "This is running in main.cob"

           DISPLAY "Enter start number:"
           ACCEPT STARTNUMBERSTRING.
           MOVE STARTNUMBERSTRING TO STARTNUMBER.
           DISPLAY "Enter end number:"
           ACCEPT ENDNUMBERSTRING.
           MOVE ENDNUMBERSTRING TO ENDNUMBER.
           DISPLAY "Enter prefix:"
           ACCEPT PREFIX.
           DISPLAY "Enter suffix:"
           ACCEPT SUFFIX.
           
           PERFORM RUN-NUMGEN.

           DISPLAY "Back to running in main.cob".
           
           STOP RUN.

       RUN-NUMGEN.
           CALL "NUMGENCALLED"
              USING
                STARTNUMBER,
                ENDNUMBER,
                PREFIX,
                SUFFIX.

       END PROGRAM MAIN.
