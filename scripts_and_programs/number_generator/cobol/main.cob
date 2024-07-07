      *This file is an example of how to call other cobol files
      *To compile with this file as the entry point:
      *cobc -x test.cob *.cob -O2 -o numgen
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.
       PROCEDURE DIVISION.
           CALL "NUMGEN".
       END PROGRAM MAIN.
