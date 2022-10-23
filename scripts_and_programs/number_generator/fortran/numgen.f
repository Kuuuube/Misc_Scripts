c build command (gcc 12.1.0):
c gfortran -O2 numgen.f -o numgen.exe

      PROGRAM NUMGEN

      IMPLICIT NONE

      INTEGER :: START_NUMBER
      INTEGER :: END_NUMBER
      INTEGER :: CURRENT_NUMBER
      CHARACTER (LEN = 1000) :: START_NUMBER_CHAR
      CHARACTER (LEN = 1000) :: END_NUMBER_CHAR
      CHARACTER (LEN = 1000) :: CURRENT_NUMBER_CHAR
      CHARACTER (LEN = 3000) :: FULL_STRING
      CHARACTER (LEN = 1000) :: PREFIX = ""
      CHARACTER (LEN = 1000) :: SUFFIX = ""
      
      WRITE (START_NUMBER_CHAR, '(i0)') START_NUMBER
      WRITE (END_NUMBER_CHAR, '(i0)') END_NUMBER

      WRITE (*,'(A)') "Start Number: "
      READ *,START_NUMBER
      
      WRITE (*,'(A)') "End Number: "
      READ *,END_NUMBER

      WRITE (*,'(A)') "Number Prefix: "
      READ *,PREFIX

      WRITE (*,'(A)') "Number Suffix: "
      READ *,SUFFIX

      OPEN (UNIT=1, FILE="testfile.txt")
      CURRENT_NUMBER = START_NUMBER

      DO WHILE (CURRENT_NUMBER <= END_NUMBER)
            WRITE (CURRENT_NUMBER_CHAR, '(i0)') CURRENT_NUMBER
            FULL_STRING = TRIM(PREFIX)//TRIM(CURRENT_NUMBER_CHAR)
            FULL_STRING = TRIM(FULL_STRING)//TRIM(SUFFIX)
            WRITE (1,'(A)') TRIM(FULL_STRING)
            CURRENT_NUMBER = CURRENT_NUMBER + 1
      END DO

      END