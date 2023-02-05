      PROGRAM RUNFMT

      REAL  X   
   

      PARAMETER( X = 0.123456789)

      CHARACTER sTRING*80

100   CONTINUE
      WRITE(*,*) ' Enter a format string suitable for one float  '
      WRITE(*,*) ' Don''t forget the ''()'' !                    '
      READ(*,FMT='(A80)') STRING

      WRITE(*,FMT=STRING) X
      GOTO 100

      END