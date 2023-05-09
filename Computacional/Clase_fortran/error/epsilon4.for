      PROGRAM epsilon
      IMPLICIT NONE
      real :: EPS = 1.0

      DO WHILE ((1.0 + EPS / 2.0) .GT. 1.0)
         EPS = EPS / 2.0
      END DO

      PRINT *, EPS

      STOP
      END PROGRAM epsilon
