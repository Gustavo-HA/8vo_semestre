      PROGRAM epsilon
      IMPLICIT NONE
      DOUBLE PRECISION :: EPS = 1.D0

      DO WHILE ((1.D0 + EPS / 2.D0) .GT. 1.D0)
         EPS = EPS / 2.D0
      END DO

      PRINT *, EPS

      PAUSE
      STOP
      END PROGRAM epsilon
