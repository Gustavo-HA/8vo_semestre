      PROGRAM epsilon
      IMPLICIT NONE
      real*8 :: EPS = 1.D0

      DO WHILE ((1.D0 + EPS / 2.D0) .GT. 1.D0)
         EPS = EPS / 2.D0
      END DO

      PRINT *, EPS

      STOP
      END PROGRAM epsilon