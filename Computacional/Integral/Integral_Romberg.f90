PROGRAM ROMBERGINTEGRAL
      DIMENSION R(20,20)
      A=0.0
      B=10
      N=20
      CALL ROMBERG(A,B,N,R)
END PROGRAM

SUBROUTINE ROMBERG(A,B,N,R)
      REAL::R(20,20)
      WRITE(*,*)' INTEGRACION DE ROMBERG ' 
      
      H=B-A
      R(1,1)=(H/2.0)*(F(A)+F(B)) !Primer termino

      DO I=2,N
            S=0
            DO K=1,2**(I-2)
                  S=S+F(A+((K-0.5)*H))
            END DO
            R(I,1)=0.5*(R(I-1,1) + (H*S)) !R(2,1) y demas
            H=H/2.0
      END DO

! Aqui hace la extrapolacion de Richardson
      DO J=2,N
        DO I=J,N
            R(I,J)=R(I,J-1)+((R(I,J-1)-R(I-1,J-1))/(4**(J-1))-1)
        END DO
      END DO

      DO I=1,N
       WRITE(*,20)(R(I,1))
  20  FORMAT(6(2X,F12.6))
      END DO
      END SUBROUTINE

function f(c)
      real, parameter :: g = 9.80, m = 68.1, t = 10
      F = g*m/c * (1-exp(-c*t/m))
      return
end