      PROGRAM ROMBERGINTEGRAL
      DIMENSION R(20,20)

! Limites de integracion
      A=0.0
      B=3.14159
      N=20
!      OPEN(2,file='OA10.DAT')
!      READ(1,*)A,B,N
      CALL ROMBERG(A,B,N,R)
      END PROGRAM


      SUBROUTINE ROMBERG(A,B,N,R)
!      F(X)=cos(x)/(x+1.0) !Funcion
!       F(X)=(log (x))**3.0
!      F(X)= log (x)*log (x+1.0)
       F(x)=sin(x)

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
!
!      DO I=1,N
!      WRITE(2,20)(R(I,J),J=1,I)
!  20  FORMAT(6(2X,F12.6))

      DO I=1,N
       WRITE(*,20)(R(I,1))
  20  FORMAT(6(2X,F12.6))
      END DO

      END SUBROUTINE