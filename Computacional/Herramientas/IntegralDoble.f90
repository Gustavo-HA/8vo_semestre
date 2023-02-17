!**********************************************************************
!                                                                     *
!                 Integral doble Algoritmo                             *
!                                                                     *
!**********************************************************************
!
!    Aproxima una integral doble (( F(X,Y) dY dX )) 
!    con limites de integracion A y B para X y
!    C(X) y D(X) para Y.


!    INPUT:   Extremos A,B; 
!             Enteros Positivos M, N.
!    OUTPUT:  Aproximacion J a la Integral.
!
! **************** Actividad 10 *************************    
      C(XZ)=-sqrt((2-xz**2)/2)      !Limite inferior en y (punto sobre la curva inferior)
      D(XZ)=sqrt((2-xz**2)/2)      !Limite superior en y (punto sobre la curva superior)
!    Integrando F(X,Y)      
      F(XZ,YZ)= 2-xz**2-2*yz**2 ! Integrando
      A= -sqrt(2.0)         !Limite inferior de X
      B= sqrt(2.0)          !Limite superior de X

!Numero de intervalos para evaluar la mas interna
      N=800
!Numero de intervalos para evaluar la mas externa
      M=800
      NN=2*N+1
      MM=2*M-1
!****     PASO 1  *********
      H=(B-A)/(2*N) ! incremento en eje x
!    Se definen AN, AE, AO para J(1), J(2), J(3) RESP.
!    Extremos
      AN=0
!    Terminos pares
      AE=0
!    Terminos impares
      AO=0
!*****    PASO 2
!    Se elige indice como uno
      DO 20 I=1,NN
!******   PASO 3
!       Metodo de Simpson compuesto para X
         X= A + (I-1) * H
         YA = C(X)  !Limite inferior de Y
         YB = D(X)  !Limite superior de Y
         HX=(YB-YA)/(2*M)
!       Se usa BN, BE, BO para K(1), K(2), K(3)
!       Extremos
         BN=F(X,YA)+F(X,YB)
!       Terminos pares
         BE=0
!       Terminos impares (se multiplicaran por 4)
         BO=0
!*******  PASO 4  *****

         DO 30 J=1,MM
!******** PASO 5  *****
            Y=YA+J*HX

            Z=F(X,Y)
!******** PASO 6  *****
            IF(J.EQ.2*(J/2)) THEN
               BE=BE+Z
            ELSE
               BO=BO+Z
            END IF
30       CONTINUE


!*****    PASO 7   ******
!    Se usa A1 para L

      A1=( BN+  2*BE + 4*BO)*HX/3

!******    PASO 8

      IF( I.EQ.1 .OR. I.EQ.NN ) THEN
         AN=AN+A1
      ELSE
         IF(I.EQ.2*(I/2)) THEN
             AO=AO+A1
         ELSE
             AE=AE+A1
         END IF
      END IF
20    CONTINUE

! ****   PASO 9     *****
!    Se usa A!para J

      AC=(AN+2*AE+4*AO)*H/3
!****    PASO 10    ******
!    SALIDA
      WRITE(*,1) A,B
      WRITE(*,2) AC
      WRITE(*,3) N,M

      STOP
1     FORMAT(1X,'La integral desde ',F15.8,' a ',F15.8,' es')
2     FORMAT(1X,F18.8)
3     FORMAT(1X,'obtenida con N = ',I3,' y M = ',I3)
      END

!******************************************


