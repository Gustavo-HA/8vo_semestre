      PROGRAM paracaidista
c Calcula la velocidad terminal de un paracaidista.
c Formula:
c v(t+1) = V(t)+ [g-(c/m)*v(t)] (t+1-t)
c
c      IMPLICIT NONE
c Se inicializan valores de variables y tipos
        
        REAL :: g= 9.8      !aceleracion gravedad m/s^2
        REAL :: c=12.5      !coeficiente de arrastre en Kg/s
        REAL :: m=68.1      !masa paracaidista Kg

        REAL V,t,paso,y, errorRP
        INTEGER :: i,sum, intervalos !contador del lazo
        CHARACTER*15  name,outfile
        DIMENSION V(100),t(100), y(100)

c Se obtiene tiempo final
        WRITE(*,*) "Entra el numero de intervalos (entero): "
        READ(*,*) intervalos
        WRITE(*,*) "Entra el tiempo del intervalo: "
        READ(*,*) paso

        Write (*,'(a)') '************************* '
        Write (*,'(a)') ' '
        Write(*,'(A)')'Nombre del archivo de salida'

       READ(*,'(A)') name

       nletras = LEN_TRIM(name)
       outFile = name(1:nletras)//'.txt'
     
       OPEN( 9, FILE=outFile )
c Condiciones iniciales
        v(1)=0.0
        t(1)=0.0 
        y(1)=0.0 
        sum=0

c       paso se define como: t(i+1)-t(i)
        WRITE(*,*) "paso es:",paso
        WRITE(*,*) "Tiempo final es:",paso*intervalos
        WRITE(*,*) " "
c  Imprimimos los nombres de las etiquetas
        WRITE(*,*)"tiempo(s)     ","       v(i):         v(i+1):",
     &  "       vel. analit:","     Error rel.%"


c Se hace un lazo DO
        DO i = 1, intervalos
c  Calculamos con la expresion analitica para compararla
c con la aproximacion:
c  Velocidad esta definida como y
c  
        y(i+1)=53.39* (1.0-exp(-0.18355*paso*i))
!        y(i+1)=(g*m/c)* (1.0-exp(-c*m*paso*i))

c forma numerica
   
      v(i+1)= V(i)+ (g-(c/m)*v(i))*paso  !(t(i+1)-t(i))
      v(i+1)=sum+V(i+1)
c Calculo del error relativo porcentual
c error%= (VT-VA)/VT)*100%
        errorRP= (y(i+1)-v(i+1))/y(i+1)*100.0
        
      WRITE(*,*)paso*i,v(i),v(i+1),y(i+1),errorRP
      Write(9,10)paso*i,v(i+1)   !numerica
c      Write(9,10)paso*i,y(i+1)  ! analitica
   10 format(2F6.2)
        END DO       
c Imprimimos los valores analiticos
c de la velocidad
        DO i = 1, intervalos
        Write(9,10)paso*i,y(i+1)
        end do
        
        STOP
        END 
