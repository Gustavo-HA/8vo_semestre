      PROGRAM Entrada
c  Se muestra como leer e imprimir datos 
c en un programa de fortran.

c      READ(n1,n2) 
c      n1 indica la unidad de donde se lee
c      n2 indica el formato de como leer

      INTEGER A,B,C,D
       
c II. leemos e imprimimos a la pantalla
      Write (*,*) 'Dame dos numeros separados por coma'
      READ (*,*) A,B
      Write (*,*) 'Los numeros dados son:'
      Write (*,*)A,B
c Mostrando otra sintaxis valida
      Write (*,'(a)') 'Dame dos numeros separados por coma'
      READ (*,*) C,D
      Write (*,'(a)') 'Los numeros dados son:'
      Write (*,*)C,D
      PAUSE
      STOP
      END
