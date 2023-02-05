      PROGRAM Entrada

c      READ(n1,n2) 
c      n1 indica la unidad de donde se lee
c      n2 indica el formato de como leer    
      CHARACTER*15  name,outfile

      INTEGER E,F
c II. Leemos de pantalla e imprimimos a un archivo      
c Se muestra como pedir el nombre del archivo de donde se
c leeran los datos
      Write (*,'(a)') '************************* '
      Write (*,'(a)') ' '
      Write(*,'(A)')'Dame el nombre del archivo 
     & de salida(menos de 15 caracteres)'
c El simbolo amperson significa que continua el texto, se visualiza en force

      READ(*,'(A)') name
c Asterisco o (A) sirve para lo mismo, igual con a minuscula
      nletras = LEN_TRIM(name)
      outFile = name(1:nletras)//'.txt'
     
      OPEN( 9, FILE=outFile )

      Write (*,*) 'Dame dos números separados por coma'
      READ (*,*) E,F
      
      Write(9,10)E,F
   10 format(I6)
      Write (*,*) 'los números están dentro del archivo de salida'
      Write (*,'(a)') ' '
      close(9)
      STOP
      END