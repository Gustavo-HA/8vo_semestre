      PROGRAM EntradaTeclado
c  Se muestra como leer e imprimir datos 
c en un programa de fortran.

c      READ(n1,n2) 
c      n1 indica la unidad de donde se lee
c      n2 indica el formato de como leer
      CHARACTER*15 :: name,name1, geofile,outfile, outFile1
      CHARACTER*15 :: geoFile1
      INTEGER :: A,B,C,D,E,F, H, J

c I. Leemos de pantalla e imprimimos a un archivo
c Se muestra como pedir el nombre del archivo de donde se
c leeran los datos
      Write (*,'(a)') '************************* '
      Write (*,'(a)') ' '
      Write(*,'(A)')'Nombre del archivo de salida(max. 15 caracteres)'

      READ(*,'(A)') name

      nletras = LEN_TRIM(name)
      outFile = name(1:nletras)//'.txt'
     
      OPEN( 9, FILE=outFile )

      Write (*,*) 'Dame dos numeros separados por coma'
      READ (*,*) E,F
      
      Write(9,10)E,F
   10 format(I6)
      Write (*,'(a)') ' '
      Write (*,*) 'los numeros estan dentro del archivo de salida'
      Write (*,'(a)') ' '
      close(9)

      PAUSE
      STOP
      END
