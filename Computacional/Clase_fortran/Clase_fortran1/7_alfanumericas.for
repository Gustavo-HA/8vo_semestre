c
c Este programa muestra como utilizar variables alfanumericas
c
c___________________________________________________________________
c___Declaracion de las variables alfanumericas
      character*1 espacio
      character*5 cad1,cad2
      character*7 cad3
      character*19 cad_total
c___Asignacion de variables alfanumericas
      espacio = ' '
      cad1= 'seat'
      cad2= 'panda'
      cad3= '16v GTI'
      
c___Operacion con variables alfanumericas
      cad_total = cad1//espacio//cad2//espacio//cad3
c_las diagonales son para concatenar strings
c___Escritura por pantalla de los resultados
      write (*,*) ''
      write (*,*) 'cadena1 = ',cad1
      write (*,*) 'cadena2 = ',cad2
      write (*,*) 'cadena3 = ',cad3
      write (*,*) ''
      write (*,*) 'Cadenas concatenadas:'
      write (6,*) cad_total
c_6 puede ser pantalla
      PAUSE
      stop
      end