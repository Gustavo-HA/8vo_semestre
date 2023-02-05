c
c
c Este programa muestra como utilizar variables logicas
c
c___________________________________________________________________
c___Declaracion de las variables logicas
      logical flag1,flag2,flag3, flag4
c___Asignacion de variables logicas
      flag1 = .true.
      flag2 = .false.
c___Operaciones con variables logicas
      flag3 = flag1 .and. flag2
      flag4 = flag1 .or. flag2
c___Escritura por
      write (6,*) 'var1= ',  flag1
      write (6,*) 'var2= ', flag2
      write (6,*) 'var1. and. var2=', flag3
       write (6,*) 'var1.or.var2= ', flag4
c pantalla

      PAUSE
      stop
      end
