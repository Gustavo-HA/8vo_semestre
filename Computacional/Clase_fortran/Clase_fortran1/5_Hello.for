       program hello_world
       implicit none
       
c  Es el mas simple de los programas
c Imprime texto a la pantalla
       character*32 text
c
       write (*,*)'     '
       write (*,*)'     '
       text = 'Hello World'
       write (*,*)'     '
       write (*,*)'     '
       write (*,*) text
c
       PAUSE
       STOP
       end
