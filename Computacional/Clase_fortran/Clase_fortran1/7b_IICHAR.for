      program IICHAR
      open(9,file='output.dat')


        write (*,*)Ichar('e'),Ichar('H')
        write (9,*)Ichar('e'),Ichar('H')
        
        write (*,*)char(32)
        write (9,*)char(32)
    1 continue
       write (*,*)' '
       write (*,*)'Se imprimio al archivo:output.dat'
       write (*,*)' '
        PAUSE
       stop 
        end
       
