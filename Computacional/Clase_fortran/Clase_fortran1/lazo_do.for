        program lazodo
        implicit none
        integer i,resultado
        
        do i=1,100
            resultado = i**2
            write(*,33)i,resultado
        end do
  33   format(11x,'N=',I3,'M=',i6)
        
        STOP
        end