       program tablas
        implicit none
        integer j,k
        write(*,*) 'Tablas de multiplicar: '
        do  j = 1,5,1
            do  k = 1,3,1
                write(*,*) j,'x',k,'=',(j*k)
      end do
            end do
        
        stop 
        end
