        program comb
        implicit none
        real n,r,fact,c
        real sum,a,h,g,x,xa,b
        write(*,*)'Ingrese el orden del exponente'
        read(*,*)n
        
        write(*,*)'Ingrese el orden limites inferior y superior'
        read(*,*)a,b
        
       ! write(*,*)'Numero de intervalos'
        !read(*,*)c
        
        !h=a+b/c
        !write(*,*)'Ingrese el valor donde se calcula la derivada'
        !read(*,*)a
        

        do while (a.le.b)
        c=n**a
        write(*,*)'El valor de la combinacion es:', c

        a=a+0.1
        
        enddo


        
        pause
        stop
        end
        


