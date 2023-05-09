        program bisec
            ! Lectura del rango donde se encuentra la raiz 
            print*, 'Extremos [a, c] donde se encuentre la raiz de la funcion'
            read*, a , c
            ! Definimos la precision 
            dl = 1.e-6
            ! Rango que lo divide el eje y 
            dx = c - a 
            ! Iteracion 
            istep = 0
            write(*,'(a17,f7.3)') 'Valor de f(a) : ', f(a)
            write(*,'(a17,f7.3)') 'Valor de f(c) : ', f(c)
            ! Mientras se cumpla la precision definida 
            do 100 while (abs(dx) > dl )
                ! Parte el rango a la mitad 
                b = (a+c)/2  
                ! Mientras la raiz aun se encuntra el rango de f(a) - f(b)
                if (f(a)*f(b) < 0.0 ) then   
                    c = b 
                    ! Nuevo rango entre b y a 
                    dx = c - a 
                else 
                    a = b 
                    ! Nuevo rango entre c y a 
                    dx = c - a 
                end if 
            ! Contador 
            istep = istep + 1   
            100 end do     
            
            write(*,'(a22,i5)') 'No. de iteraciones : ', istep
            write(*,'(a14,f30.25)') 'Tolerancia : ', abs(dx)
            print*, '----------------------------------------'
            write(*,'(a8,f30.25)') 'Raiz : ', b 
            print*, '----------------------------------------'
                
            end program  bisec  
            
            function  f(x) ! Definimos la funcion 
            
            f = x**6 - 4*x**4 + x*x + 6 ! [-1,1.5]     
            ! f = -14 + 25*x - 10**x ![-1,1]    
            ! f = 5 - x*log(3*x) ![-3,3]     
            ! f = -0.2*x - 0.75 - (sin(x))**3 ! [-1.5,1.5]    
            ! f = 0.1 - x*3**(x)![-0.5,0.5]      
            ! f = cos(x-10.) - exp(3.*x) ! [-2,2]    
            ! f = x - sqrt(2.0) ![2,-2]  
            
            return 
            end function 