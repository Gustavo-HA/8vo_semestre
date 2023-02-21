program fact 

    integer :: n , j, a(0:20000) = 0, len , c , num 

    ! Se define el número a sacar el factorial, el contador de las multiplicaciones iniciando en 2,
    ! El primer elemento del arreglo, de forma auxiliar, se escoje a 1. 
    ! Otras variables auxiliares c y num se definen como 0 al inicio de cada ciclo.
    write(*,'(a)',advance='no')'Factorial del numero: ';read*,n
    j = 2 
    a(0) = 1 
    len  = 1
    c = 0 
    num = 0 

    ! Se inicia el ciclo while que se mantiene hasta que se realicen todas las multiplicaciones

    do while (j <= n)
        c = 0 
        num = 0          

        ! Se inicializa un segundo ciclo que introduce en el arreglo el c-ésimo dígito en cada ciclo.
        do while (c < len)
            a(c) = a(c)*j
            a(c) = a(c) + num 
            num = a(c)/10
            a(c) = mod(a(c),10)
            c = c + 1                       
        end do 
        
        ! En este ciclo se modifica el último elemento del arreglo en el ciclo actual, el cual seguirá
        ! cambiando conforme continúe el ciclo mayor.
        do while (num /=  0)
            a(len) = mod(num,10)
            num = num/10
            len = len + 1 
        end do 
        j = j + 1                     
    end do     
    
    ! Se imprimen los resultados desde el último dígito hasta el primero, cuidando
    ! que la variable len no sea mayor al tamaño del arreglo
    len =  len  - 1 
    do while (len >= 0)
        write(*,'(i0)',advance='no') a(len)
        len = len - 1
    end do 

end program fact 