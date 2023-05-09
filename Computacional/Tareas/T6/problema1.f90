program problema1
    implicit none
    integer  :: k, n, i, sistemaoperativo
    real*8 :: bernoulli
    character(1000) :: nombre = 'serietaylor', funcion

    write(*,*)'Que sistema operativo tiene?'
    write(*,*)'1. Linux o MacOS'
    write(*,*)'2. Windows'
2    write(*,'(a)',advance='no')'Seleccion: ';read*, sistemaoperativo
    if (sistemaoperativo .lt. 1 .or. sistemaoperativo .gt.2) then
        print*,'ERROR, seleccione 1 o 2.'
        goto 2
    end if 

    
    ! Menú 
    write(*,*)'Listado de funciones:'
    write(*,*)'1.   exp(x)*cos(x)'
    write(*,*)'2.   1/(1+9*x**2)'
    write(*,*)'3.   x/(exp(x)-1)'
    write(*,*)'4.   exp(sin(x))'
    write(*,*)'5.   exp(-x**2)'
    write(*,*)'6.   log10(1+exp(x))'
    write(*,*)'7.   log(cos(x))'
    write(*,*)'8.   (exp(x)-exp(-x))/2'
    write(*,*)'9.   cos(x)**2'
    write(*,*)'10.  log(sqrt(1+x))'
1    write(*,'(a)',advance='no')'Seleccion: ';read*,k
    if (k .lt. 1 .or. k .gt.10) then
        print*,'ERROR, seleccione una funcion entre 1 y 10.'
        goto 1
    end if 

    ! Función seleccionada
    select case (k)
    case (1)
        funcion = 'exp(x)*cos(x)'
    case (2)
        funcion = '1/(1+9*x**2)'
    case (3)
        funcion = 'x/(exp(x)-1)'
    case (4)
        funcion = 'exp(sin(x))'
    case (5)
        funcion = 'exp(-x**2)'
    case (6)
        funcion = 'log10(1+exp(x))'
    case (7)
        funcion = 'log(cos(x))'
    case (8)
        funcion = '(exp(x)-exp(-x))/2'
    case (9)
        funcion = 'cos(x)**2'
    case (10)
        funcion = 'log(sqrt(1+x))'
    end select
    
    ! Ejecuta el programa de Taylor con la función seleccionada
    call funcionyejecutar(nombre,funcion,sistemaoperativo)

    ! Esto es adicional para este problema, se inserta una línea en el .gpl del programa de Taylor
    ! para graficar también la función que se expande en series
    open(11,file=trim(nombre)//'.gpl', access = 'append')
    write(11,*)'replot '//trim(funcion)//' lc 7'
    write(11,*)'pause 10'
    close(11)
    if (sistemaoperativo .eq. 2)call system(trim(nombre)//'.gpl') ! Se ejecuta la gráfica.
    if (sistemaoperativo .eq. 1)call system('gnuplot '//trim(nombre)//'.gpl') ! Se ejecuta la gráfica.

    write(*,*)'------------ Numeros de Bernoulli ---------------'
    write(*,*)'Numeros de Bernoulli a calcular: ';read*,n
    do i = 0, n
        write(*,*)i,bernoulli(i)
    enddo

end program

    ! Versión para windows, los comandos en Linux y MacOS son distintos y deben ajustarse
subroutine funcionyejecutar(nombre,funcion,sistemaoperativo)
    implicit none
    character(1000) :: nombre, funcion
    integer :: sistemaoperativo
    ! Copia el código en otro archivo donde se insertará la función
    if (sistemaoperativo.eq.2) call execute_command_line('copy '//trim(nombre)//'.f90 '//trim(nombre)//'_mod.f90') !Windows
    if (sistemaoperativo.eq.1) call execute_command_line('cp '//trim(nombre)//'.f90 '//trim(nombre)//'_mod.f90') !Linux
    open(10,file=trim(nombre)//'_mod.f90',access = 'append')
    write(10,*)'real*16 function f(x)'
    write(10,*)'real*16 :: x'
    write(10,*)'f = '//trim(funcion)
    write(10,*)'return'
    write(10,*)'end function'
    close(10)
    ! Se compila el código con la función y se ejecuta
    if (sistemaoperativo.eq.2) then
        call system('gfortran '//trim(nombre)//'_mod.f90 -o '//trim(nombre)//'.exe')
        call system(trim(nombre)//'.exe')
    else
        call system('gfortran '//trim(nombre)//'_mod.f90 -o '//trim(nombre)//'.out')
        call system('./'//trim(nombre)//'.out') 
    endif
end subroutine

real*8 recursive function bernoulli(n) result (bern)
    implicit none
    integer :: n, k
    real*8 :: sum
    if (n.eq.0) then
        bern = 1
    else
        sum = 0
        do k = 0, n-1 
            sum = sum + bernoulli(k)/(gamma(real(k+1))*gamma(real(n+2-k)))
        end do
        bern = -gamma(real(n+1))*sum
    end if 
    return
end function