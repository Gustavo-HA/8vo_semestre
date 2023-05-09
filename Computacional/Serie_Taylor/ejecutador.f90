program reescribir
    implicit none 
    character(1000) :: funcion2,archivo
    write(*,*)'Introduzca el nombre del archivo: ';read*,archivo
    write(*,'(a)')'Introduzca la funcion f(x): ';read(*,'(a)')funcion2
    call system('copy '//trim(archivo)//'.f90 '//trim(archivo)//'_1.f90')
    open(10,file=trim(archivo)//'_1.f90',access = 'append')
    write(10,*)'real*16 function f(x)'
    write(10,*)'real*16 :: x'
    write(10,*)'f = '//trim(funcion2)
    write(10,*)'return'
    write(10,*)'end function'
    close(10)
    call system('gfortran '//trim(archivo)//'_1.f90 -o '//trim(archivo)//'.exe')
    call system(trim(archivo)//'.exe')
    open(11,file=trim(archivo)//'.gpl', access = 'append')
    write(11,*)'replot '//trim(funcion2)
    close(11)
    call system(trim(archivo)//'.gpl')
end program 


