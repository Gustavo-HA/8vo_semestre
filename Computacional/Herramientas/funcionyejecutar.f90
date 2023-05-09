program funcionyejecutar
    implicit none
    character(1000) :: nombre, funcion
    write(*,*)'Introduzca el nombre del archivo: ';read*,nombre
    write(*,'(a)')'Introduzca la funcion f(x): ';read(*,'(a)')funcion
    call system('copy '//trim(nombre)//'.f90 '//trim(nombre)//'_mod.f90')
    open(10,file=trim(nombre)//'_mod.f90',access = 'append')
    write(10,*)'real*16 function f(x)'
    write(10,*)'real*16 :: x'
    write(10,*)'f = '//trim(funcion)
    write(10,*)'return'
    write(10,*)'end function'
    close(10)
    call system('gfortran '//trim(nombre)//'_mod.f90 -o '//trim(nombre)//'.exe')
    call system(trim(nombre)//'.exe')
end program