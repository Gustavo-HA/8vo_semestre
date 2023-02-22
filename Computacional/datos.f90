program pasararchivo
    implicit none
    character*9 ::hola='hola',caca='caca'
    character(100):: combinacion
    real*8 :: pi

    pi = 4.d0*atan(1.d0)
    combinacion = trim(hola)//trim(caca)
    write(*,'(a)')combinacion
    write(combinacion,'(f18.16)')pi
    write(*,'(a)')combinacion
end program