program calculopi
    implicit none
    integer :: total, colisiones=0, i
    real*8 :: propuesta, angulo, y, Ld2=0.16, d=1.0, y1, y2, auxy, auxangulo
    write(*,'(a)',advance = 'no')'Numero de iteraciones: ';read*,total

    do i = 1,total
        auxangulo = rand()
        angulo = auxangulo*3.15/2.d0
        auxy = rand()
        y = 1000.0*auxy
        y1 = y+Ld2*sin(angulo)
        y2 = y-ld2*sin(angulo)
        if ((floor(y1) .ne. floor(y)) .or. (floor(y2).ne.floor(y))) then
            colisiones = colisiones+1
        end if 
    end do
    print*,colisiones

    propuesta = (Ld2*4.d0/d) * (dble(total)/dble(colisiones))

    write(*,'(a,f18.15)')'El valor calculado para pi es:',propuesta
end program