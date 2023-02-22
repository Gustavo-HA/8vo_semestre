program calculopicuadrado
    implicit none
    integer :: i, totales, dentro=0
    real*8 :: x1, y1, xc = 0.5d0 , yc = 0.5d0, propuesta, distance
    write(*,'(a)',advance = 'no')'Numero de iteraciones: ';read*,totales

    do i = 1,totales
        x1 = rand()
        y1 = rand()
        distance = ((xc-x1)**2.d0+(yc-y1)**2.d0)**(1.d0/2.d0)
        if (distance .lt. 0.5) dentro = dentro + 1
    enddo

    propuesta = 4.d0*(dble(dentro)/dble(totales))
    write(*,'(a,f18.15)')'El valor calculado para pi es:',propuesta
end