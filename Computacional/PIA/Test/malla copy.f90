program malla_trig 
  integer, parameter :: nx = 21                     ! Espaciado de los nodos en x (hay 2*nx+1) 
  integer, parameter :: ny = 7                      ! Espaciado de los nodos en y (hay 2*ny+1) 
  integer, parameter :: mx = 2*nx -1 , my = 2*ny -1 ! Numero de nodos en x , y 
  integer, parameter :: nquad = 3                   ! El número de puntos de cuadratura por elemento.
  integer, parameter :: nelemn = 2*(nx - 1)*(ny -1) ! Numero de elementos 
  integer, parameter :: nnodes = 6                  ! No. nodos por elemento (6 triangulares cuadráticos)
  integer, parameter :: np = mx*my                  ! Numero de nodos  

  integer :: indx(np, 2)          ! Contiene para cada iesimo nodo el index de la velocidad u, v (Puede 0)
  integer :: insc(np)             ! Contiene para cada iesimo nodo el index de la presion p (Puede 0)
  integer :: isotri(nelemn)       ! Contiene para cada elemento 1 si es isoparametrico o 0 
  integer :: neqn                 ! Numero de ecuaciones y funciones
  integer :: node(nelemn,nnodes)  ! Contiene por cada elemento el ind global de cada elemento del nodo

  real(8) :: area(nelemn)         ! Area total de los elementos 
  real(8) :: xc(np)               ! Coordenadas x de los nodos 
  real(8) :: xm(nelemn,nquad)     ! Cord x de los puntos de cuadratura de cada elemento
  real(8) :: yc(np)               ! Coordenada y de los nodos  
  real(8) :: ym(nelemn,nquad)     ! Cord y de los puntos de cuadratura de cada elemento
  real(8) :: ypert                ! ypert = aprof

! ---------------------- Condiciones iniciales ----------------------------- ! 

  real(8) :: xbleft  = 3.0d0      ! Coordenada x izquierda del objeto
  real(8) :: xbrite  = 1.0d0      ! Coordenada x derecha del objeto
  ! Por lo tanto el diametro de la region seria de 2 
  real(8) :: xlngth  = 7.0d0      ! Largo de la region del cilindro (xf - x0)
  real(8) :: aprof   = 1.0d0      ! Valor de la altura del objeto
  real(8) :: reynld  = 2.3d0      ! El valor del no. Reynolds (2300 Flujo laminar)
  real(8) :: ylngth  = 3.0d0      ! Altura de la region del cilindro (h)
  print*, ''
  write(*, '(1x,a,f10.7)')' El objeto se generara con una altura de : ', aprof
  write(*,'(1x,a,f10.5,a,f10.5)')' Largo y altura del cilindro : ', xlngth,',', ylngth
  write(*,'(1x,a,f10.7)')' Numero de Reynolds    :', reynld
  write(*,'(1x,a,i7)')' Numero de Elementos   : ', nelemn
  print*, ''

! ---------------------- Definicion de la geometria del sistema ----------------------------- ! 

! setgrd : Configuracion de geometria de la malla 
  call setgrd (indx, insc, isotri,mx, my, &
     nelemn, neqn, nnodes, node, np, xbleft, xbrite, xlngth )
  ypert = aprof ! Establecemos nuestro punto de la altura del puente como ypert 
  call setxy (my, np, nx, ny, xc, xlngth, yc, ylngth, ypert )
  call setqud ( area, isotri,nelemn, nnodes, node, np, nquad, xc, xm, yc, ym )
end program 
! ---------------------- Inicio definicion de la geometria del sistema ----------------------------- ! 


subroutine setgrd (indx, insc, isotri,mx, my, nelemn, neqn, nnodes, node, np, xbleft, xbrite, xlngth )
  integer :: indx(np,2) ! Contiene para cada iesimo nodo el index de la velocidad u, v (o 0)
  integer :: insc(np)   ! Contiene para cada iesimo nodo el index de la presion p (o 0)
  integer :: isotri(nelemn)      ! Contiene para cada elemento 1 si es isoparametrico o 0 
  integer :: my , mx    ! Numero de nodos en x , y   
  integer :: nnodes     ! No. nodos (6 elementos triangulares cuadráticos que usamos)
  integer :: node(nelemn,nnodes) ! Contiene por cada elemento el ind global de cada elemento del nodo
  real(8) :: xbleft              ! Coordenada x izquierda del bump 
  real(8) :: xbrite              ! Coordenada x derecha del bump
  real(8) :: xlngth              ! Largo de la region 
  nbleft = nint(xbleft*(mx-1)/xlngth)+1
  nbrite = nint(xbrite*(mx-1)/xlngth)+1
  write(*,'(1x,a,f10.5,a, i3,a,f10.5, a, i3)') & 
  ' El objeto se extiende desde ',xbleft,' en el nodo ',nbleft, & 
  ' a ',xbrite,' en el nodo ',nbrite
  neqn = 0
  ielemn = 0
  do ip = 1, np ! np = mx*my  (Numero de nodos = 533) 
    ic = ((ip-1)/my)+1
    jc = mod((ip-1),my)+1
    icnt = mod(ic,2)
    jcnt = mod(jc,2)
    if ( (icnt == 1.and.jcnt == 1).and.(ic /= mx).and.(jc /= my) ) then
      ip1 = ip+my
      ip2 = ip+my+my
      ielemn = ielemn+1
      node(ielemn,1) = ip
      node(ielemn,2) = ip+2
      node(ielemn,3) = ip2+2
      node(ielemn,4) = ip+1
      node(ielemn,5) = ip1+2
      node(ielemn,6) = ip1+1
      if ( nbleft <= ic .and. ic < nbrite ) then
        isotri(ielemn) = 1
        else
        isotri(ielemn) = 0
      end if
      ielemn = ielemn+1
      node(ielemn,1) = ip
      node(ielemn,2) = ip2+2
      node(ielemn,3) = ip2
      node(ielemn,4) = ip1+1
      node(ielemn,5) = ip2+1
      node(ielemn,6) = ip1
      if ( nbleft <= ic .and. ic < nbrite ) then
        isotri(ielemn) = 1
        else
        isotri(ielemn) = 0
      end if
      if ( ic == 1.and.1 < jc .and. jc < my ) then
        indx(ip,1) = -1
        indx(ip,2) = -1
        else if ( ic == mx.and.1 < jc .and. jc < my) then
        neqn = neqn+1
        indx(ip,1) = neqn
        indx(ip,2) = 0
        else if ( jc == 1 .and. isotri(ielemn) == 1 ) then
        indx(ip,1) = -2
        indx(ip,2) = -2
        else if ( ic == 1 .or. ic == mx .or. jc == 1 .or. jc == my ) then
        indx(ip,1) = 0
        indx(ip,2) = 0
        else
        neqn = neqn+2
        indx(ip,1) = neqn-1
        indx(ip,2) = neqn
      end if
      if ( jcnt == 1 .and. icnt == 1 ) then
        neqn = neqn+1
        insc(ip) = neqn
        else
        insc(ip) = 0
      end if
    end if 
   end do
  return

end subroutine setgrd


subroutine setxy (my, np, nx, ny, xc, xlngth, yc, ylngth, ypert )
  integer :: my              ! mx : Numero de nodos en la direccion y (2ny - 1)
  real(8) :: xc(np)          ! xc : Las coordenada x de los nodos (Vector xc(np))
  real(8) :: yc(np)          ! yc : Las coordenadas y de los nodos (Vecor yc(np))
  real(8) :: xlngth, ylngth  ! Largo y ancho del cilindro (Def 10,3)
  real(8) :: ypert           ! ypert : La variable parametro que contiene nuestra altura del puente 
  real(8) :: ybot, ylo

  do ip = 1 , np ! np = mx*my, no. nodos = 533, para nuestro triangulo de 6 nodos cada uno 
    ic = ( ( ip - 1 ) / my ) + 1
    jc = mod ( ( ip - 1 ), my ) + 1
    xc(ip) = (ic-1) * xlngth / (2*nx-2)
    ybot = -ypert * ( xc(ip) - 3.0) * ( xc(ip) - 1.0)
    ylo = max (0.0,ybot)
    yc(ip) = ( (my-jc)*ylo + (jc-1)*ylngth ) / (2*ny-2)
  end do 
  return
end subroutine setxy

subroutine setqud ( area, isotri, nelemn, nnodes, node, np, nquad, xc, xm, yc, ym )
  integer :: nelemn ! 2*(nx - 1)*(ny -1) ! nx = 21 , ny = 7 (Numero de elementos) 
  integer :: nnodes ! No. nodos (6 elementos triangulares cuadráticos que usamos)
  integer :: np     ! mx*my, no. nodos = 533 (Para nuestro triangulo de 6 nodos cada uno) 
  integer :: nquad  ! 3  (El número de puntos de cuadratura por elemento) 
  integer :: isotri(nelemn) ! Contiene para cada elemento 1 si es isoparametrico o 0 
  integer :: node(nelemn,nnodes)  ! Contiene por cada elemento el indice global del valor del nodo
  real (8) :: area(nelemn) ! Area total de los elementos 
  real (8) :: x1  ! Guarda el punto 1 de la cuadratura, coordenada en x 
  real (8) :: x2  ! Guarda el punto 2 de la cuadratura, coordenada en x 
  real (8) :: x3  ! Guarda el punto 3 de la cuadratura, coordenada en x 
  real (8) :: xc(np) ! Coordenadas x de los nodos 
  real (8) :: xm(nelemn,nquad) ! Arreglo de coordenadas x de los puntos de cuadratura de cada elemento
  real (8) :: y1  ! Guarda el punto 1 de la cuadratura, coordenada en y  
  real (8) :: y2  ! Guarda el punto 2 de la cuadratura, coordenada en y
  real (8) :: y3  ! Guarda el punto 3 de la cuadratura, coordenada en y 
  real (8) :: yc(np)  ! Coordenadas y de los nodos
  real (8) :: ym(nelemn,nquad) ! Arreglo de coordenadas y de los puntos de cuadratura de cada elemento

  do it = 1, nelemn ! 240 
    ip1 = node(it,1) ! 1,1,3,3,5,5,7,7,9,9,11,11 ... 505,505
    ip2 = node(it,2) ! 3,29,5,31,7,33,9,35,11,37 ... 533
    ip3 = node(it,3) ! 29, 27, 31,29,33,31,35... 531
    x1 = xc(ip1)
    x2 = xc(ip2)
    x3 = xc(ip3)
    y1 = yc(ip1)
    y2 = yc(ip2)
    y3 = yc(ip3)
    if ( isotri(it) == 0 ) then ! Si no es isoparametrico calculamos el punto medio 
      xm(it,1) = 0.5d0*(x1 + x2)
      ym(it,1) = 0.5d0*(y1 + y2)
      xm(it,2) = 0.5d0*(x2 + x3)
      ym(it,2) = 0.5d0*(y2 + y3)
      xm(it,3) = 0.5d0*(x3 + x1)
      ym(it,3) = 0.5d0*(y3 + y1)
      area(it) = 0.5d0*abs((y1 + y2)*(x2 - x1)+(y2 + y3)*(x3 - x2)+(y3 + y1)*(x1 - x3))
    else ! Para cada elemento isoparametrico definimos que sencillamente esta a la mitad 
         ! (Triangulo de 0.5, 1.0, 0.5)
      xm(it,1) = 0.5d0
      ym(it,1) = 0.5d0
      xm(it,2) = 1.0d0
      ym(it,2) = 0.5d0
      xm(it,3) = 0.5d0
      ym(it,3) = 0.0d0
      area(it) = 0.5d0
    end if
  end do

  ! Para ver esta malla tambien sirve el script de gnuplot de la subrutina anterior 
  open(40 , file = 'triag_0.dat' , status = 'replace' )
    do i = 1, nelemn
      do j = 1, nquad 
        write(40,'(2f10.5,i3)') xm(i,j), ym(i,j), j
      end do
        write(40, *) ''
    end do
  close(40)

  open(40 , file = 'triag_test.dat' , status = 'replace' )
    do i = 1, np
      write(40,'(2f10.5,i3)') xc(i), yc(i)
      !write(40, *) ''
    end do
    do i = 1, np
      write(40,'(2f10.5,i3)') xc(i), -yc(i)
      !write(40, *) ''
    end do
  close(40)
  return
end subroutine setqud 