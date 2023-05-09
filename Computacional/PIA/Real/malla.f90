! Las ecuaciones de Navier Stokes
! La formulación implica : 
! La velocidad horizontal U
! La velocidad vertical V 
! La presión P.  

! Las ecuaciones son:
! F1 = U dUdx + V dUdy + dPdx - mu*(ddU/dxdx + ddU/dydy) 
! F2 = U dVdx + V dVdy + dPdy - mu*(ddV/dxdx + ddV/dydy) 
! dUdx + dVdy = 0
! 
! Los nodos se conforman en dividir nuestro sistema en una geometria (finita), en nuestro caso 
! Triangulo isoparametrico (6 nodos)
! Tiene un nodo en cada vertice y en los puntos medios 
!             
!             1 
!        4 _ / \ _ 6    
!       2 _ / _ \ _ 3  
!             5    
! 
! Recomiendo : 
! 
! - Finite Element Analysis, P. Seshu, 5.8 Fluid Flow pg. 220 
! 
! - Finite Elements for the (Navier) Stokes Equations, Numerical Analysis Seminar, John Burkardt
! 
! - (Playlist) https://www.youtube.com/playlist?list=PLKSR9A4mJH5rvt_Lf007xbmJISRWA0xYS


program malla_trig 


  integer, parameter :: nx = 21                     ! Espaciado de los nodos en x (hay 2*nx+1) 
  integer, parameter :: ny = 7                      ! Espaciado de los nodos en y (hay 2*ny+1) 
  integer, parameter :: mx = 2*nx -1 , my = 2*ny -1 ! Numero de nodos en x , y 
  integer, parameter :: nquad = 6                   ! El número de puntos de cuadratura por elemento.
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
     nelemn, neqn, nnodes, node, np, nx, ny, xbleft, xbrite, xlngth )

! setxy :  Establece las coordenadas x, y de los puntos de la malla.

  ypert = aprof ! Establecemos nuestro punto de la altura del puente como ypert 
  call setxy (mx, my, np, nx, ny, xc, xlngth, yc, ylngth, ypert )

! setqud : Determinar nuestros puntos de cuadratura numérica

 call setqud ( area, isotri,nelemn, nnodes, node, np, nquad, xc, xm, yc, ym )

end program 







! ---------------------- Inicio definicion de la geometria del sistema ----------------------------- ! 


subroutine setgrd (indx, insc, isotri,mx, my, nelemn, neqn, nnodes, node, np, nx, ny, xbleft, xbrite, xlngth )

! setgrd : Construye una malla, numera incógnitas, calcula áreas y puntos por la regla 
! de cuadratura del punto medio.
! 
! Referencia : Programming the Isoparametric Six Node Triangle, Carlos A. Felippa, Boulder, Colorado
! 
! Variables utilizadas (input)
! 
  integer :: indx(np,2) ! Contiene para cada iesimo nodo el index de la velocidad u, v (o 0)
  integer :: insc(np)   ! Contiene para cada iesimo nodo el index de la presion p (o 0)
  integer :: isotri(nelemn)      ! Contiene para cada elemento 1 si es isoparametrico o 0 
  integer :: my , mx    ! Numero de nodos en x , y   
  integer :: nnodes     ! No. nodos (6 elementos triangulares cuadráticos que usamos)
  integer :: node(nelemn,nnodes) ! Contiene por cada elemento el ind global de cada elemento del nodo
  real(8) :: xbleft              ! Coordenada x izquierda del bump 
  real(8) :: xbrite              ! Coordenada x derecha del bump
  real(8) :: xlngth              ! Largo de la region 

!
!  Idea principal de la configuracion isoparamétrica ( Mismo parametro en toda la malla) : 
! 
! Derivamos una funcion con la misma forma, para definir un elemento geometrico 
! para describir fuerzas, velocidades, vectores en general
! Recomiendo (Por prueba y error) que todos los elementos por encima del bump sean isoparametricos.
!
! Calculamos la ubicación de los nodos de las esquinas del bump a partir de las coordenadas X.
! mx = 2*21 - 1 = 41  (no. nodos en x)
! 
! (x0 = xbleft)                                           (xf = xbrite)
! |=======================================================| 
!                   (xlngth = 10)

  nbleft = nint(xbleft*(mx-1)/xlngth)+1

  nbrite = nint(xbrite*(mx-1)/xlngth)+1

  write(*,'(1x,a,f10.5,a, i3,a,f10.5, a, i3)') & 
  ' El objeto se extiende desde ',xbleft,' en el nodo ',nbleft, & 
  ' a ',xbrite,' en el nodo ',nbrite

! Los nodos son los puntos de unión de cada elemento . 
! La solución del sistema completo sigue las reglas de los problemas discretos. 
! El sistema completo se forma por ensamblaje de cada nodo


! Determina si la región es larga o delgada 
! Esto determinará la numeración de los nodos y elementos.

! nx = 21       ! Espaciado de los nodos en x (hay 2*nx+1) 
! ny = 7        ! Espaciado de los nodos en y (hay 2*ny+1) 
! Como ny < nx entonces tomamos una ordenacion vertical 


!  Estrategia isoparametrica
!
! Vamos a definir nuestros elementos isoparametricos arriba del bump 

! 
! Asignamos los nodos a nuestra malla de elementos triangulares
!
!              ___  ___
!             /\  /\  /\   
!            /__\/__\/__\ 
!           /\  /\  /\  /\ 
! (nbleft) /__\/__\/__\/__\ (nbrite)
! 

  neqn = 0
  ielemn = 0

 ! Recuerda que : 
 ! nx = 21       ! Espaciado de los nodos en x 
 ! ny = 7        ! Espaciado de los nodos en y  
 ! mx = 2*nx -1 = 41 ! Nodo en x 
 ! my = 2*ny -1 = 13 ! Nodo en y 


  do ip = 1, np ! np = mx*my  (Numero de nodos = 533) 

    ic = ((ip-1)/my)+1
    jc = mod((ip-1),my)+1
   

    icnt = mod(ic,2)
    jcnt = mod(jc,2)


! Si tanto el recuento de filas como el de columnas son impares y no estamos en la última 
! fila o columna superior, entonces podemos definir dos nuevos elementos triangulares basados en el nodo.

  ! Por ordenación horizontal, dada la siguiente disposición de nodos, por ejemplo:

  !   21 22 23 24 25
  !   16 17 18 19 20
  !   11 12 13 14 15
  !   06 07 08 09 10
  !   01 02 03 04 05

  ! Entonces cuando lleguemos al nodo 13, definiremos

  ! elemento 7: (13, 23, 25, 18, 24, 19)
  ! elemento 8: (13, 25, 15, 19, 20, 14)


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



! Triangulo isoparametrico (6 nodos)
! Tiene un nodo en cada vertice y en los puntos medios 
!             
!             1 
!        4 _ / \ _ 6    
!       2 _ / _ \ _ 3  
!             5    
!  Podemos reducir nuestro grid como un polinomio cuadratico 
!  f(x,y) = c0 + c1x + c2y + c3x^2 + c4xy + c5y^2
!  en principio resolvemos para los coeficientes en terminos de las coordenadas 
!  (x1, y1), (x2 , y2) ... (x6, y6) 
!  x = N1x1 + N2x2 + N3x3 + N4x4 + N5x5 + N6x6
!  y = N1y1 + N2y2 + N3y3 + N4y4 + N5y5 + N6y6


! { Ux(xy) }   [N1, 0,  N2, 0,  N3, 0,  N4, 0,  N5, 0,  N6, 0 ] x [T]
! { Uy(xy) } = [0,  N1, 0,  N2, 0,  N3, 0,  N4, 0,  N5, 0 , N6] 

! T = [ Ux1, Uy1, Ux2, Uy2, Ux3, Uy3, Ux4, Uy4, Ux5, Uy5, Ux6, Uy6 ]
! ielemn max : 240

!
! Límite izquierdo, velocidades horizontales y verticales especificadas.
!
    if ( ic == 1.and.1 < jc .and. jc < my ) then
      indx(ip,1) = -1
      indx(ip,2) = -1
!
! Límite derecho, velocidades horizontales desconocidas, vertical
! velocidades especificadas
!
    else if ( ic == mx.and.1 < jc .and. jc < my) then
      neqn = neqn+1
      indx(ip,1) = neqn
      indx(ip,2) = 0
!
! Límite inferior, con triángulo isoperimétrico
!
    else if ( jc == 1 .and. isotri(ielemn) == 1 ) then
      indx(ip,1) = -2
      indx(ip,2) = -2
!
!  De lo contrario, sólo una pared
!
    else if ( ic == 1 .or. ic == mx .or. jc == 1 .or. jc == my ) then
      indx(ip,1) = 0
      indx(ip,2) = 0
!
!  En caso contrario, un nodo interior normal en el que ambas velocidades son desconocidas
!
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

  !   print*, ''
  !   ! print*, '     i     indx 1, indx 2, insc'
  !   print*, ''
  !   do i = 1, np
  !   !  write(*,'(4i5)')i,indx(i,1),indx(i,2),insc(i)
  !   end do
  !   print*, ''
  !   ! print*, ' Triángulos isoparamétricos:'
  !   print*, ''
  !   do i = 1, nelemn
  !   !  if ( isotri(i) == 1) print*, i
  !   end do
  !   print*, ' '
  !   ! print*, '   it   node(it,*)'
  !   print*, ' '
  !   do it = 1, nelemn
  !   !  write(*,'(7i6)') it,(node(it,i),i = 1,6)
  !   end do
  ! end if

  ! write(*,'(1x,a,i7)')' setgrd : número de incógnitas ', neqn

  return

end subroutine setgrd



subroutine setxy (mx, my, np, nx, ny, xc, xlngth, yc, ylngth, ypert )
! setxy : Establecemos las coordenadas normales de la malla basandonos el el valor del parametro (ypert)
! Referencia : Finite Element Analysis, P. Seshu, 5.3.3 Natural Coordinates Triangular Elements pg. 165 

integer :: my              ! mx : Numero de nodos en la direccion y (2ny - 1)
integer :: mx              ! my : Numero de nodos en la direccion x (2nx - 1)
real(8) :: xc(np)          ! xc : Las coordenada x de los nodos (Vector xc(np))
real(8) :: yc(np)          ! yc : Las coordenadas y de los nodos (Vecor yc(np))
real(8) :: xlngth, ylngth  ! Largo y ancho del cilindro (Def 10,3)
real(8) :: ypert           ! ypert : La variable parametro que contiene nuestra altura del puente 
real(8) :: ybot, ylo

  ! Inicio  
  ! .........................>
  ! .........................>
  ! .........................>
  ! .........................>
  ! .........................>
  ! .........................>
  ! .........................>


  do ip = 1 , np ! np = mx*my, no. nodos = 533, para nuestro triangulo de 6 nodos cada uno 

    ! Aqui sencillamente determio el inicio y final de mi malla 
      ic = ( ( ip - 1 ) / my ) + 1
      jc = mod ( ( ip - 1 ), my ) + 1

    xc(ip) = (ic-1) * xlngth / (2*nx-2)
  ! Llenamos nuestro vector de coord. x 
  ! xc = 13(0) , 13(0.25) , 13(0.5) , 13(0.75) ... 13(10)
  

  ! Variables dummies, lo que quiero hacer el llenar mi
  ! coordenada en y 

  ybot = -ypert * ( xc(ip) - 3.0) * ( xc(ip) - 1.0)

  ! ybot = 13(-1.2) , 13(-0.825) , 13(-0.5) ... 13(-25)

  ! Obtener el valor maximo de [0 : ybot]
  ! Nos quedamos sin los valores negativos
  ylo = max (0.0,ybot)

  ! ylo = Varios ceros exepto 
  ! [6](0.175) , [7](0.3) , [8](0.375) , [9](0.4)
  ! [10](0.375) , [11](0.3) , [12](0.175)

  yc(ip) = ( (my-jc)*ylo + (jc-1)*ylngth ) / (2*ny-2)

  ! Hemos llenado nuestro vector de coordenadas y 
  ! yc = 5(0.0,0.25 0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.75,3.0) 
  !    = (0.175,0.41,0.64,0.88,1.11,1.35,1.58,1.82,2.05,2.29,2.52,2.76,3.0)
  !    = (0.30, 0.41,0.64,0.88,1.11,1.35,1.58,1.82,2.05,2.29,2.52,2.76,3.0)
  !    = (0.175,0.41,0.64,0.88,1.11,1.35,1.58,1.82,2.05,2.29,2.52,2.76,3.0)
  !   = 29(0.0,0.25 0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.75,3.0) 

  end do 


  ! Para ver como va la cosa 
  ! open(30 , file = 'malla_0.dat' , status = 'replace' )
  ! print*, '' 
  ! write(*,'(2x,a,i5)') 'setxy : vectores de coordenadas x, y con longitud : ' , np  
  ! Para ver como quedo 
  ! iesimo-elemento fin , coord(x) , coord(y)
  ! print*, ''
  ! write(*,'(2x,a,3x,a,8x,a)') 'i-elem' , 'cordx' , 'cordy'
  ! do i = 1, np
  !  write(30,'(i5,2f12.5)') i, xc(i), yc(i)
  ! end do
  !   i-elem    | cordx | cordy 
  !  1  ... 13 
  !  14 ... 26 
  !  27 ... 39 
  ! close(30)

  ! Archivo para gnuplot 
  !   set termoption dashed

  ! datafile = 'malla_0.dat'
  ! stats datafile nooutput
  ! plot for [IDX=0:STATS_blocks-1] \
  !     datafile \
  !     index IDX \
  !     using 1:2 \
  !     w lp  pt '^' lt '.' lc rgb 'dark-blue' \
  !     notitle
  ! pause 5 



  ! Final 
  ! .........................>
  ! ......       ............>
  ! ....          ...........>
  ! ...            ..........>
  ! ..              .........>
  ! .                ........>
  !                   .......>

  return


end subroutine setxy




subroutine setqud ( area, isotri, nelemn, nnodes, node, np, nquad, xc, xm, yc, ym )
! setqud : Establece la información de la regla de cuadratura del punto medio
! (Sencillamente calculamos el area y el punto medio de nuestros triangulos en la malla)
! Referencia : Finite Element Analysis, P. Seshu, 5.3.3 Natural Coordinates Triangular Elements pg. 165 

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


  ! Leemos para cada coordanada (x,y) el valor que ya tenia en el numero de su nodo respectivo
  do it = 1, nelemn ! 240 

    ! Ya habiamos determinado que nuestro numero de puntos por cuadratura serian 3 
    ip1 = node(it,1) ! 1,1,3,3,5,5,7,7,9,9,11,11 ... 505,505
    ip2 = node(it,2) ! 3,29,5,31,7,33,9,35,11,37 ... 533
    ip3 = node(it,3) ! 29, 27, 31,29,33,31,35... 531

    ! Entonces vamos a reescribir nuestro arreglo de coordenadas en x,y como el punto medio entre
    ! estos dos puntos donde ahora se encuentran nuestros nodos. 

    x1 = xc(ip1)
    x2 = xc(ip2)
    x3 = xc(ip3)

    y1 = yc(ip1)
    y2 = yc(ip2)
    y3 = yc(ip3)

    if ( isotri(it) == 0 ) then ! Si no es isoparametrico calculamos el punto medio 
    ! Ver pagina 214 de Referencia

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


  ! Ver los valores de cada elemento de area y sus puntos 
  ! print*, ''
  ! write(*,'(1x,a,i5)') ' setqud: elementos de area y puntos de cuadratura : ', nelemn
  ! write(*,'(2x,a,1x)') ' no.elemento'

  ! do i = 1, nelemn
  !   write(*,'(3x,i5,f10.5,1x,a)') i, area(i), ' | area inicial y puntos (x,y) | '
  !   do j = 1, nquad
  !     write(*,'(3x, 2i5, 2f10.5)') i, j, xm(i,j), ym(i,j)
  !   end do
  ! end do


  ! Para ver esta malla tambien sirve el script de gnuplot de la subrutina anterior 
  open(40 , file = 'triag_0.dat' , status = 'replace' )
    do i = 1, nelemn
      do j = 1, nquad 
        write(40,'(2f10.5,i3)') xm(i,j), ym(i,j), j
      end do
        write(40, *) ''
    end do

    do i = 1, nelemn
      do j = 1, nquad 
        if (ym(i,j) /= 0) then
          write(40,'(2f10.5,i3)') xm(i,j), -ym(i,j), j
        end if
      end do
        write(40, *) ''
    end do
  close(40)

  return
end subroutine setqud 



