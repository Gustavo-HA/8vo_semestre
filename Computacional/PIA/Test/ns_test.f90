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
! Recomiendo : 
! 
! - Finite Element Analysis, P. Seshu, 5.8 Fluid Flow pg. 220 
! 
! - Finite Elements for the (Navier) Stokes Equations, Numerical Analysis Seminar, John Burkardt
! 
! - (Playlist) https://www.youtube.com/playlist?list=PLKSR9A4mJH5rvt_Lf007xbmJISRWA0xYS


program ns_cylinderflow 

  
  integer, parameter :: maxnew = 4    ! Numero maximo de pasos Newton por iteración
  integer, parameter :: maxsec = 10  ! Numero maximo de pasos Secante por iteración
  integer, parameter :: nx = 21      ! Espaciado de los nodos en x (hay 2*nx+1) !cambio
  integer, parameter :: ny = 7        ! Espaciado de los nodos en y (hay 2*ny+1) !cambio
  integer, parameter :: mx = 2*nx -1 , my = 2*ny -1 ! Numero de nodos en x , y 
  integer, parameter :: maxrow = 27*ny              ! La ultima fila de la matriz A. maxrow = 27*min(nx,ny)
  integer, parameter :: maxeqn = 2*mx*my + nx*ny    ! Numero maximo de equaciones y funciones 
  integer, parameter :: nquad = 3                   ! El número de puntos de cuadratura por elemento.
  integer, parameter :: nelemn = 2*(nx - 1)*(ny -1) ! Numero de elementos 
  integer, parameter :: nnodes = 6                  ! No. nodos por elemento (6 triangulares cuadráticos)
  integer, parameter :: np = mx*my                  ! Numero de nodos  

  integer :: indx(np, 2)    ! Contiene para cada iesimo nodo el index de la velocidad u, v (Puede 0)
  integer :: insc(np)       ! Contiene para cada iesimo nodo el index de la presion p (Puede 0)
  integer :: isotri(nelemn) ! Contiene para cada elemento 1 si es isoparametrico o 0 
  integer :: neqn           ! Numero de ecuaciones y funciones

  integer :: i, j                 ! Iteradores 
  integer :: iline(my)            ! index del coef del elemento finito asociado a cada nodo del perfil
  integer :: iter                 ! iterador para sol. N-S
  integer :: itype                ! Condicional para Soluciones Navier Stokes 
  integer :: nband                ! Ancho de banda del sistema lineal 
  integer :: nlband               ! Ancho de banda inferior de la matriz
  integer :: node(nelemn,nnodes)  ! Contiene por cada elemento el ind global de cada elemento del nodo
  integer :: nrow                 ! Numero de filas necesarias para guardar la matriz A 
  integer :: numnew               ! Contador de numeros totales de pasos 
  integer :: ibump = 2            ! Define cuando los elementos son isoparametricos (Solo Arriba)

  logical :: long                 ! Orientacion Vertical(V) u Horizontal (F), depende de nx, ny 

  real(8) :: a(maxrow, maxeqn)    ! Contiene nuestro arreglo principal 
  real(8) :: anew        ! Valor nuevo de a (Elemento de arreglo principal)
  real(8) :: anext        ! Step de siguiente valor de a 
  real(8) :: aold       ! Valor anterior de nuestro (Metodo de secante)
  real(8) :: area(nelemn)         ! Area total de los elementos 
  real(8) :: dcda(my)             ! Sensivilidad 
  real(8), external :: ddot                 ! Producto punto de dos vectores
  real(8) :: cpu1
  real(8) :: cpu2
  real(8) :: f(maxeqn)            ! Funcion f(x) (Los coeficientes de los elementos finitos)
  real(8) :: g(maxeqn)            ! Funcion g(x) (Estimación inicial g = 0.)
  real(8) :: gr(my, my)           ! La matriz de Gram
  real(8) :: r(my)                ! La integral de línea de uprof * phi.
  real(8) :: res(maxeqn)          ! Contiene el residual.
  real(8) :: sens(maxeqn)         ! Sensivilidad 
  real(8) :: temp                 ! Temperatura (No es cierto)
  real(8) :: test                 ! Test de convergencia 
  real(8) :: uprof(my)            ! Velocidad horizontal sobre la linea 
  real(8) :: xc(np)               ! Coordenadas x de los nodos 
  real(8) :: xm(nelemn,nquad)     ! Cord x de los puntos de cuadratura de cada elemento
  real(8) :: yc(np)               ! Coordenada y de los nodos  
  real(8) :: ym(nelemn,nquad)     ! Cord y de los puntos de cuadratura de cada elemento
  real(8) :: ypert                ! ypert = aprof

  ! phi (1)
  ! contiene el valor de una función de base cuadrática o su derivada, evaluado en un punto 
  ! de cuadratura en  particular
  ! phi(i,j,k,1) es el valor de la base cuadrática, función asociada al nodo 
  ! local k en el elemento i, evaluada
  ! en el punto de cuadratura j.
  ! phi (i,j,k,2) es la derivada x de esa misma función base,
  ! phi (i,j,k,3) es la derivada y de esa misma función base.

  real (8) :: phi(nelemn,nquad,nnodes,3) ! (1)

  ! psi (2)
  ! contiene el valor de una función de base lineal evaluada en un , punto de cuadratura
  ! psi(i,j,k) es el valor de la función de base lineal asociada, 
  ! con nodo local k en el elemento i evaluado en el punto de cuadratura j.

  real(8) :: psi(nelemn,nquad,nnodes) ! (2)
  real(8) :: rjpnew  = 0.0d0          ! J prima 
  real(8) :: rjpold  = 0.0d0          ! rjnew
  real(8) :: tolnew  = 0.00001d0      ! Tolerancia de convergencia de pasos sol. N-S por iteracion
  real(8) :: tolsec  = 0.00001d0      ! Tolerancia de convergencia de pasos sol. general por iteracion 
  real(8) :: xbleft  = 0.0d0          ! Coordenada x izquierda del cilindro 
  real(8) :: xbrite  = 0.5d0          ! Coordenada x derecha del cilindro
  !cambio
  ! ---------------------- Condiciones iniciales ----------------------------- ! 
 
  real(8) :: xlngth  = 10.0d0     ! Largo de la region (xf - x0)
  real(8) :: aprof   = 0.5d0      ! Valor de la altura del bump
  real(8) :: reynld  = 2.3d0      ! El valor del no. Reynolds
  real(8) :: xprof   = 4.0d0      ! Cord x en la que se mide el perfil 
  real(8) :: ylngth  = 3.0d0      ! Altura de la region (h)

  character (len = 13):: time_file = 'vel_time0.dat'    ! Archivo guardar datos (xc(ip), yc(ip), u, v)
  character (len = 16):: time_file_test = 'magvel_time0.dat' !Archivo guardar datos xc, yc, sqrt(u**2+y**2)
  


  call cpu_time(cpu1)
  aold = 0.0d0
  anew = 0.0d0
  anext = 0.3

  print*, ''
  write(*, '(1x,a,f10.7)')' El objeto se generara con una altura de : ', aprof
  write(*,'(1x,a,f10.5,a,f10.5)')' Largo y altura del cilindro : ', xlngth,',', ylngth
  write(*,'(1x,a,i7)')' Numero de Elementos   : ', nelemn
  print*, ''



  ! ---------------------- Definicion de la geometria del sistema ----------------------------- ! 

  ! 27 / 03 
  ! setgrd : Configuracion de geometria de la malla 
  call setgrd ( ibump, indx, insc, isotri,  long, maxeqn, mx, my, &
     nelemn, neqn, nnodes, node, np, nx, ny, xbleft, xbrite, xlngth )
  ! 28 / 03 
  ! setxy :  Establece las coordenadas x, y de los puntos de la malla.
  ypert = aprof ! Establecemos nuestro punto de la altura del puente como ypert 
  call setxy (  long, mx, my, np, nx, ny, xc, xlngth, yc, ylngth, ypert )
  ! 29 / 03 
  ! setqud : Determinar nuestros puntos de cuadratura numérica
  call setqud ( area, isotri,  nelemn, nnodes, node, np, nquad, xc, xm, yc, ym )
  ! 02 / 04 
  ! setbas : Establecer valores de funciones base en puntos de cuadricula
  call setbas ( isotri, nelemn, nnodes, node, np, nquad, phi, psi, xc, xm, yc, ym )
  ! 03 / 04 
  ! setlin : Encuentra puntos en la línea de muestreo del perfil de velocidad
  call setlin ( iline, indx,  long, mx, my, np, nx, ny, xlngth, xprof )
  ! setban : Calcula el ancho de banda.
  call setban ( indx, insc, maxrow, nband, nelemn, nlband, nnodes, node, np, nrow )

  ! --------- Solución de Navier Stokes, utilizando una estimación inicial de g = 0  ------------ ! 
  ! 04 / 04 
  ! Empezamos con la solución de las ecuaciones de Navier Stokes
  ! Primero utilizamos una estimación inicial de G = 0.
  !
  g(1:neqn) = 0.0d0 
  ! nstoke : Resuelve la ecuacion de Navier Stokes utilizando elementos de Taylor-Hood.
  call nstoke ( a, area, f, g, indx, insc, isotri, maxnew, &
    maxrow, nband, nelemn, neqn, nlband, nnodes, node, np, nquad, &
    nrow, numnew, phi, psi, reynld, tolnew, xc, xm, yc, ym )
  ! 05 / 04 
  ! resid : Calcula el residuo de la solucion G (Recuerda que al principio fue 0)
  call resid ( area, g, indx, insc, isotri,  nelemn, neqn, nnodes, &
    node, np, nquad, phi, psi, res, reynld, xc, xm, yc, ym )
  ! 07 / 04 
  ! getg : Copia el flujo a lo largo de la linea de perfil.
  !
  call getg ( g, iline, my, neqn, uprof )
  ! gram : Calcula y almacena la matriz gram.
  call gram ( gr, iline, indx,  my, nelemn, nnodes, node, np, r, uprof, xc, xprof, yc )


  ! ------------- Escritura del archivo de datos e iteracion general ---------------------- ! 
  ! 10 / 04 
  ! Escribimos nuestro primer archivo de datos en time_file
  ! xc(ip) : Coordenadas x de los nodos 
  ! yc(ip) : Coordenadas y de los nodos 
  ! u      : Coeficientes de la velocidad horizontal U
  ! v      : Coeficientes de la velocidad vertical V 
  !call file_name_inc ( time_file ) ! Aumenta un digito el nombre del archivo 
  !call file_name_inc ( time_file_test ) !cambio
  !open (33, file = time_file, status = 'replace' )
  !open (34, file = time_file_test, status = 'replace')
  !call time_write ( f, indx, neqn, np, yc ,xc) ! Guarda en el primer archivo 'time0.dat'
  !close (33)
  !close (34)
  ! Borra la información sobre la verdadera solución antes de comenzar.
  g(1:neqn) = 0.0d0
  ! Bucle de iteración para escribir 'time(n).dat':
  do iter = 1, maxsec
    !write(*,*)'Iteracion secante:', iter
    ! Actualiza la malla.
      ypert = anew
    ! Establece las coordenadas x, y de los puntos de la malla.
      call setxy (  long, mx, my, np, nx, ny, xc, xlngth, yc, ylngth, ypert )
    ! Establecer los puntos de cuadratura.
      call setqud ( area, isotri,  nelemn, nnodes, node, np, nquad, xc, xm, yc, ym )
    ! Establezca los valores de las funciones base en los puntos de la cuadrícula.
      call setbas ( isotri, nelemn, nnodes, node, np, nquad, phi, psi, xc, xm, yc, ym )
    ! Resuelve el flujo en el nuevo valor del parámetro.
      call nstoke ( a, area, f, g, indx, insc, isotri, maxnew, &
        maxrow, nband, nelemn, neqn, nlband, nnodes, node, np, nquad, &
        nrow, numnew, phi, psi, reynld, tolnew, xc, xm, yc, ym )
    ! Obtiene el perfil de velocidad a lo largo de nuestra geometria horizontal.
    call getg ( g, iline, my, neqn, uprof )
    ! Guarda en el archivo 'time(1,2,3,4 ...).dat'
    call file_name_inc ( time_file )
    call file_name_inc ( time_file_test )
    open (33, file = time_file, status = 'replace' ) 
    open (34, file = time_file_test, status = 'replace' ) 
    call time_write ( f, indx, neqn, np, yc ,xc)
    close (33)
    close (34)
    ! Obtiene el perfil de velocidad a lo largo de nuestra geometria horizontal.
    call getg ( g, iline, my, neqn, uprof )
    ! Resuelve sistema lineal para sensibilidades.
    itype = -2
    call linsys ( a, area, sens, g, indx, insc, isotri, itype, maxrow, &
          nband, nelemn, neqn, nlband, nnodes, node, np, nquad, nrow, &
          phi, psi, reynld, xc, xm, yc, ym )
    ! Obtiene el perfil de velocidad a lo largo de nuestra geometria horizontal.
    call getg ( sens, iline, my, neqn, dcda )
    ! Evalua j primo en el valor actual del parámetro donde j es funcional a minimizar.
    ! jprime = 2.0d+00 * dcda(i) * ( gr(i,j) * uprof(j) - r(i) )
      rjpnew = 0.0d0
      do i = 1, my
        temp = -r(i)
        do j = 1, my
            temp = temp + gr(i,j) * uprof(j)
        end do
        rjpnew = rjpnew + 2.0d0 * dcda(i) * temp
      end do
    ! Actualiza la estimación del parámetro utilizando el paso secante.
      if ( 1 < iter ) then
        anext = aold - rjpold * ( anew - aold ) / ( rjpnew - rjpold )
      end if
      aold = anew
      anew = anext
      rjpold = rjpnew
      if ( anew /= 0.0d0 ) then
        test = abs ( anew - aold ) / anew
        else
        test = 0.0d0
      end if
        !write ( *, * ) '  Nuevo valor del parametro', anew
        !write ( *, * ) '  Prueba de convergencia ', test
      if ( abs ( ( anew - aold ) ) <= abs ( anew ) * tolsec .and. 1 < iter ) then
        write ( *, * ) 'Iteracion Secante convergio'
        go to 40
      end if
    ! Termina el bucle de iteración para escribir 'time(n).dat':
  end do 


  write ( *, * ) 'La iteracion no convergio'
  40 continue
  call cpu_time(cpu2)
  write ( *, '(a)' ) ' '
  write ( *, '(a,f8.3,a)' ) 'Tiempo total de ejecucion = ', cpu2 - cpu1, ' segundos.'

end program ns_cylinderflow




! ---------------------- Inicio definicion de la geometria del sistema ----------------------------- ! 




subroutine setgrd ( ibump, indx, insc, isotri,  long, maxeqn, mx, my, &
  nelemn, neqn, nnodes, node, np, nx, ny, xbleft, xbrite, xlngth )

  ! setgrd : Construye una malla, numera incógnitas, calcula áreas y puntos por la regla 
  ! de cuadratura del punto medio.
  ! 
  ! Referencia : Programming the Isoparametric Six Node Triangle, Carlos A. Felippa, Boulder, Colorado
  ! 
  ! Variables utilizadas (input)
  ! 
  integer :: ibump      ! Define cuando los elementos son isoparametricos (Solo Arriba)
  integer :: my , mx    ! Numero de nodos en x , y   
  integer :: nnodes     ! No. nodos (6 elementos triangulares cuadráticos que usamos)
  integer :: indx(np,2) ! Contiene para cada iesimo nodo el index de la velocidad u, v (o 0)
  integer :: insc(np)   ! Contiene para cada iesimo nodo el index de la presion p (o 0)
  integer :: isotri(nelemn)      ! Contiene para cada elemento 1 si es isoparametrico o 0 
  integer :: node(nelemn,nnodes) ! Contiene por cada elemento el ind global de cada elemento del nodo
  real(8) :: xbleft              ! Coordenada x izquierda del bump 
  real(8) :: xbrite              ! Coordenada x derecha del bump
  real(8) :: xlngth              ! Largo de la region 
  logical :: long       ! Orientacion Vertical(V) u Horizontal (F), depende de nx, ny 

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

  if ( ny < nx ) then
    long = .true.
 !   write(*,*)'Orientacion Vertical'
  else
    long = .false.
  !  write(*,*)'Orientacion Horizontal'
  end if

  !
  !  Estrategia isoparametrica
  !

  if ( ibump == 0 ) then
  !  write(*,*)'Sin elementos isoparametricos'
  else if ( ibump == 1 ) then
  !  write(*,*)'Elementos isoparametricos en el bump.'
  else if ( ibump == 2 ) then
  !  write(*,*)'Elementos isoparametricos arriba del bump'
  else if ( ibump == 3 ) then
  !  write(*,*)'Todos los elementos son isoparametricos.'
  else
  !  write(*,*)'Valor inesperado ',ibump
    stop
  end if


  neqn = 0
  ielemn = 0

 ! Recuerda que : 
 ! nx = 21       ! Espaciado de los nodos en x 
 ! ny = 7        ! Espaciado de los nodos en y  
 ! mx = 2*nx -1 = 41 ! Nodo en x 
 ! my = 2*ny -1 = 13 ! Nodo en y 


  do ip = 1, np ! np = mx*my  (Numero de nodos = 533) 

    if ( long ) then
      ic = ((ip-1)/my)+1
      jc = mod((ip-1),my)+1
    else
      ic = mod((ip-1),mx)+1
      jc = ((ip-1)/mx)+1
    end if

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

      if ( long ) then

        ip1 = ip+my
        ip2 = ip+my+my
        ielemn = ielemn+1
        node(ielemn,1) = ip
        node(ielemn,2) = ip+2
        node(ielemn,3) = ip2+2
        node(ielemn,4) = ip+1
        node(ielemn,5) = ip1+2
        node(ielemn,6) = ip1+1

        if ( ibump == 0 ) then
          isotri(ielemn) = 0
        else if ( ibump == 1 ) then
          isotri(ielemn) = 0
        else if ( ibump == 2 ) then
          if ( nbleft <= ic .and. ic < nbrite ) then
            isotri(ielemn) = 1
          else
            isotri(ielemn) = 0
          end if
        else
          isotri(ielemn) = 1
        end if

        ielemn = ielemn+1
        node(ielemn,1) = ip
        node(ielemn,2) = ip2+2
        node(ielemn,3) = ip2
        node(ielemn,4) = ip1+1
        node(ielemn,5) = ip2+1
        node(ielemn,6) = ip1

        if ( ibump == 0 ) then
          isotri(ielemn) = 0
        else if ( ibump == 1 ) then
          if ( jc == 1 .and. nbleft <= ic .and. ic < nbrite ) then
            isotri(ielemn) = 1
          else
            isotri(ielemn) = 0
          end if
        else if ( ibump == 2 ) then
          if ( nbleft <= ic .and. ic < nbrite ) then
            isotri(ielemn) = 1
          else
            isotri(ielemn) = 0
          end if
        else
          isotri(ielemn) = 1
        end if

      else

    ! Para la ordenación vertical, dada la siguiente disposición de nodos, por ejemplo:
    !
    !    05 10 15 20 25
    !    04 09 14 19 24
    !    03 08 13 18 23
    !    02 07 12 17 22
    !    01 06 11 16 21
    !
    ! Cuando lleguemos al nodo 13, definiremos
    ! Elemento 7: (13, 25, 23, 19, 24, 18)
    ! Elemento 8: (13, 15, 25, 14, 20, 19)

        ip1 = ip+mx
        ip2 = ip+mx+mx

        ielemn = ielemn+1
        node(ielemn,1) = ip
        node(ielemn,2) = ip2
        node(ielemn,3) = ip2+2
        node(ielemn,4) = ip1
        node(ielemn,5) = ip2+1
        node(ielemn,6) = ip1+1

        if ( ibump == 0 ) then
          isotri(ielemn) = 0
        else if ( ibump == 1 ) then
          isotri(ielemn) = 0
        else if ( ibump == 2 ) then
          if ( nbleft <= ic .and. ic < nbrite ) then
            isotri(ielemn) = 1
          else
            isotri(ielemn) = 0
          end if
        else
          isotri(ielemn) = 1
        end if

        ielemn = ielemn+1
        node(ielemn,1) = ip
        node(ielemn,2) = ip2+2
        node(ielemn,3) = ip+2
        node(ielemn,4) = ip1+1
        node(ielemn,5) = ip1+2
        node(ielemn,6) = ip+1

        if ( ibump == 0 ) then
          isotri(ielemn) = 0
        else if ( ibump == 1 ) then
          if ( jc == 1 .and. nbleft <= ic .and. ic < nbrite ) then
            isotri(ielemn) = 1
          else
            isotri(ielemn) = 0
          end if
        else if ( ibump == 2 ) then
          if ( nbleft <= ic .and. ic < nbrite ) then
            isotri(ielemn) = 1
          else
            isotri(ielemn) = 0
          end if
        else
          isotri(ielemn) = 1
        end if

      end if
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

  end do

  ! Para comprobar como va la cosa
  ! if ( 1 <= iwrite ) then 
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

 ! maxeqn = 2*mx*my + nx*ny  = 1213  ! Numero maximo de equaciones y funciones 
  if ( maxeqn < neqn ) then
    print*, ' Error : demasiadas incognitas! '
   !  print*, ' El máximo permitido es maxeqn = ', maxeqn
    stop
  end if
  return

end subroutine setgrd



subroutine setxy (  long, mx, my, np, nx, ny, xc, xlngth, yc, ylngth, ypert )
  ! setxy : Establecemos las coordenadas normales de la malla basandonos el el valor del parametro (ypert)
  ! Referencia : Finite Element Analysis, P. Seshu, 5.3.3 Natural Coordinates Triangular Elements pg. 165 

  integer :: my              ! mx : Numero de nodos en la direccion y (2ny - 1)
  integer :: mx              ! my : Numero de nodos en la direccion x (2nx - 1)
  real(8) :: xc(np)          ! xc : Las coordenada x de los nodos (Vector xc(np))
  real(8) :: yc(np)          ! yc : Las coordenadas y de los nodos (Vecor yc(np))
  real(8) :: xlngth, ylngth  ! Largo y ancho del cilindro (Def 10,3)
  real(8) :: ypert           ! ypert : La variable parametro que contiene nuestra altura del puente
  real(8) :: ybot, ylo
  logical :: long            ! Orientacion Vertical(V) u Horizontal (F), depende de nx, ny 


  ! Inicio  
  ! .........................>
  ! .........................>
  ! .........................>
  ! .........................>
  ! .........................>
  ! .........................>
  ! .........................>


  do ip = 1 , np ! np = mx*my, no. nodos = 533, para nuestro triangulo de 6 nodos cada uno 

    ! Aqui sencillamente determino el inicio y final de mi malla 
    if ( long ) then
      ic = ( ( ip - 1 ) / my ) + 1
      jc = mod ( ( ip - 1 ), my ) + 1
    else
      ic = mod ( ( ip - 1 ), mx ) + 1
      jc = ( ( ip - 1 ) / mx ) + 1
    end if

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




subroutine setqud ( area, isotri,  nelemn, nnodes, node, np, nquad, xc, xm, yc, ym )
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
  ! open(40 , file = 'triag_0.dat' , status = 'replace' )
  !   do i = 1, nelemn
  !     do j = 1, nquad 
  !       write(40,'(2f10.5)') xm(i,j), ym(i,j)
  !     end do
  !       write(40, *) ''
  !   end do
  ! close(40)

  return
end subroutine setqud 



subroutine setbas ( isotri, nelemn, nnodes, node, np, nquad, phi, psi, xc, xm, yc, ym )
  ! setbas : Evalúa las funciones base en cada punto de integración.
  !  Necesito 
  ! - Calcular la transformación de elementos (Jacobiano)
  ! - Evalúa las funciones (Base cuadratica)
  ! - Extrapolar la transformacion en el triangulo 
  ! 
  ! Referencias :
  ! - 6 nodes me 478 finite element method, chapter 6. isoparametric formulation
  ! - https://www.youtube.com/watch?v=gJzqCaOEqsA  (Minuto 28 - : 37)

  integer :: nelemn         ! El numero de elementos 
  integer :: nnodes         ! El número de nodos por elemento (6) Eelementos triangulares cuadráticos 
  integer :: np             ! El numero de nodos totales 
  integer :: nquad          ! El número de puntos de cuadratura por elemento (Definimos que son 3)
  integer :: iq, it, j      ! Iteradores habituales 
  integer :: isotri(nelemn) ! Contiene para cada elemento 1 si es isoparametrico o 0 
  integer :: node(nelemn,nnodes) ! Contiene por cada elemento el ind global de cada elemento del nodo

  real(8) :: bb             ! Valor de la funcion base en el triangulo de referencia 
  real(8) :: bx             ! Valor de la derivada de bb respecto a x 
  real(8) :: by             ! Valor de la derivada de bb respecto a y 
  real(8) :: bsp            ! Funcion : Evalúa la función de base lineal asociada con la presión.
  real(8) :: det            ! El determinante de la transformación, evaluado en (xq,yq)
  real(8) :: etax           ! Las entradas de la matriz jacobiana deta/dx evaluadas en (xq,yq)
  real(8) :: etay           ! Las entradas de la matriz jacobiana deta/dy evaluadas en (xq,yq)

  ! phi 
  ! contiene el valor de una función de base cuadrática o su derivada, evaluado en un punto 
  ! de cuadratura en  particular
  ! phi(i,j,k,1) es el valor de la base cuadrática, función asociada al nodo 
  ! local k en el elemento i, evaluada
  ! en el punto de cuadratura j.
  ! phi (i,j,k,2) es la derivada x de esa misma función base,
  ! phi (i,j,k,3) es la derivada y de esa misma función base.


  real(8) :: phi(nelemn,nquad,nnodes,3)

  ! psi 
  ! contiene el valor de una función de base lineal evaluada en un , punto de cuadratura
  ! psi(i,j,k) es el valor de la función de base lineal asociada, 
  ! con nodo local k en el elemento i evaluado en el punto de cuadratura j.


  real(8) :: psi(nelemn,nquad,nnodes)

  real(8) :: refbsp   ! Funcion : evalúa las funciones de base lineal en un triángulo de referencia.
  real(8) :: xc(np)   ! Coordenadas x de los nodos
  real(8) :: yc(np)   ! Coordenadas y de los nodos 
  real(8) :: xix      ! Las entradas de la matriz jacobiana dxi/dx evaluadas en (xq,yq)
  real(8) :: xiy      ! Las entradas de la matriz jacobiana dxi/dy evaluadas en (xq,yq)
  real(8) :: xm(nelemn,nquad) ! Arreglo de coordenadas x de los puntos de cuadratura de cada elemento
  real(8) :: xq               ! La coordenada x del punto en el que se evaluará el mapeo  
  real(8) :: ym(nelemn,nquad) ! Arreglo de coordenadas y de los puntos de cuadratura de cada elemento
  real(8) :: yq               ! La coordenada y del punto en el que se evaluará el mapeo  


  do it = 1, nelemn ! Primera iteracion hasta nuestro numero de elementos (240)
    do j = 1, nquad ! Segunda iteracion hata los puntos de cuadratura (3)
    xq = xm(it,j)   ! El calculo sobre nuestra i-esima coordenada en x
    yq = ym(it,j)   ! i-esima coordenada en y, vamos a aplicar una transformacion del triangulo 
      call trans(det,etax,etay,it,nelemn,nnodes,node,np,xc,xix,xiy,xq,yc,yq)
        do iq = 1, nnodes
          if ( isotri(it) == 0 ) then
            psi(it,j,iq) = bsp(it,iq,1,nelemn,nnodes,node,np,xc,xq,yc,yq)
              call qbf(xq,yq,it,iq,bb,bx,by,nelemn,nnodes,node,np,xc,yc)
          else
              call refqbf(xq,yq,iq,bb,bx,by,etax,etay,xix,xiy)
            psi(it,j,iq) = refbsp(xq,yq,iq)
          end if
        
        phi(it,j,iq,1) = bb
        phi(it,j,iq,2) = bx
        phi(it,j,iq,3) = by
        
        end do
    end do
  end do

  return
end subroutine setbas






subroutine trans ( det, etax, etay, it, nelemn, nnodes, node, np, xc, xix, xiy, xq, yc, yq )
  ! trans : calcula el mapeo de transformación de elementos.
  ! 
  ! Referencias : 
  ! - 6 nodes me 478 finite element method, chapter 6. isoparametric formulation
  ! - Finite Element Analysis, P. Seshu, 5.4.8 Natural Coordinates Triangular Elements pg. 193
  ! 
  ! El mapeo de transformación de elementos asigna el elemento de referencia 
  ! a un elemento isoparamétrico particular. La rutina también genera el determinante 
  ! y las derivadas parciales de la transformación.
  ! 
  ! Ejemplo : 
  ! Diagrama del mapeo desde nuestro punto de referencia hasta el elemento isoparamétrico:
  !
  !
  !          2                           2
  !         /|                          / \
  ! P.ref  4 5       ===== >  Elm.iso  4   5
  !       /  |                        /     \
  !      1-6-3                       1--6----3
  !
  !       Xi                              X
  !
  !    La forma del mapeo cuadratico seria : 
  !
  !      x = a1 * xi^2 + b1 * xi * eta + c1 * eta^2 + d1 * xi + e1 * eta + f1
  !      y = a2 * xi^2 + b2 * xi * eta + c2 * eta^2 + d2 * xi + e2 * eta + f2

  integer :: nelemn ! El numero de elementos 
  integer :: nnodes ! El número de nodos por elemento (6) Eelementos triangulares cuadráticos 
  integer :: np     ! El numero de nodos totales 
  integer :: i1     ! Se asigna al primer indice de mis nodos (it,1)
  integer :: i2     ! Se asigna al segundo indice de mis nodos (it,1)
  integer :: i3     ! Se asigna al tercer indice de mis nodos (it,1) 
  integer :: i4     ! Se asigna al cuarto indice de mis nodos (it,1)
  integer :: i5     ! Se asigna al quinto indice de mis nodos (it,1)
  integer :: i6     ! Se asigna al sexto indice de mis nodos (it,1)
  integer :: it     ! El índice del elemento que contiene (xq,yq).
  integer :: node(nelemn,nnodes) ! Contiene los índices de los nodos que componen cada elemento.
  
  real(8) :: a1     ! 2.0*x3 - 4.0*x6 + 2.0*x1
  real(8) :: a2     ! 2.0*y3 - 4.0*y6 + 2.0*y1
  real(8) :: b1     !-4.0*x3 - 4.0*x4 + 4.0*x5 + 4.0*x6
  real(8) :: b2     !-4.0*y3 - 4.0*y4 + 4.0*y5 + 4.0*y6
  real(8) :: c1     ! 2.0*x2 + 2.0*x3 - 4.0*x5
  real(8) :: c2     ! 2.0*y2 + 2.0*y3 - 4.0*y5
  real(8) :: d1     !-3.0*x1 - x3 + 4.0*x6
  real(8) :: d2     !-3.0*y1 - y3 + 4.0*y6
  real(8) :: det    ! El determinante de la transformación, evaluado en (xq,yq)
  real(8) :: dxdeta ! b1*xq + 2.0*c1*yq + e1
  real(8) :: dxdxi  ! 2.0*a1*xq + b1*yq + d1
  real(8) :: dydeta ! b2*xq + 2.0d0*c2*yq + e2
  real(8) :: dydxi  ! 2.0*a2*xq + b2*yq + d2
  real(8) :: e1     ! -x2 + x3 + 4.0*x4 - 4.0*x6
  real(8) :: e2     ! -y2 + y3 + 4.0*y4 - 4.0*y6
  real(8) :: etax   ! Las entradas de la matriz jacobiana deta/dx evaluadas en (xq,yq)
  real(8) :: etay   ! Las entradas de la matriz jacobiana deta/dy evaluadas en (xq,yq)
  real(8) :: x1     ! Asigno a mi node(it,1) su respectiva coordenada en x 
  real(8) :: x2     ! Asigno a mi node(it,2) su respectiva coordenada en x 
  real(8) :: x3     ! Asigno a mi node(it,3) su respectiva coordenada en x 
  real(8) :: x4     ! Asigno a mi node(it,4) su respectiva coordenada en x 
  real(8) :: x5     ! Asigno a mi node(it,5) su respectiva coordenada en x 
  real(8) :: x6     ! Asigno a mi node(it,6) su respectiva coordenada en x 
  real(8) :: xc(np) ! Las coordenadas x de los nodos 
  real(8) :: xix    ! Las entradas de la matriz jacobiana dxi/dx evaluadas en (xq,yq)
  real(8) :: xiy    ! Las entradas de la matriz jacobiana dxi/dy evaluadas en (xq,yq)
  real(8) :: xq     ! La coordenada x del punto en el que se evaluará el mapeo  
  real(8) :: y1     ! Asigno a mi node(it,1) su respectiva coordenada en y 
  real(8) :: y2     ! Asigno a mi node(it,2) su respectiva coordenada en y 
  real(8) :: y3     ! Asigno a mi node(it,3) su respectiva coordenada en y 
  real(8) :: y4     ! Asigno a mi node(it,4) su respectiva coordenada en y 
  real(8) :: y5     ! Asigno a mi node(it,5) su respectiva coordenada en y 
  real(8) :: y6     ! Asigno a mi node(it,6) su respectiva coordenada en y 
  real(8) :: yc(np) ! Las coordenadas y de los nodos 
  real(8) :: yq     ! La coordenada y del punto en el que se evaluará el mapeo

  ! Llenamos los indices 1, 2, 3, 4, 5, 6, a sus respectivos nodos 

  i1 = node(it,1)
  i2 = node(it,2)
  i3 = node(it,3)
  i4 = node(it,4)
  i5 = node(it,5)
  i6 = node(it,6)

  ! Los 6 puntos (Cada nodo) le asignamos su respectiva coordenada en x, y 

  x1 = xc(i1)
  y1 = yc(i1)
  x2 = xc(i2)
  y2 = yc(i2)
  x3 = xc(i3)
  y3 = yc(i3)
  x4 = xc(i4)
  y4 = yc(i4)
  x5 = xc(i5)
  y5 = yc(i5)
  x6 = xc(i6)
  y6 = yc(i6)
  !
  ! Establecer los coeficientes en la transformación:
  !
  !  X = X(XI,ETA)
  !  Y = Y(XI,ETA)
  !
  a1 = 2.0D+00*x3-4.0D+00*x6+2.0D+00*x1
  b1 = -4.0D+00*x3-4.0D+00*x4+4.0D+00*x5+4.0D+00*x6
  c1 = 2.0D+00*x2+2.0D+00*x3-4.0D+00*x5
  d1 = -3.0D+00*x1-x3+4.0D+00*x6
  e1 = -x2+x3+4.0D+00*x4-4.0D+00*x6

  a2 = 2.0D+00*y3-4.0D+00*y6+2.0D+00*y1
  b2 = -4.0D+00*y3-4.0D+00*y4+4.0D+00*y5+4.0D+00*y6
  c2 = 2.0D+00*y2+2.0D+00*y3-4.0D+00*y5
  d2 = -3.0D+00*y1-y3+4.0D+00*y6
  e2 = -y2+y3+4.0D+00*y4-4.0D+00*y6

  !
  ! Calcular derivadas parciales d x/deta, d x/dxi, d y/deta/ dy/d xi,
  ! en el punto (xq,yq) en el triángulo de referencia
  ! Este es el jacobiano:
  !   ( dx/dxi    dx/deta )
  !   ( dy/dxi    dy/deta )
  !
  dxdxi = 2.0d0*a1*xq + b1*yq + d1
  dxdeta = b1*xq + 2.0d0*c1*yq + e1
  dydxi = 2.0d0*a2*xq + b2*yq + d2
  dydeta = b2*xq + 2.0d0*c2*yq + e2

  ! 
  ! Calcule el determinante de la transformación
  !

  det =  (2.0d0*a1*b2-2.0d0*a2*b1)*xq*xq +(4.0d0*a1*c2-4.0d0*a2*c1)*xq*yq &
        +(2.0d0*b1*c2-2.0d0*b2*c1)*yq*yq +(2.0d0*a1*e2+b2*d1-b1*d2-2.0d0*a2*e1)*xq &
        +(2.0d0*c2*d1+b1*e2-b2*e1-2.0d0*c1*d2)*yq+d1*e2-d2*e1
  !
  ! Calculamos el Jacobiano inverso 
  ! 
  !    ( dxi/dx   dxi/dy  )
  !    ( deta/dx  deta/dy )
  !

  xix  =   dydeta  / det
  xiy  = - dxdeta / det
  etax = - dydxi / det
  etay =   dxdxi / det

  return
end subroutine trans




subroutine qbf ( xq, yq, it, in, bb, bx, by, nelemn, nnodes, node, np, xc, yc )
  ! qbf : evalúa las funciones de base cuadrática.
  ! Referencias : 
  ! Finite Elements for the (Navier) Stokes Equations - John Burkardt, Numerical Analysis Seminar


  integer :: nelemn ! El numero de elementos 
  integer :: nnodes ! El número de nodos por elemento (6) Eelementos triangulares cuadráticos 
  integer :: np     ! El numero de nodos totales 
  integer :: i1     ! Se asigna al primer indice de mis nodos  (it,1)
  integer :: i2     ! Se asigna al segundo indice de mis nodos (it,2)
  integer :: i3     ! Se asigna al tercer indice de mis nodos  (it,3)
  integer :: in     ! El indice de una función base en el triángulo de referencia.
  integer :: in1    ! Primera iteracion respecto al primer indice de los nodos (i1)
  integer :: in2    ! Segunda iteracion respecto al primer indice de los nodos (i2)
  integer :: in3    ! Tercera iteracion respecto al primer indice de los nodos (i3)
  integer :: inn    ! Guardamos los valores (Cambia)
  integer :: it     ! El elemento en el que se basa la función se define.
  integer :: j1     ! Guarda de manera iterativa el valor de i1 
  integer :: j2     ! Guarda de manera iterativa el valor de i2 
  integer :: j3     ! Guarda de manera iterativa el valor de i2 
  integer :: node(nelemn,nnodes) ! Contiene los índices de los nodos que componen cada elemento.

  ! Coeficientes de la funcion lineal (Diapositiva 35 - Ref) 
  real(8) :: s      
  real(8) :: t
  real(8) :: c
  real(8) :: d

  real(8) :: xc(np) ! Las coordenadas x de los nodos 
  real(8) :: yc(np) ! Las coordenadas y de los nodos 

  !  bb, bx, by, el valor de la función base y sus derivadas x e y en el punto (x,y)
  real(8) :: bb
  real(8) :: bx
  real(8) :: by
 
  ! xq, yq,las coordenadas de un punto en el triángulo de referencia.
  real(8) :: xq
  real(8) :: yq

  ! Vamos a constuir las funciones de base lineal
  ! Las funciones de base para la presión se definen en el triángulo de tres vértices
  ! triángulo T = {(x1 , y1 ), (x2 , y2 ), (x3 , y3 )}. La base φ1 (x, y ) es 1 en
  ! vértice 1, 0 en los otros dos vértices, y lineal sobre T .


  if ( in <= 3 ) then ! Si sigue perteneciendo a un triangulo de 3 vertices 
    in1 = in
    in2 = mod(in,3)+1
    in3 = mod(in+1,3)+1
    i1 = node(it,in1)
    i2 = node(it,in2)
    i3 = node(it,in3)
    d = (xc(i2)-xc(i1))*(yc(i3)-yc(i1))-(xc(i3)-xc(i1))*(yc(i2)-yc(i1))
    t = 1.0d0 +((yc(i2)-yc(i3))*(xq-xc(i1))+(xc(i3)-xc(i2))*(yq-yc(i1)))/d
    bb = t*(2.0d0*t-1.0d0)
    bx = (yc(i2)-yc(i3))*(4.0d0*t-1.0d0)/d
    by = (xc(i3)-xc(i2))*(4.0d0*t-1.0d0)/d
  else
    inn = in-3
    in1 = inn
    in2 = mod(inn,3)+1
    in3 = mod(inn+1,3)+1
    i1 = node(it,in1)
    i2 = node(it,in2)
    i3 = node(it,in3)
    j1 = i2
    j2 = i3
    j3 = i1
    d = (xc(i2)-xc(i1))*(yc(i3)-yc(i1))-(xc(i3)-xc(i1))*(yc(i2)-yc(i1))
    c = (xc(j2)-xc(j1))*(yc(j3)-yc(j1))-(xc(j3)-xc(j1))*(yc(j2)-yc(j1))
    t = 1.0d0+((yc(i2)-yc(i3))*(xq-xc(i1))+(xc(i3)-xc(i2))*(yq-yc(i1)))/d
    s = 1.0d0+((yc(j2)-yc(j3))*(xq-xc(j1))+(xc(j3)-xc(j2))*(yq-yc(j1)))/c
    bb = 4.0d0 * s*t
    bx = 4.0d0 * (t*(yc(j2)-yc(j3))/c+s*(yc(i2)-yc(i3))/d)
    by = 4.0d0 * (t*(xc(j3)-xc(j2))/c+s*(xc(i3)-xc(i2))/d)
  end if


  ! Al igual que para las funciones de base lineal, podemos encontrar una función lineal
  ! que es cero a lo largo de cualquier línea que elijamos. Por tanto, existe una función lineal
  ! que es cero en N2 y N1 (y, por tanto, también en N12).
  ! Otra función lineal es cero en N23 y N31, y así sucesivamente.

  return
end subroutine qbf 




subroutine refqbf ( x, y, in, bb, bx, by, etax, etay, xix, xiy )
  ! refqbf :  evalúa las funciones de base cuadrática en el triángulo de referencia!
  ! Referencias : Finite Elements: Basis functions triangles: quadratic elements (Presentacion) 
  ! 
  ! 
  !  Ejemplo de nuestro triangulo 
  !
  !    3
  !    |\
  !    6 5
  !    |  \
  !    1-4-2
  !

  !  bb, bx, by, el valor de la función base y sus derivadas x e y en el punto (x,y)
  real(8) :: bb
  real(8) :: bx
  real(8) :: by

  real(8) :: etax     ! Las entradas de la matriz jacobiana deta/dx evaluadas en (xq,yq)
  real(8) :: etay     ! Las entradas de la matriz jacobiana deta/dy evaluadas en (xq,yq)

  real(8) :: tbx
  real(8) :: tby

  real(8) :: x      ! X(*) el vector a escalar en x 
  real(8) :: y      ! y(*) el vector a escalar en y 

  real(8) :: xix    ! Las entradas de la matriz jacobiana dxi/dx evaluadas en (xq,yq)
  real(8) :: xiy    ! Las entradas de la matriz jacobiana dxi/dy evaluadas en (xq,yq)

  integer :: in     ! El indice de una función base en el triángulo de referencia.

  ! Ejemplo de la posicion de los nodos en nuestro triangulo        
  ! 
  !             1 
  !        4 _ / \ _ 6    
  !       2 _ / _ \ _ 3  
  !             5    
  ! 


  ! (Diapositiva 13 y 14)
  ! Nodo 1 
  if ( in == 1) then
    bb  = 1.0d0 - 3.0d0*x + 2.0d0*x*x
    tbx = -3.0d0 + 4.0d0*x
    tby = 0.0d0
  ! Nodo 2 
  else if ( in == 2) then
    bb  = -y + 2.0d0*y*y
    tbx = 0.0d0
    tby = -1.0d0 + 4.0d0*y
  ! Nodo 3 
  else if (in == 3) then
    bb  = -x + 2.0d0*x*x+y - 4.0d0*x*y + 2.0d0*y*y
    tbx = -1.0d0 + 4.0d0*x - 4.0d0*y
    tby =  1.0d0 - 4.0d0*x + 4.0d0*y
  ! Nodo 4 
  else if ( in == 4) then
    bb = 4.0d0*y - 4.0d0*x*y
    tbx = -4.0d0*y
    tby = 4.0d0 - 4.0d0*x
  ! Nodo 5 
  else if ( in == 5) then
    bb = 4.0d0*x*y - 4.0d0*y*y
    tbx = 4.0d0*y
    tby = 4.0d0*x - 8.0d0*y
  ! Nodo 6 
  else if ( in == 6) then
    bb = 4.0d0*x-4.0d0*x*x - 4.0d0*y + 4.0d0*x*y
    tbx = 4.0d0-8.0d0*x + 4.0d0*y
    tby = -4.0d0 + 4.0d0*x
  else
    write(*,*)'refqbf - Error! Valor erroneo de in' , in
    stop
  end if

  bx = tbx * xix + tby * etax
  by = tbx * xiy + tby * etay

  return
end subroutine refqbf



function bsp ( it, iq, id, nelemn, nnodes, node, np, xc, xq, yc, yq )
  ! bsp :  evalúa la función de base lineal asociada con la presión.
  ! Referencia  :
  ! 
  ! Ejemplo del elemento de referencia local:
  !
  !    ^
  !    |
  !    1  3
  !    |  |\
  !    |  | \
  !    |  |  \
  !    |  |   \
  !    0  1----2
  !    |
  !    +--0----1---->
  !


  integer :: nelemn    ! Numero de elementos 
  integer :: nnodes    ! 6 nodos para los elementos triangulares cuadráticos en uso aquí.
  integer :: np        ! El numero de nodos totales 
  integer :: g1        ! Indice global del nodo 1 
  integer :: g2        ! Indice global del nodo 2 
  integer :: g3        ! Indice global del nodo 3 
  integer :: i4_wrap   ! Funcion : Determina a un I4 a estar entre los límites dados.
  integer :: id        ! ID del nodo (1, 2, 3)
  integer :: iq        ! Indice de una función base en el triángulo de referencia.
  integer :: it        ! El elemento en el que se basa la función se define.
  integer :: l1        ! Primer indice de nodo local
  integer :: l2        ! Segundo indice del nodo local
  integer :: l3        ! Tercer indice del nodo local 
  integer :: node(nelemn,nnodes) ! Indices de nodo global de los nodos de elemento.

  real(8) :: bsp       ! Esta funcion 
  real(8) :: d         ! Coeficiente de la funcion lineal    
  real(8) :: xc(np)    ! Las coordenadas x de los nodos 
  real(8) :: yc(np)    ! Las coordenadas y de los nodos 
  real(8) :: xq        ! Coord. x del punto trinagular de referencia 
  real(8) :: yq        ! Coord. y del punto triangular de referencia 


  !  L1, L2, L3 son los índices de nodos locales apropiados.
  !
  l1 = iq
  l2 = i4_wrap ( iq + 1, 1, 3 )
  l3 = i4_wrap ( iq + 2, 1, 3 )
  !
  !  G1, G2, G3 son los índices de nodos globales.
  !
  g1 = node(it,l1)
  g2 = node(it,l2)
  g3 = node(it,l3)

  d = (xc(g2)-xc(g1))*(yc(g3)-yc(g1))-(xc(g3)-xc(g1))*(yc(g2)-yc(g1))

  if ( id == 1 ) then
    bsp = 1.0d0 +((yc(g2)-yc(g3))*(xq-xc(g1))+(xc(g3)-xc(g2))*(yq-yc(g1)))/d
  else if ( id == 2 ) then
    bsp = (yc(g2)-yc(g3))/d
  else if ( id == 3 ) then
    bsp = (xc(g3)-xc(g2))/d
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) ' BSP - Error!'
    write ( *, '(a)' ) ' Valor de índice local ilegal para base lineal.'
    write ( *, '(a)' ) ' Los valores pueden ser : 1, 2 or 3.'
    write ( *, '(a,i6)' ) ' El valor obtenido fue :', id
    stop
  end if

  return
end function bsp 




function refbsp ( xq, yq, iq )
  ! refbsp : evalua las funciones de base lineal en un triángulo de referencia.
  ! 
  ! (Ejemplo)
  ! El triangulo de referencia dado aquí no es el mejor. los nodos no se dan en las posiciones habituales,
  ! y se enumeran en orden en el sentido de las agujas del reloj en lugar de orden en el sentido 
  ! contrario a las agujas del reloj
  ! 
  !   Diagrama:
  ! 
  !        2
  !       /|
  !      / |
  !     /  |
  !    1---3
  !


  integer :: iq      ! Indice de una función base en el triángulo de referencia.
  real(8) :: refbsp  ! Valor de la función base iq-ésimaen el punto (xq,yq) del triángulo de referencia.
  ! xq, yq,las coordenadas de un punto en el triángulo de referencia.
  real(8) :: xq
  real(8) :: yq

  if ( iq == 1 ) then
    refbsp = 1.0d0 - xq
  else if ( iq == 2 ) then
    refbsp = yq
  else if ( iq == 3 ) then
    refbsp = xq - yq
  end if

  return
end function refbsp



function i4_wrap( ival, ilo, ihi )
  ! i4_wrap : Hace que un I4 a estar entre los límites dados al envolver.
  !
  !  Ejemplo:
  !
  !    ilo = 4, ihi = 8
  !
  !    I    Valor
  !
  !    -2     8
  !    -1     4
  !     0     5
  !     1     6
  !     2     7
  !     3     8
  !     4     4
  !     5     5
  !     6     6
  !     7     7
  !     8     8
  !     9     4
  !    10     5
  !    11     6
  !    12     7
  !    13     8
  !    14     4

  integer :: i4_modp  ! Funcion : Devuelve el resto no negativo de la división I4.
  integer :: i4_wrap  ! Valor de la funcion 
  integer :: ihi      ! Limite superior deseado para el valor 
  integer :: ilo      ! Limite inferior deseado para el valor 
  integer :: ival     ! Un valor entero 
  integer :: jhi      ! Maximo entre los limites 
  integer :: jlo      ! Minimo entre los limites 
  integer :: value    ! Valor de Jenifer Lopez
  integer :: wide     ! Se encuentra entre los limites 

  jlo = min ( ilo, ihi )
  jhi = max ( ilo, ihi )

  wide = jhi - jlo + 1

  if ( wide == 1 ) then
    value = jlo
  else
    value = jlo + i4_modp ( ival - jlo, wide )
  end if

  i4_wrap = value

  return
end function i4_wrap




function i4_modp ( i, j )
  ! i4_modp : Devuelve el resto no negativo de la división (modulo) i4.
  ! Ejemplo Algoritmo:
  ! si  
  !   nrem = i4_modp (i, j)
  !   nmult = (i - nrem) / j
  ! entonces
  !   i = j * nmult + nrem
  ! (donde nrem siempre es no negativo) 
  ! La función mod calcula un resultado con el mismo signo que la cantidad que se divide. 
  ! por lo tanto, supongamos que tenemos un ángulo a y nos queremos asegurar
  ! de que estaba entre 0 y 360.
  ! 
  ! Entonces mod(a,360) funcionaría, si a fuera positivo, pero si a
  ! fue negativo, el resultado estaría entre -360 y 0.
  ! por otro lado, i4_modp(a,360) está siempre entre 0 y 360, siempre :) 

  integer :: i         ! El número a dividir
  integer :: i4_modp   ! La funcion esta 
  integer :: j         ! El numero que divie i 
  integer :: value     ! Valor de regreso 

  if ( j == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'i4_modp - Error!'
    write ( *, '(a,i8)' ) ' Division erronea = ', j
    stop
  end if

  value = mod ( i, j )

  if ( value < 0 ) then
    value = value + abs ( j )
  end if

  i4_modp = value

  return
end function i4_modp


subroutine setlin ( iline, indx,  long, mx, my, np, nx, ny,  xlngth, xprof )
  ! setlin : Determina números de nodos desconocidos (incognitas) a lo largo de la línea de perfil.
  ! Para nuestro problema, la línea de perfil tiene la ecuación x = xprof.
      
  integer :: nx ! Espaciado de los nodos en x (hay 2*nx+1) 
  integer :: ny ! Espaciado de los nodos en y (hay 2*ny+1) 
  integer :: my ! my = 2*ny -1  Numero de nodos en y 
  integer :: mx ! mx = 2*nx -1  Numero de nodos en x 
  integer :: indx(np,2) ! Contiene, para cada nodo I, el indice de las velocidades U y V en ese nodo, o 0.
  integer :: np           ! mx*my Numero de nodos  
  integer :: i            ! Iterador 
  integer :: iline(my)    ! index del coef del elemento finito asociado a cada nodo del perfil
  ! xc(ip) : Coordenadas x de los nodos 
  ! yc(ip) : Coordenadas y de los nodos 
  integer :: ip
  integer :: itemp       ! Numero de nodos en la linea de perfil
  integer :: nodex0      ! Encima del nodo  , nodex0
  real(8) :: xlngth     ! Largo de la region (xf - x0)
  real(8) :: xprof      ! Cord x en la que se mide el perfil
  logical :: long       ! Orientacion Vertical(V) u Horizontal (F), depende de nx, ny 

  !
  !  Determina el numero de nodos en la linea de perfil
  !
  itemp = nint ( ( 2.0d0 * real ( nx - 1, kind = 8 ) * xprof ) / xlngth )

  if ( long ) then ! Si la orientacion es Vertical 
    nodex0 = itemp * ( 2 * ny - 1 ) + 1
  else              ! Orientacion horizontal
    nodex0 = itemp + 1
  end if

  ! write(*,*)' '
  ! write(*,*)' setlin:'
  ! write(*,*)'  Perfil generado en x :',xprof
  ! write(*,*)'  Eue está encima del nodo :',nodex0


  do i = 1, my
    if ( long ) then ! Orientacion vertical 
      ip = nodex0+(i-1)
    else             ! Orientacion horizontal 
      ip = nodex0+mx*(i-1)
    end if
    iline(i) = indx(ip,1)
  end do

  ! Prueba y error :(  
  ! if ( 1 <= iwrite ) then
  !   write(*,*)' '
  !   write(*,*)'  Indices de incógnitas a lo largo de la línea de perfil:'
  !   write(*,*)' '
  !   write(*,'(5i5)') iline(1:my)
  ! end if

  return
end subroutine setlin






subroutine setban ( indx, insc, maxrow, nband, nelemn, nlband, nnodes, node, np, nrow )
  ! setban : Calcula el ancho de media banda 
  ! Esto sólo se aplicaría para un programa de EF simple en el que los nodos del modelo siempre 
  ! están etiquetados de 1 a n, cada nodo tiene el mismo número de grados de libertad (f)
  ! y las filas y columnas de la matriz global también se ordenan en grupos de (f) de 1 a n
  ! Referencias : (Este la verdad no le entiendo mucho, pero si no me da error) :( 
  ! El codigo esta en Programming the Isoparametric Six Node Triangle, Carlos A. Felippa


  integer :: nelemn ! 2*(nx - 1)*(ny -1) ! Numero de elementos 
  integer :: nnodes ! No. nodos por elemento (6 triangulares cuadráticos)
  integer :: np     ! mx*my  Numero de nodos  
  integer :: i      ! Iterador 
  integer :: indx(np,2) ! Contiene para cada iesimo nodo el index de la velocidad u, v (Puede 0)
  integer :: insc(np)   ! Contiene para cada iesimo nodo el index de la presion p (Puede 0)
  integer :: ip         ! Iterador de los nodos
  integer :: ipp        ! Iterador de nnodes 
  integer :: iq         ! Indice y de nodes 
  integer :: iqq        ! Indice y de node
  integer :: it         ! Indice x de indx 
  integer :: iuk        ! Iterador de 1 a 3 
  integer :: iukk       ! Iterador de 1 a 3 
  integer :: j          ! indx(ipp,iukk)
  integer :: maxrow     ! 27*ny  La ultima fila de la matriz A  
  integer :: nband      ! Ancho de banda del sistema lineal 
  integer :: nlband     ! Ancho de banda inferior de la matriz
  integer :: node(nelemn,nnodes) ! Contiene por cada elemento el ind global de cada elemento del nodo
  integer :: nrow       ! Sumador restador de las n bandas 

  nlband = 0

  do it = 1, nelemn
    do iq = 1, nnodes
        ip = node(it,iq)
          do iuk = 1, 3
            if (iuk == 3) then
              i = insc(ip)
              else
              i = indx(ip,iuk)
            end if
            if ( 0 < i ) then
              do iqq = 1, nnodes
                ipp = node(it,iqq)
                do iukk = 1, 3
                  if (iukk == 3) then
                    j = insc(ipp)
                    else
                    j = indx(ipp,iukk)
                  end if
                  if ( 0 < j ) then
                    nlband = max(nlband,j-i)
                  end if
                end do
              end do
            end if
          end do
    end do
  end do

  nband = nlband + nlband + 1
  nrow = nlband + nlband + nlband + 1

  ! write(*,*)' '
  ! write(*,*)' setban:'
  ! write(*,*)' Ancho de banda inferior :',nlband
  ! write(*,*)' Ancho de banda total :',nband
  ! write(*,*)' Filas de matriz requeridas :',nrow

  if ( maxrow < nrow ) then
    write(*,*)' setban - ¡nrow es demasiado grande!'
    write(*,*)' El máximo permitido es ',maxrow
    stop
  end if

  return
end subroutine setban



! --------- Inicio Solución de Navier Stokes, utilizando una estimación inicial de g = 0  ------------ ! 


subroutine nstoke ( a, area, f, g, indx, insc, isotri, maxnew,maxrow, nband, nelemn, neqn, nlband, & 
  nnodes, node, np, nquad, nrow, numnew, phi, psi, reynld, tolnew, xc, xm, yc, ym )
  ! 
  ! nstoke : Resuelve la ecuación de Navier Stokes utilizando elementos de Taylor-Hood.
  ! Referencias : Basicamente es este codigo del Pitón trasladado a Fortran con mucho cafe 
  ! Documentacion 
  ! https://fenicsproject.org/olddocs/dolfin/2016.2.0/python/demo/documented/stokes-taylor-hood/
  ! Codigo fuente 
  ! https://bitbucket.org/fenics-project/dolfin/src/master/python/src/
  ! Tambien tome ejemplos de esto 
  ! https://www.mathworks.com/matlabcentral/fileexchange/49169-triangular-taylor-hood-finite-elements
    
  integer :: maxrow ! 27*ny La ultima fila de la matriz A  
  integer :: nelemn ! 2*mx*my + nx*ny Numero maximo de equaciones y funciones 
  integer :: neqn   ! Contador del numero de ecuaciones 
  integer :: nnodes ! 6 No. nodos por elemento (6 triangulares cuadráticos)
  integer :: np     ! mx*my, numero de nodos  
  integer :: nquad  ! El número de puntos de cuadratura por elemento.
  integer :: idamax ! Funcion : Encuentra el índice del elemento vectorial de máximo valor absoluto.!
  integer :: indx(np,2) ! contiene, para cada nodo i, el indice de las velocidades u y v en ese nodo, o 0.
  integer :: insc(np)   ! Contiene para cada iesimo nodo el index de la presion p (Puede 0)
  integer :: isotri(nelemn) ! Contiene para cada elemento 1 si es isoparametrico o 0 
  integer :: iter           ! Un iterador fancy 
  integer :: itype          ! Condicional para Soluciones Navier Stokes 
  integer :: maxnew         ! Numero maximo de pasos sol. N-S por iteracion
  integer :: nband          ! Ancho de banda del sistema lineal 
  integer :: nlband         ! Ancho de banda inferior de la matriz
  integer :: node(nelemn,nnodes) ! Contiene por cada elemento el ind global de cada elemento del nodo
  integer :: nrow                ! Numero de filas necesarias para guardar la matriz A
  integer :: numnew              ! Contador de numeros totales de pasos 

  real(8) :: a(maxrow,neqn)      ! Contiene nuestro arreglo principal 
  real(8) :: area(nelemn)        ! Area total de los elementos 
  real(8) :: diff                ! Condicional de convergencia para NS 
  real(8) :: f(neqn)             ! Funcion f(x) (Los coeficientes de los elementos finitos)
  real(8) :: g(neqn)             ! Funcion g(x) (Estimación inicial g = 0.)
 
  ! phi (1)
  ! contiene el valor de una función de base cuadrática o su derivada, evaluado en un punto 
  ! de cuadratura en  particular
  ! phi(i,j,k,1) es el valor de la base cuadrática, función asociada al nodo 
  ! local k en el elemento i, evaluada
  ! en el punto de cuadratura j.
  ! phi (i,j,k,2) es la derivada x de esa misma función base,
  ! phi (i,j,k,3) es la derivada y de esa misma función base.

  real (8) :: phi(nelemn,nquad,nnodes,3) ! (1)

  ! psi (2)
  ! contiene el valor de una función de base lineal evaluada en un , punto de cuadratura
  ! psi(i,j,k) es el valor de la función de base lineal asociada, 
  ! con nodo local k en el elemento i evaluado en el punto de cuadratura j.

  real(8) :: psi(nelemn,nquad,nnodes) ! (2)

  real(8) :: reynld    ! El valor del no. Reynolds (2300 Flujo laminar)
  real(8) :: tolnew    ! Tolerancia de convergencia de pasos sol. N-S por iteracion
  real(8) :: xc(np)    ! Coordenadas x de los nodos 
  real(8) :: xm(nelemn,nquad)  ! Cord x de los puntos de cuadratura de cada elemento
  real(8) :: yc(np)            ! Coordenada y de los nodos 
  real(8) :: ym(nelemn,nquad)  ! Cord y de los puntos de cuadratura de cada elemento

  ! G contiene una estimación inicial de la solución.
  do iter = 1, maxnew ! Lo definimos como 10 
    numnew = numnew + 1
    itype = -1

    ! Procedemos a resolver las ecuaciones de NS linealizadas 
    call linsys ( a, area, f, g, indx, insc, isotri, itype, maxrow, nband, nelemn, neqn,  & 
      nlband, nnodes, node, np, nquad, nrow, phi, psi, reynld, xc, xm, yc, ym )

  !  Comprobamos la convergencia.

    g(1:neqn) = g(1:neqn) - f(1:neqn)
    diff = abs ( g(idamax(neqn,g,1)) )

    ! Para ver los valores de iteracion
    ! write(*,*) '  nstoke: iteracion ', iter, ' maxnorm(diff) = ', diff

    g(1:neqn) = f(1:neqn)

    ! Converge 
    if ( diff <= tolnew ) then
    !  write(*,*) ' nstoke convergio con ', iter , 'iteraciones'
      exit
    end if
    
    ! Hemos llegado al numero maximo de iteracion (Aumentar o cambiar valores)
    if ( iter == maxnew ) then 
    !  write(*,*) '  nstoke fallo!'
      stop
    end if

  end do

  return
end subroutine nstoke 




subroutine linsys ( a, area, f, g, indx, insc, isotri, itype, maxrow, nband, nelemn, neqn, &
                  nlband, nnodes, node, np, nquad, nrow, phi, psi, reynld, xc, xm, yc, ym )

  ! linsys : Resuelve las ecuaciones de Navier - Stokes linealizadas 
  !
  !    itype = -1 para soluciones de las ecuaciones de navier stokes 
  !    itype = -2 para soluciones de las ecuaciones de sensivilidad 
  !

  integer :: maxrow ! 27*ny , la ultima fila de la matriz A  
  integer :: nelemn ! 2*(nx - 1)*(ny -1) , numero de elementos
  integer :: neqn   ! Numero de ecuaciones y funciones
  integer :: nnodes ! 6, no. nodos por elemento (6 triangulares cuadráticos)
  integer :: np     ! mx*my , Numero de nodos totales 
  integer :: nquad  !  3 , el número de puntos de cuadratura por elemento.
  integer :: i      ! Iterador 
  integer :: ihor   ! indx(ip,1)
  integer :: indx(np,2) ! Contiene, para cada nodo I, el índice de las velocidades U y V en ese nodo, o 0.
  integer :: info       ! Avisa si hay algun error o valor normal 
  integer :: insc(np)   ! Contiene para cada iesimo nodo el index de la presion p (Puede 0)
  integer :: ioff       ! i = neqn-j+ioff
  integer :: ip         ! node(it,iq)
  integer :: ipivot(neqn)   ! Pivotes para el n numero de ecuaciones 
  integer :: ipp            ! node(it,iqq)
  integer :: iprs           ! insc(ip)
  integer :: iq             ! Iterador de 1, 2 , 3 nodos 
  integer :: iqq            ! Iterador de 1 a nnodes 
  integer :: iquad          ! Iterador de 1 a 3 
  integer :: isotri(nelemn) ! Contiene para cada elemento 1 si es isoparametrico o 0 
  integer :: it             ! Iterador fancy 
  integer :: itype          ! Condicional para Soluciones Navier Stokes 
  integer :: iuse           ! ihor-jp+ioff
  integer :: iver           ! indx(ip,2)
  integer :: j              ! Iterador (Este no es fancy)
  integer :: job            ! Recordatorio de conseguir un trabajo 
  integer :: jp             ! insc(ipp)
  integer :: ju             ! indx(ipp,1)
  integer :: jv             ! indx(ipp,1)
  integer :: nband          ! Ancho de banda total 
  integer :: nlband         ! Ancho de banda inferior de la matriz
  integer :: node(nelemn,nnodes) ! Contiene por cada elemento el ind global de cada elemento del nodo
  integer :: nrow                ! Numero de filas necesarias para guardar la matriz A 

  real(8) :: a(maxrow,neqn)      ! Contiene nuestro arreglo principal 
  real(8) :: aij                 ! ar*(visc*(by*bby+bx*bbx) +bb*(bbb*unx(1)+bbx*un(1)+bby*un(2)))
  real(8) :: ar                  ! area(it) / 3.0d0
  real(8) :: area(nelemn)        ! Area total de los elementos 
  real(8) :: bb                  ! Valor de la funcion base en el triangulo de referencia 
  real(8) :: bbb                 ! phi(it,iquad,iqq,1)
  real(8) :: bbbl                ! psi(it,iquad,iqq)
  real(8) :: bbl                 ! psi(it,iquad,iq)
  real(8) :: bbx                 ! phi(it,iquad,iqq,2)
  real(8) :: bby                 ! phi(it,iquad,iqq,3)
  real(8) :: bx                  ! Valor de la derivada de bb respecto a x 
  real(8) :: by                  ! Valor de la derivada de bb respecto a y 
  real(8) :: det                 ! El determinante de la transformación, evaluado en (xq,yq)
  real(8) :: etax                ! Las entradas de la matriz jacobiana deta/dx evaluadas en (xq,yq)
  real(8) :: etay                ! Las entradas de la matriz jacobiana deta/dy evaluadas en (xq,yq
  real(8) :: f(neqn)             ! Funcion f(x) (Los coeficientes de los elementos finitos)
  real(8) :: g(neqn)             ! Funcion g(x) (Estimación inicial g = 0.)
 
 
  ! phi (1)
  ! contiene el valor de una función de base cuadrática o su derivada, evaluado en un punto 
  ! de cuadratura en  particular
  ! phi(i,j,k,1) es el valor de la base cuadrática, función asociada al nodo 
  ! local k en el elemento i, evaluada
  ! en el punto de cuadratura j.
  ! phi (i,j,k,2) es la derivada x de esa misma función base,
  ! phi (i,j,k,3) es la derivada y de esa misma función base.

  real (8) :: phi(nelemn,nquad,nnodes,3) ! (1)

  ! psi (2)
  ! contiene el valor de una función de base lineal evaluada en un , punto de cuadratura
  ! psi(i,j,k) es el valor de la función de base lineal asociada, 
  ! con nodo local k en el elemento i evaluado en el punto de cuadratura j.

  real(8) :: psi(nelemn,nquad,nnodes) ! (2)

  real(8) :: reynld     ! El valor del no. Reynolds (2300 Flujo laminar)
  real(8) :: ubc        ! ubdry(2,yc(ipp))
  real(8) :: ubdry      ! Funcion : Flujo parabólico de entrada en términos del valor del parámetro
  real(8) :: ubump      ! Funcion : Calcula la sensibilidad dU/dA en la protuberancia.
  real(8) :: un(2)      ! un(iuk) = un(iuk)+bb*ubc
  real(8) :: unx(2)     ! unx(iuk) = unx(iuk)+bx*ubc
  real(8) :: uny(2)     ! uny(iuk) = uny(iuk)+by*ubc
  real(8) :: visc       ! 1.0d0 / reynld
  real(8) :: xc(np)     ! Coordenadas x de los nodos 
  real(8) :: xix        ! Las entradas de la matriz jacobiana dxi/dx evaluadas en (xq,yq)
  real(8) :: xiy        ! Las entradas de la matriz jacobiana dxi/dy evaluadas en (xq,yq)
  real(8) :: xm(nelemn,nquad) ! Arreglo de coordenadas x de los puntos de cuadratura de cada elemento
  real(8) :: xq               ! La coordenada x del punto en el que se evaluará el mapeo
  real(8) :: yc(np)           ! Coordenadas y de los nodos 
  real(8) :: ym(nelemn,nquad) ! Arreglo de coordenadas y de los puntos de cuadratura de cada elemento
  real(8) :: yq               ! La coordenada y del punto en el que se evaluará el mapeo  

  ioff = nlband + nlband + 1
  visc = 1.0d0 / reynld

  f(1:neqn) = 0.0d0
  a(1:nrow,1:neqn) = 0.0d0

  do it = 1, nelemn
    ar = area(it) / 3.0d0
      do iquad = 1, nquad

      yq = ym(it,iquad)
      xq = xm(it,iquad)

      if ( isotri(it) == 1 ) then
        call trans (det,etax,etay,it,nelemn,nnodes,node,np,xc,xix,xiy,xq,yc,yq)
        ar = det * area(it) / 3.0d0
      end if

      call uval ( etax, etay, g, indx, isotri, it, nelemn, neqn, &
        nnodes, node, np, un, uny, unx, xc, xix, xiy, xq, yc, yq )

  !  Para cada función base:

      do iq = 1, nnodes

        ip = node(it,iq)
        bb = phi(it,iquad,iq,1)
        bx = phi(it,iquad,iq,2)
        by = phi(it,iquad,iq,3)
        bbl = psi(it,iquad,iq)
        ihor = indx(ip,1)
        iver = indx(ip,2)
        iprs = insc(ip)

        if ( 0 < ihor ) then
          f(ihor) = f(ihor) + ar * bb*(un(1)*unx(1)+un(2)*uny(1))
        end if

        if ( 0 < iver ) then
          f(iver) = f(iver) + ar * bb*(un(1)*unx(2)+un(2)*uny(2))
        end if

  !  Para otra función base

      do iqq = 1, nnodes

          ipp = node(it,iqq)
          bbb = phi(it,iquad,iqq,1)
          bbx = phi(it,iquad,iqq,2)
          bby = phi(it,iquad,iqq,3)
          bbbl = psi(it,iquad,iqq)
          ju = indx(ipp,1)
          jv = indx(ipp,2)
          jp = insc(ipp)

  !  Variable de velocidad horizontal

          if ( 0 < ju ) then

            if ( 0 < ihor ) then
              iuse = ihor-ju+ioff
              a(iuse,ju) = a(iuse,ju)+ar*(visc*(by*bby+bx*bbx)+bb*(bbb*unx(1)+bbx*un(1)+bby*un(2)))
            end if

            if ( 0 < iver ) then
              iuse = iver-ju+ioff
              a(iuse,ju) = a(iuse,ju)+ar*bb*bbb*unx(2)
            end if

            if ( 0 < iprs ) then
              iuse = iprs-ju+ioff
              a(iuse,ju) = a(iuse,ju)+ar*bbx*bbl
            end if

          else if ( ju == itype ) then

            if ( ju == -1 ) then
              ubc = ubdry(1,yc(ipp))
            else if ( ju == -2 ) then
              ubc = ubump(g,indx,ipp,iqq,isotri,it,1,nelemn,neqn,nnodes,node,np,xc,yc)
            end if

            if ( 0 < ihor ) then
              aij = ar*(visc*(by*bby+bx*bbx)+bb*(bbb*unx(1)+bbx*un(1)+bby*un(2)))
              f(ihor) = f(ihor)-ubc*aij
            end if

            if ( 0 < iver ) then
              aij = ar*bb*bbb*unx(2)
              f(iver) = f(iver)-ubc*aij
            end if

            if ( 0 < iprs ) then
              aij = ar*bbx*bbl
              f(iprs) = f(iprs)-ubc*aij
            end if

          end if

  ! Variable de velocidad vertical

          if ( 0 < jv ) then

            if ( 0 < ihor ) then
              iuse = ihor-jv+ioff
              a(iuse,jv) = a(iuse,jv)+ar*bb*bbb*uny(1)
            end if

            if ( 0 < iver ) then
              iuse = iver-jv+ioff
              a(iuse,jv) = a(iuse,jv)+ar*(visc*(by*bby+bx*bbx)+bb*(bbb*uny(2)+bby*un(2)+bbx*un(1)))
            end if

            if ( 0 < iprs ) then
              iuse = iprs-jv+ioff
              a(iuse,jv) = a(iuse,jv)+ar*bby*bbl
            end if

          else if ( jv == itype ) then

            if ( jv == -1 ) then
              ubc = ubdry(2,yc(ipp))
            else if ( jv == -2 ) then
              ubc = ubump(g,indx,ipp,iqq,isotri,it,2,nelemn,neqn,nnodes,node,np,xc,yc)
            end if

            if ( 0 < ihor ) then
              aij = ar*bb*bbb*uny(1)
              f(ihor) = f(ihor)-ubc*aij
            end if

            if ( 0 < iver ) then
              aij = ar*(visc*(by*bby+bx*bbx)+bb*(bbb*uny(2)+bby*un(2)+bbx*un(1)))
              f(iver) = f(iver)-ubc*aij
            end if

            if ( 0 < iprs ) then
              aij = ar * bby * bbl
              f(iprs) = f(iprs) - ubc * aij
            end if

          end if

  !  Variable de presión

          if ( 0 < jp ) then

            if ( 0 < ihor ) then
              iuse = ihor-jp+ioff
              a(iuse,jp) = a(iuse,jp) - ar * bx * bbbl
            end if

            if ( 0 < iver ) then
              iuse = iver-jp+ioff
              a(iuse,jp) = a(iuse,jp) - ar * by * bbbl
            end if
          end if

        end do
      end do
    end do
  end do

  !  La última ecuación se "restablece" para requerir que la última presión sea cero.

  f(neqn) = 0.0d0
  do j = neqn-nlband, neqn-1

      i = neqn-j+ioff
      
      a(i,j) = 0.0d0
  
  end do

  a(nband,neqn) = 1.0d0

  !  Factorizamos la matriz

  call dgbfa ( a, maxrow, neqn, nlband, nlband, ipivot, info )

  if (info /= 0) then
    write (*,*) ' '
    write (*,*) ' linsys - Error!'
    write (*,*) ' dgbfa Informacion',info
    stop
  end if

  !  Resolvemos el sistema lineal 

  job = 0
  call dgbsl ( a, maxrow, neqn, nlband, nlband, ipivot, f, job )

  return
end subroutine linsys


subroutine uval ( etax, etay, g, indx, isotri, it, nelemn, neqn, nnodes,node, np, & 
                un, uny, unx, xc, xix, xiy, xq, yc, yq )
  ! uval : evalua las velocidades en un punto de cuadratura dado y calculo de
  !  las derivadas espaciales de las velocidades.

  integer :: nelemn          ! 2*(nx - 1)*(ny -1) Numero de elementos 
  integer :: neqn            ! Iterador de numero de ecuaciones 
  integer :: nnodes          ! No. nodos por elemento (6 triangulares cuadráticos)
  integer :: np              ! mx*my, numero de nodos  
  integer :: indx(np,2)      ! Contiene para cada iesimo nodo el index de la velocidad u, v (Puede 0)
  integer :: ip              ! xc(ip) : Coordenadas x de los nodos 
  integer :: iq              ! Numero de nodos por elemento 
  integer :: isotri(nelemn)  ! Contiene para cada elemento 1 si es isoparametrico o 0 
  integer :: it                  ! Iterador fancy 
  integer :: iuk                 ! Iterador de 1 a 2 
  integer :: iun                 ! indx(ip,iuk)
  integer :: node(nelemn,nnodes) ! Contiene por cada elemento el ind global de cada elemento del nodo
 
  real(8) :: bb             ! Valor de la funcion base en el triangulo de referencia 
  real(8) :: bx             ! Valor de la derivada de bb respecto a x 
  real(8) :: by             ! Valor de la derivada de bb respecto a y 

  real(8) :: etax           ! Las entradas de la matriz jacobiana deta/dx evaluadas en (xq,yq)
  real(8) :: etay           ! Las entradas de la matriz jacobiana deta/dy evaluadas en (xq,yq)

  real(8) :: g(neqn)        ! Comprovar convergencia     g(1:neqn) = g(1:neqn) - f(1:neqn)
  real(8) :: ubc            ! ubdry(iuk,yc(ip))
  real(8) :: ubdry          ! Funcion : Flujo parabólico de entrada en términos del valor del parámetro
  real(8) :: un(2)          ! un(iuk) = un(iuk)+bb*g(iun)
  real(8) :: unx(2)         ! unx(iuk) = unx(iuk)+bx*g(iun)
  real(8) :: uny(2)         ! uny(iuk) = uny(iuk)+by*g(iun)
  real(8) :: xc(np)         ! las coordenadas X de los nodos.
  real(8) :: xix            ! Las entradas de la matriz jacobiana dxi/dx evaluadas en (xq,yq)
  real(8) :: xiy            ! Las entradas de la matriz jacobiana dxi/dy evaluadas en (xq,yq)
  real(8) :: xq             ! La coordenada x del punto en el que se evaluará el mapeo  
  real(8) :: yc(np)         ! Coordenadas y de los nodos
  real(8) :: yq             ! La coordenada y del punto en el que se evaluará el mapeo  

  un(1:2) = 0.0d0
  unx(1:2) = 0.0d0
  uny(1:2) = 0.0d0

  do iq = 1, nnodes

    if ( isotri(it) == 1 ) then ! Si el elemento es isoparametrico entonces
      ! Evaluamos las funciones de base cuadrática en el triángulo de referencia! 
      call refqbf(xq,yq,iq,bb,bx,by,etax,etay,xix,xiy)
    else
      ! Evaluamos las funciones de base cuadrática.
      call qbf(xq,yq,it,iq,bb,bx,by,nelemn,nnodes,node,np,xc,yc)
    end if
    ! Valor del nodo 
    ip = node(it,iq)

    do iuk = 1, 2
      iun = indx(ip,iuk)
      if ( 0 < iun ) then
        un(iuk) = un(iuk)+bb*g(iun)
        unx(iuk) = unx(iuk)+bx*g(iun)
        uny(iuk) = uny(iuk)+by*g(iun)
      else if ( iun == -1 ) then
        ubc = ubdry(iuk,yc(ip))
        un(iuk) = un(iuk)+bb*ubc
        unx(iuk) = unx(iuk)+bx*ubc
        uny(iuk) = uny(iuk)+by*ubc
      end if
    end do

  end do

  return
end subroutine uval


  ! Estas subrutinas las reescribi del paquete LINPACK de Algebra Lineal 

subroutine dgbfa ( abd, lda, n, ml, mu, ipvt, info )
  ! dgbfa : factoriza una matriz de bandas real por eliminación.
  ! La dgbfa suele llamarse dgbco, pero puede llamarse
  ! directamente con un ahorro de tiempo si no se necesita rcond.
  !
  ! Referencias :
  !
  !    Jack Dongarra, Cleve Moler, Jim Bunch and Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, (Society for Industrial and Applied Mathematics),
  !    3600 University City Science Center,
  ! 
  !    Parametros 
  ! 
  !    abd(lda,n) : a la entrada, la matriz en banda almacenamiento. 
  !    las columnas de la matriz se almacenan en las columnas de abd
  !    y las diagonales de la matriz se almacenan en las filas ml+1 a
  !    2*ml+mu+1 de abd. a la salida, una matriz triangular superior en almacenamiento de banda
  !    y los multiplicadores que se utilizaron para obtenerlo. la factorización
  !    se puede escribir a = l*u donde l es un producto de permutación y unidad inferior
  !    matrices triangulares y u es triangular superior.
  !    lda : la dimensión principal del arreglo abd, se requiere 2*ml + mu + 1 <= lda.
  !    ml, mu, el número de diagonales debajo y encima de la diagonal principal. 
  !    0 <= ml < n, 0 <= mu < n.

  integer :: lda      ! La dimensión principal del arreglo abd, se requiere 2*ml + mu + 1 <= lda.
  integer :: n        ! El orden de la matriz.
  integer :: i        ! Iterador de 1 a i0 
  integer :: i0       ! m + 1 - jz
  integer :: info     ! Flag de error 0, normal value.
  integer :: ipvt(n)  ! Los índices de pivote
  integer :: idamax   ! Encuentra el índice del elemento vectorial de máximo valor absoluto
  integer :: j        ! iterador k+1, ju
  integer :: j0       ! mu + 2
  integer :: j1       ! Guarda de manera iterativa el valor de i1  min ( n, m ) - 1
  integer :: ju       ! min ( max ( ju, mu+ipvt(k) ), n )
  integer :: jz       ! iterador de j0, j1
  integer :: k        ! iterador de 1, n-1
  integer :: l        ! idamax (lm+1, abd(m,k), 1 ) + m - 1
  integer :: lm       ! min ( ml, n-k )
  integer :: m        ! m = ml + mu + 1
  ! El número de diagonales debajo y encima de la diagonal principal. 0 <= ml < n, 0 <= mu < n.
  integer :: ml
  integer :: mm
  integer :: mu
  real(8) :: t          !  {(x1 , y1 ), (x2 , y2 ), (x3 , y3 )} 
  !   A la entrada, la matriz en banda almacenamiento. 
  !   Las columnas de la matriz se almacenan en las columnas de ABD
  !   y las diagonales de la matriz se almacenan en las filas ML+1 a
  real(8) :: abd(lda,n) 

  m = ml + mu + 1
  info = 0

  ! Cero columnas iniciales de relleno

  j0 = mu + 2
  j1 = min ( n, m ) - 1

  do jz = j0, j1
    i0 = m + 1 - jz
    do i = i0, ml
      abd(i,jz) = 0.0d0
    end do
  end do

  jz = j1
  ju = 0

  !  Eliminación gaussiana con pivoteo parcial.

  do k = 1, n-1

  !  Pone a cero la siguiente columna de relleno.

  jz = jz + 1
  if ( jz <= n ) then
      abd(1:ml,jz) = 0.0d0
  end if

  ! Encuentre L = índice de pivote.

    lm = min ( ml, n-k )
    l = idamax (lm+1, abd(m,k), 1 ) + m - 1
    ipvt(k) = l + k - m

  !  El pivote cero implica que esta columna ya está triangularizada.

    if ( abd(l,k) == 0.0d0) then
    info = k

  !  Intercambiar si es necesario.

    else

    if ( l /= m ) then
        t = abd(l,k)
        abd(l,k) = abd(m,k)
        abd(m,k) = t
    end if

  !  Calcular multiplicadores.

      t = -1.0d0 / abd(m,k)
      call dscal(lm, t, abd(m+1,k), 1 )

  ! Eliminación de filas con indexación de columnas.

      ju = min (max( ju, mu+ipvt(k) ), n )
      mm = m

      do j = k+1, ju
        
        l = l - 1
        mm = mm - 1
        t = abd(l,j)
        
        if ( l /= mm ) then
          abd(l,j) = abd(mm,j)
          abd(mm,j) = t
        end if

          ! Vamos a calcular tiempos constantes de un vector más un vector.
          call daxpy ( lm, t, abd(m+1,k), 1, abd(mm+1,j), 1 )
      
      end do

    end if

  end do

  ipvt(n) = n

  !    k, if u(k,k) == 0.0. Esta no es una condición de error para este
  !    subrutina, pero indica que dgbsl dividirá por cero si es llamado. 

  if ( abd(m,n) == 0.0d0 ) then
    info = n
  end if

  return
end subroutine dgbfa 



subroutine dgbsl( abd, lda, n, ml, mu, ipvt, b, job )
  ! 
  ! dgbsl : resuelve un sistema real de bandas factorizado por dgbco o dgbfa.
  ! dgbsl puede resolver a * x = b o a' * x = b.
  ! 
  ! Se producirá una división por cero si el factor de entrada contiene un cero en la diagonal. 
  ! técnicamente esto indica singularidad, pero a menudo es causado por argumentos inadecuados o
  ! Ajuste de lda. 
  ! No ocurrirá si las subrutinas están llamado correctamente y si dgbco ha establecido 
  ! 0.0 < rcond o dgbfa ha puesto info == 0.
  ! 
  ! Para calcular inversa(a) * c donde c es una matriz con p columnas:

  integer :: lda      ! La dimensión principal de la matriz 
  integer :: n        ! El orden de la matriz.
  integer :: ipvt(n)  ! El vector pivote de dgbco o dgbfa.
  integer :: job      ! Consigue un trabajo 
  integer :: k        ! 1, n - 1
  integer :: l        ! ipvt(k)
  integer :: la       ! m - lm
  integer :: lb       ! k - lm
  integer :: lm       ! min ( k, m ) - 1
  integer :: m        ! mu + ml + 1
  integer :: ml       ! El número de diagonales debajo y encima de la diagonal principal. 0 <= ml < n
  integer :: mu       ! El número de diagonales debajo y encima de la diagonal principal. 0 <= mu < n
 
  real(8) :: abd(lda,n) ! La salida de dgbco o dgbfa.
  real(8) :: b(n)       ! n la entrada, el lado derecho. en salida : La solución.
  real(8) :: ddot       ! Forma el producto escalar de dos vectores
  real(8) :: t          ! ddot ( lm, abd(la,k), 1, b(lb), 1 )

  m = mu + ml + 1


  ! Resolvemos A * x = b.
  ! Primero resuelve L * y = b.

  if ( job == 0 ) then
    if ( 0 < ml ) then
      do k = 1, n-1
        lm = min ( ml, n-k )
        l = ipvt(k)
        t = b(l)
          if ( l /= k ) then
          b(l) = b(k)
          b(k) = t
          end if
        call daxpy ( lm, t, abd(m+1,k), 1, b(k+1), 1 )
      end do

    end if

  ! Ahora resuelve U * x = y.

    do k = n, 1, -1
      b(k) = b(k) / abd(m,k)
      lm = min ( k, m ) - 1
      la = m - lm
      lb = k - lm
      t = -b(k)
      call daxpy ( lm, t, abd(la,k), 1, b(lb), 1 )
    end do

  ! Si es distinto de cero, resuelve A' * x = b.
  ! Primero resolvemos U' * y = b.

  else

    do k = 1, n
      lm = min ( k, m ) - 1
      la = m - lm
      lb = k - lm
      t = ddot ( lm, abd(la,k), 1, b(lb), 1 )
      b(k) = ( b(k) - t ) / abd(m,k)
    end do

  ! Ahora resolvemos L' * x = y.

    if ( 0 < ml ) then
      do k = n-1, 1, -1
      lm = min ( ml, n-k )
      b(k) = b(k) + ddot ( lm, abd(m+1,k), 1, b(k+1), 1 )
      l = ipvt(k)
        if ( l /= k ) then
          t = b(l)
          b(l) = b(k)
          b(k) = t
        end if
      end do
    end if
  end if

  return
end subroutine dgbsl 



subroutine daxpy (n, da, dx, incx, dy, incy )
  ! daxpy : calcula tiempos constantes de un vector más un vector 
  ! Utiliza bucles desenrollados para incrementos iguales a uno.
  ! 
  !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
  !    Basic Linear Algebra Subprograms for Fortran Usage,
  !    Algorithm 539
  !

  real(8) :: da     ! El multiplicador de dx
  real(8) :: dx(*)  ! El primer vector
  real(8) :: dy(*)  ! El segundo vector en la salida, dy(*) ha sido reemplazado por dy(*) + da * dx(*).
  
  integer :: i      ! m+1, n, 4
  integer :: incx   ! El incremento entre sucesivos de entradas de dx.
  integer :: incy   ! El incremento entre sucesivos entradas de dy
  integer :: ix     ! ix + incx
  integer :: iy     ! iy + incy
  integer :: m      ! mod ( n, 4 )
  integer :: n      ! El número de elementos en dx y dy

  if ( n <= 0 ) then
    return
  end if

  if ( da  == 0.0d0 ) then
    return
  end if

  ! Código para incrementos desiguales o incrementos iguales no es igual a 1.

  if ( incx /= 1 .or. incy /= 1 ) then

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( - n + 1 ) * incy + 1
    end if

    do i = 1, n
      dy(iy) = dy(iy) + da * dx(ix)
      ix = ix + incx
      iy = iy + incy
    end do

  ! Código para ambos incrementos igual a 1.

  else

    m = mod ( n, 4 )

    do i = 1, m
      dy(i) = dy(i) + da * dx(i)
    end do

    do i = m+1, n, 4
      dy(i)   = dy(i)   + da * dx(i)
      dy(i+1) = dy(i+1) + da * dx(i+1)
      dy(i+2) = dy(i+2) + da * dx(i+2)
      dy(i+3) = dy(i+3) + da * dx(i+3)
    end do

  end if

  return
end subroutine daxpy 



subroutine resid ( area, g, indx, insc, isotri,  nelemn, neqn, nnodes, & 
                  node , np, nquad, phi, psi, res, reynld, xc, xm, yc, ym )
  ! resid : Calcula el residual del valor de g (Sigue siendo la libreria de LINPAK)
  ! 
  ! Referencias :
  !
  !    Jack Dongarra, Cleve Moler, Jim Bunch and Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, (Society for Industrial and Applied Mathematics),
  !    3600 University City Science Center
  ! 
  ! Las variables son las mismas que linsys y nstokes 

  integer :: nelemn
  integer :: neqn
  integer :: nnodes
  integer :: np
  integer :: nquad
  integer :: i
  integer :: ibad
  integer :: ihor
  integer :: imax
  integer :: indx(np,2)
  integer :: insc(np)
  integer :: ip
  integer :: ipp
  integer :: iprs
  integer :: iq
  integer :: iqq
  integer :: iquad
  integer :: isotri(nelemn)
  integer :: it
  integer :: itype
  integer :: iver
  integer :: jp
  integer :: ju
  integer :: jv
  integer :: node(nelemn,nnodes)

  real(8) :: aij
  real(8) :: ar
  real(8) :: area(nelemn)
  real(8) :: bb
  real(8) :: bbb
  real(8) :: bbbl
  real(8) :: bbl
  real(8) :: bbx
  real(8) :: bby
  real(8) :: bx
  real(8) :: by
  real(8) :: det
  real(8) :: etax
  real(8) :: etay
  real(8) :: g(neqn)
  real(8) :: phi(nelemn,nquad,nnodes,3)
  real(8) :: psi(nelemn,nquad,nnodes)
  real(8) :: res(neqn)
  real(8) :: reynld
  real(8) :: rmax
  real(8) :: test
  real(8) :: ubc
  real(8) :: ubdry
  real(8) :: ubump
  real(8) :: un(2)
  real(8) :: unx(2)
  real(8) :: uny(2)
  real(8) :: visc
  real(8) :: xc(np)
  real(8) :: xix
  real(8) :: xiy
  real(8) :: xm(nelemn,nquad)
  real(8) :: xq
  real(8) :: yc(np)
  real(8) :: ym(nelemn,nquad)
  real(8) :: yq

  itype = -1
  visc = 1.0d0 / reynld

  res(1:neqn) = 0.0d0

  do it = 1, nelemn
    ar = area(it) / 3.0d0
      do iquad = 1, nquad
        yq = ym(it,iquad)
        xq = xm(it,iquad)
        if ( isotri(it) == 1 ) then
            call trans(det,etax,etay,it,nelemn,nnodes,node,np,xc,xix,xiy,xq,yc,yq)
            ar = det * area(it) / 3.0d0
        end if

    call uval(etax,etay,g,indx,isotri,it,nelemn,neqn,nnodes,node,np,un,uny,unx,xc,xix,xiy,xq,yc,yq)

  ! Para cada función base:
      do iq = 1, nnodes

        ip = node(it,iq)    
        bb = phi(it,iquad,iq,1)
        bx = phi(it,iquad,iq,2)
        by = phi(it,iquad,iq,3)
        bbl = psi(it,iquad,iq)
        iprs = insc(ip)
        ihor = indx(ip,1)
        iver = indx(ip,2)

        if ( 0 < ihor ) then
          res(ihor) = res(ihor)-ar*bb*(un(1)*unx(1)+un(2)*uny(1))
        end if

        if ( 0 < iver ) then
          res(iver) = res(iver)-ar*bb*(un(1)*unx(2)+un(2)*uny(2))
        end if

  ! Para la otra función base

        do iqq = 1, nnodes
          ipp = node(it,iqq)
          bbb = phi(it,iquad,iqq,1)
          bbx = phi(it,iquad,iqq,2)
          bby = phi(it,iquad,iqq,3)
          bbbl = psi(it,iquad,iqq)
          ju = indx(ipp,1)
          jv = indx(ipp,2)
          jp = insc(ipp)

  ! Variable de velocidad horizontal

      if ( 0 < ju ) then

        if ( 0 < ihor ) then

        res(ihor) = res(ihor)+ar*(visc*(by*bby+bx*bbx) +bb*(bbb*unx(1)+bbx*un(1)+bby*un(2)))*g(ju)
            
        end if

        if ( 0 < iver ) then
       
          res(iver) = res(iver)+ar*bb*bbb*unx(2)*g(ju)
       
        end if

        if ( 0 < iprs ) then
          
          res(iprs) = res(iprs)+ar*bbx*bbl*g(ju)
        
        end if

        
        else if ( ju == itype ) then

          if ( ju == -2 ) then

          ubc = ubump(g,indx,ipp,iqq,isotri,it,1,nelemn, neqn,nnodes,node,np,xc,yc)
          
          else if ( ju == -1 ) then
          
              ubc = ubdry(1,yc(ipp))
          
          end if

            if ( 0 < ihor ) then

              aij = ar*(visc*(by*bby+bx*bbx) +bb*(bbb*unx(1)+bbx*un(1)+bby*un(2)))
            
              res(ihor) = res(ihor)+ubc*aij
            
            end if

            if ( 0 < iver ) then

              aij = ar*bb*bbb*unx(2)
              
              res(iver) = res(iver)+ubc*aij
            
            end if

            if ( 0 < iprs ) then
              aij = ar*bbx*bbl
              res(iprs) = res(iprs)+ubc*aij
            end if

          end if

  ! Variable de velocidad vertical

          if ( 0 < jv ) then

            if ( 0 < ihor ) then
            
              res(ihor) = res(ihor)+ar*bb*bbb*uny(1)*g(jv)
            
            end if

            if ( 0 < iver ) then
            
            res(iver) = res(iver)+ar*(visc*(by*bby+bx*bbx)+bb*(bbb*uny(2)+bby*un(2)+bbx*un(1)))*g(jv)
            
            end if

            if ( 0 < iprs ) then
            
              res(iprs) = res(iprs)+ar*bby*bbl*g(jv)
            
            end if

            
          else if ( jv == itype ) then

            if ( jv == -2 ) then

              ubc = ubump(g,indx,ipp,iqq,isotri,it,2,nelemn,neqn,nnodes,node,np,xc,yc)
            
            else if ( jv == -1 ) then
            
              ubc = ubdry(2,yc(ipp))
            
            end if

            if ( 0 < ihor ) then
            
              aij = ar*bb*bbb*uny(1)
            
              res(ihor) = res(ihor)+ubc*aij
            
            end if

            if ( 0 < iver ) then
              
              aij = ar*(visc*(by*bby+bx*bbx)+bb*(bbb*uny(2)+bby*un(2)+bbx*un(1)))
              
              res(iver) = res(iver)+ubc*aij
            
            end if

            if ( 0 < iprs ) then
             
              aij = ar*bby*bbl
             
              res(iprs) = res(iprs)+ubc*aij
           
             end if
          end if

  ! Variable de presión

          if ( 0 < jp ) then

            if ( 0 < ihor ) then
            
              res(ihor) = res(ihor)-ar*bx*bbbl*g(jp)
            
            end if

            if ( 0 < iver ) then
            
              res(iver) = res(iver)-ar*by*bbbl*g(jp)
            
            end if
          end if
        end do
      end do
    end do
  end do

  ! La última ecuación se "reinicia" para requerir que la última presión ser cero

  res(neqn) = g(neqn)

  rmax = 0.0d0 
  imax = 0
  ibad = 0

  do i = 1, neqn

    test = abs(res(i))

    if ( rmax < test ) then

      rmax = test
      imax = i
    
    end if

    if ( 1.0D-03 < test ) then
      
      ibad = ibad + 1
    
    end if

  end do

  ! Revisar como va la cosa 
  ! Para ver los resultados 
  ! if ( 1 <= iwrite ) then
  !   write(*,*)' '
  !   write(*,*)' Información residual:'
  !   write(*,*)' '
  !   write(*,*)' El peor residual es el número ',imax
  !   write(*,*)' de magnitud ',rmax
  !   write(*,*)' '
  !   write(*,*)' numero de residuos "malos" es',ibad,' fuera de',neqn
  !   write(*,*)' '
  ! end if

  ! if ( 2 <= iwrite ) then
  !   ! write(*,*)'Residuos crudos:'
  !   ! write(*,*)' '
  !   i = 0
  !   do j = 1, np

  !     if ( 0 < indx(j,1) ) then
  !       i = i+1
  !       if ( abs(res(i)) <= 1.0D-03 ) then
  !         write(*,'(1x,a1,2i5,g14.6)')'U',i,j,res(i)
  !       else
  !         write(*,'(a1,a1,2i5,g14.6)')'*','U',i,j,res(i)
  !       end if
  !     end if

  !     if ( 0 < indx(j,2) ) then
  !       i = i+1
  !       if ( abs(res(i)) <= 1.0D-03 ) then
  !         write(*,'(1x,a1,2i5,g14.6)')'V',i,j,res(i)
  !       else
  !         write(*,'(a1,a1,2i5,g14.6)')'*','V',i,j,res(i)
  !       end if
  !     end if

  !     if ( 0 < insc(j) ) then
  !       i = i+1
  !       if ( abs(res(i)) <= 1.0D-03 ) then
  !         write(*,'(1x,a1,2i5,g14.6)')'P',i,j,res(i)
  !       else
  !         write(*,'(a1,a1,2i5,g14.6)')'*','P',i,j,res(i)
  !       end if
  !     end if

  !   end do

  ! end if
  ! return

end subroutine resid


function ddot ( n, dx, incx, dy, incy )
  ! 
  ! ddot : Obtiene el producto escalar de dos vectores.
  !
  ! Esta rutina usa bucles desenrollados para incrementos iguales a uno.
  !
  !  Referencia
  !
  !    Jack Dongarra, Cleve Moler, Jim Bunch and Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, (Society for Industrial and Applied Mathematics),
  !    3600 University City Science Center,
  !    Philadelphia, PA, 19104-2688.
  !    ISBN 0-89871-172-X
  !
  ! Las variables son las mismas que linsys y nstokes 


  real(8) :: ddot   ! Valor de la funcion ddot 
  real(8) :: dtemp  ! Diferenciales dx, dy 
  real(8) :: dx(*)  ! El primer vector 
  real(8) :: dy(*)  ! El segundo vector 

  integer :: i      ! Iterador fancy 
  integer :: incx   ! El incremento entre sucesivos entradas de x 
  integer :: incy   ! El incremento entre sucesivos entradas de y 
  integer :: ix     ! ix + incx
  integer :: iy     ! iy + incy 
  integer :: m      ! mod ( n, 5 )
  integer :: n      ! El número de elementos en dx y Dy

  ddot = 0.0d0
  dtemp = 0.0d0 

  if ( n <= 0 ) then
  
    return
  
  end if

  ! Código para incrementos desiguales o incrementos iguales
  ! (No es igual a 1) 


  if ( incx /= 1 .or. incy /= 1 ) then

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( - n + 1 ) * incy + 1
    end if

    do i = 1, n
      dtemp = dtemp + dx(ix) * dy(iy)
      ix = ix + incx
      iy = iy + incy
    end do

  ! Código para ambos incrementos igual a 1.

  else

    m = mod ( n, 5 )

    do i = 1, m
      dtemp = dtemp + dx(i) * dy(i)
    end do

    do i = m+1, n, 5

      dtemp = dtemp + dx(i  ) * dy(i  ) &
                    + dx(i+1) * dy(i+1) &
                    + dx(i+2) * dy(i+2) &
                    + dx(i+3) * dy(i+3) &
                    + dx(i+4) * dy(i+4)
    end do

  end if

  ddot = dtemp

  return
end function ddot




subroutine dscal ( n, sa, x, incx )
  ! 
  ! dscal : Escala un vector por una constante.
  !  Referencias 
  !
  !    Jack Dongarra, Cleve Moler, Jim Bunch and Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, (Society for Industrial and Applied Mathematics),
  !    3600 University City Science Center,
  !    Philadelphia, PA, 19104-2688.
  !    ISBN 0-89871-172-X
  !
  !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
  !    Basic Linear Algebra Subprograms for Fortran Usage,
  !    Algorithm 539,
  !    ACM Transactions on Mathematical Software,
  !    Volume 5, Number 3, September 1979, pages 308-323.
  !
  ! implicit none
  ! Las variables son las mismas que linsys y nstokes 


  integer :: i
  integer :: incx
  integer :: ix
  integer :: m
  integer :: n

  real(8) :: sa
  real(8) :: x(*)

  if ( n <= 0 ) then

  else if ( incx == 1 ) then

    m = mod ( n, 5 )

    x(1:m) = sa * x(1:m)

    do i = m+1, n, 5
      x(i)   = sa * x(i)
      x(i+1) = sa * x(i+1)
      x(i+2) = sa * x(i+2)
      x(i+3) = sa * x(i+3)
      x(i+4) = sa * x(i+4)
    end do

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( - n + 1 ) * incx + 1
    end if

    do i = 1, n
      x(ix) = sa * x(ix)
      ix = ix + incx
    end do

  end if

  return
end subroutine dscal



subroutine getg ( f, iline, my, neqn, u )
  ! 
  ! getg : extrae los valores de una cantidad a lo largo de la línea del perfil.
  ! 
  ! Referencias 
  !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
  !    Basic Linear Algebra Subprograms for Fortran Usage,
  !    Algorithm 539,
  !    ACM Transactions on Mathematical Software,
  !    Volume 5, Number 3, September 1979, pages 308-323
  !  implicit none
  ! Las variables son las mismas que linsys y nstokes 


  integer :: my
  integer :: neqn
  integer :: i
  integer :: iline(my)
  integer :: j

  real(8) :: f(neqn)
  real(8) :: u(my)

  do i = 1, my
    u(i) = 0.0d0 
    j = iline(i)
    if ( j <= 0 ) then
      u(i) = 0.0d0 
    else
      u(i) = f(j)
    end if
  end do

  return
end subroutine getg





subroutine gram ( gr, iline, indx,  my, nelemn, nnodes, node, &
  np, r, uprof, xc, xprof, yc )
  !
  ! gram  : calcula y almacena la matriz gram.
  ! 
  ! La rutina calcula la matriz de gram y el vector
  ! cuyas componentes son la integral de línea de ui*phi(j).
  ! se utiliza una regla de cuadratura de Gauss de tres puntos para evaluar la integral de línea.
  ! 
  !    output, gr(my,my) : La matriz de gram.
  !
  !    iline(my) : La lista de números desconocidos globales a lo largo de la línea del perfil.
  !
  !    indx(np,2 ) :  Para cada nodo i, el índice de las velocidades u y v en ese nodo, o 0.
  !
  !    iwrite : Controla la cantidad de salida impresa.
  ! 
  !    output, r(my) : La integral de línea de uprof * phi.
  !
  !    uprof(my) : La velocidad horizontal a lo largo de la línea del perfil. 
  !
  !  implicit none
  ! Las demas variables son las mismas que linsys y nstokes 

  integer :: my
  integer :: nelemn
  integer :: nnodes
  integer :: np
  integer :: i
  integer :: igetl
  integer :: ii
  integer :: iline(my)
  integer :: indx(np,2)
  integer :: ip
  integer :: ipp
  integer :: iq
  integer :: iqq
  integer :: iquad
  integer :: it
  integer :: iun
  integer :: j
  integer :: jj
  integer :: k
  integer :: kk
  integer :: node(nelemn,nnodes)

  real(8) :: ar
  real(8) :: bb
  real(8) :: bbb
  real(8) :: bbx
  real(8) :: bby
  real(8) :: bma2
  real(8) :: bx
  real(8) :: by
  real(8) :: gr(my,my)
  real(8) :: r(my)
  real(8) :: ubc
  real(8) :: ubdry
  real(8) :: uiqdpt
  real(8) :: uprof(my)
  real(8) :: wt(3)
  real(8) :: x
  real(8) :: xc(np)
  real(8) :: xprof
  real(8) :: y
  real(8) :: yc(np)
  real(8) :: yq(3)
  !
  ! Valores para cuadratura de Gauss de 3 puntos.
  !
  wt(1) = 5.0d0 / 9.0d0
  wt(2) = 8.0d0 / 9.0d0
  wt(3) = 5.0d0 / 9.0d0

  yq(1) = -0.7745966692d0
  yq(2) =  0.0d0
  yq(3) =  0.7745966692d0

  r(1:my) = 0.0d0
  gr(1:my,1:my) = 0.0d0

  !
  ! Calcula la integral de línea recorriendo intervalos a lo largo de la línea
  ! utilizando la cuadratura de Gauss de tres puntos
  !

  do it = 1, nelemn
  !
  ! Comprobar para ver si estamos en un triángulo con un lado a lo largo de la línea
  ! x = xprof
  !
      k = node(it,1)
      kk = node(it,2)

      if ( 1.0D-04 < abs ( xc(k) - xprof ) .or. &
         1.0D-04 < abs ( xc(kk) - xprof ) ) then
        go to 70
      end if

      do iquad = 1, 3

        bma2 = ( yc(kk) - yc(k) ) / 2.0d0
        ar = bma2 * wt(iquad)
        x = xprof
        y = yc(k) + bma2 * ( yq(iquad) + 1.0d0)
  !
  ! Calcule U interna en puntos de cuadratura
  !
        uiqdpt = 0.0d0

        do iq = 1, nnodes
          if ( iq == 1 .or. iq == 2 .or. iq == 4 ) then
            call qbf ( x, y, it, iq, bb, bx, by, nelemn, nnodes, &
              node, np, xc, yc )
            ip = node(it,iq)
            iun = indx(ip,1)
            if ( 0 < iun ) then
              ii = igetl(iun,iline,my)
              uiqdpt = uiqdpt + bb * uprof(ii)
            else if ( iun == -1) then
              ubc = ubdry(1,yc(ip))
              uiqdpt = uiqdpt + bb * ubc
            end if
          end if
        end do
  !
  ! Solo pase por encima de los nodos que se encuentran en la línea x = xprof
  !
        do iq = 1, nnodes
          if ( iq == 1 .or. iq == 2 .or. iq == 4 ) then
            ip = node(it,iq)
            call qbf(x,y,it,iq,bb,bx,by,nelemn,nnodes,node,np,xc,yc)
            i = indx(ip,1)
            if ( 0 < i ) then
              ii = igetl(i,iline,my)
              r(ii) = r(ii) + bb * uiqdpt * ar
              do iqq = 1, nnodes
                if ( iqq == 1 .or. iqq == 2 .or. iqq == 4 ) then
                  ipp = node(it,iqq)
                  call qbf(x,y,it,iqq,bbb,bbx,bby,nelemn,nnodes,node,np,xc,yc)
                  j = indx(ipp,1)
                  if ( j /= 0 ) then
                    jj = igetl(j,iline,my)
                    gr(ii,jj) = gr(ii,jj)+bb*bbb*ar
                  end if
                end if
              end do
            end if
          end if
        end do

      end do

 70     continue

  end do

  ! Para ver como va la cosa 
  ! if ( 3 <= iwrite ) then
  !   write(*,*)' '
  !   write(*,*)' Matriz de gramos:'
  !   do i = 1, my
  !     do j = 1, my
  !       write(*,*)i,j,gr(i,j)
  !     end do
  !   end do
  !   write(*,*)' '
  !   write(*,*)'Vector R:'
  !   do i = 1, my
  !     write(*,*)r(i)
  !   end do

  ! end if

  return
end subroutine gram








function idamax ( n, dx, incx )
  ! 
  ! idamax : encuentra el índice del elemento vectorial de máximo valor absoluto.!
  !  Referencias:
  !
  !    Jack Dongarra, Cleve Moler, Jim Bunch, Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, (Society for Industrial and Applied Mathematics),
  !    3600 University City Science Center,
  !    Philadelphia, PA, 19104-2688.
  !    ISBN 0-89871-172-X
  !
  !    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
  !    Basic Linear Algebra Subprograms for Fortran Usage,
  !    Algorithm 539,
  !    ACM Transactions on Mathematical Software,
  !    Volume 5, Number 3, September 1979, pages 308-323.
  !
  !  Parametros:
  !
  !    n : El número de entradas en el vector.
  !
  !    x(*) : El vector a examinar.
  !
  !    incx : El incremento entre sucesivos entries of sx.
  !
  !    idamax : El índice del elemento de sx de valor absoluto máximo.
  ! 
  !  implicit none
  ! Las demas variables son las mismas que linsys y nstokes 

  real(8) :: dmax
  real(8) :: dx(*)

  integer :: i
  integer :: idamax
  integer :: incx
  integer :: ix
  integer :: n

  idamax = 0

  if ( n < 1 .or. incx <= 0 ) then
    return
  end if

  idamax = 1

  if ( n == 1 ) then
    return
  end if

  if ( incx == 1 ) then

    dmax = abs ( dx(1) )

    do i = 2, n
      if ( dmax < abs ( dx(i) ) ) then
        idamax = i
        dmax = abs ( dx(i) )
      end if
    end do

  else

    ix = 1
    dmax = abs ( dx(1) )
    ix = ix + incx

    do i = 2, n
      if ( dmax < abs ( dx(ix) ) ) then
        idamax = i
        dmax = abs ( dx(ix) )
      end if
      ix = ix + incx
    end do

  end if

  return
end function idamax





function igetl ( i, iline, my )
  !
  ! igetl : obtiene el número local desconocido a lo largo de la línea del perfil.
  ! para nuestro problema, la línea de perfil se especifica mediante x = xprof.
  ! nos dan un número global desconocido y necesitamos determinar el
  ! número local desconocido.
  ! 
  !    i : el número desconocido global.
  !
  !    iline(my) : la lista de números desconocidos globales a lo largo de la línea del perfil.
  !
  !    my : el número de nodos en la línea de perfil.
  !
  !    igetl : el índice del nodo en el perfil línea en la que aparece i global desconocido, o 
  !    -1 si no existe tal entrada.
  !
  !  implicit none
  ! Las demas variables son las mismas que linsys y nstokes 

  integer :: my
  integer :: i
  integer :: igetl
  integer :: iline(my)
  integer :: j

  igetl = -1

  do j = 1, my
    if ( iline(j) == i ) then
      igetl = j
      return
    end if
  end do

  return
end function igetl


subroutine pval ( g, insc, long, mx, my, nelemn, neqn, nnodes, node, np, press )
  ! 
  ! pval : Lee las presiones en cada punto y despues las interpola en cada punto par, impar 
  ! 
  !  implicit none
  ! Las demas variables son las mismas que linsys y nstokes 

  integer :: mx
  integer :: my
  integer :: nelemn
  integer :: neqn
  integer :: nnodes
  integer :: np
  integer :: i
  integer :: insc(np)
  integer :: ip
  integer :: iq
  integer :: it
  integer :: ivar
  integer :: j
  integer :: node(nelemn,nnodes)

  real(8) :: g(neqn)
  real(8) :: press(mx,my)

  logical :: long

  press(1:mx,1:my) = 0.0d0

  !
  !  Lectura de las presiones 
  ! 

  do it = 1, nelemn
    do iq = 1, 3
      ip = node(it,iq)
      ivar = insc(ip)
      if ( long ) then
        i = ((ip-1)/my)+1
        j = mod(ip-1,my)+1
      else
        i = mod(ip-1,mx)+1
        j = ((ip-1)/mx)+1
      end if

      if ( 0 < ivar ) then
        press(i,j) = g(ivar)
      end if

    end do
  end do

  !
  ! Interpolar las presiones en los puntos (par, impar) e (par, impar).
  !

  do i = 2, mx-1, 2
    do j = 1, my, 2
      press(i,j) = 0.5D+00 * (press(i-1,j)+press(i+1,j))
    end do
  end do

  do j = 2, my-1, 2
    do i = 1, mx, 2
      press(i,j) = 0.5D+00 * (press(i,j-1)+press(i,j+1))
    end do
  end do

  !
  !  Interpola las presiones en los puntos (pares, pares)
  !

  do j = 2, my-1, 2
    do i = 2, mx-1, 2
      press(i,j) = 0.5D+00 * (press(i-1,j-1)+press(i+1,j+1))
    end do
  end do

  return
end subroutine pval





function ubdry ( iuk, yy )
  !
  ! ubdry : establece el flujo parabólico de entrada en términos del valor del parámetro
  !
  !  implicit none

  integer :: iuk
  !
  !    iuk : El índice de lo desconocido.
  !    1, la velocidad horizontal.
  !    2, la velocidad vertical
  !
  real(8) :: ubdry ! El valor del límite prescrito componente de flujo en esta coordenada en el límite.
  real(8) :: yy    !  La coordenada y del punto límite.

  if ( iuk == 1 ) then
    !ubdry = ( -2.0d0* yy + 6.0d0) * yy / 9.0d0 !cambio
    ubdry = (0.5d0 - (0.5d0/9.d0)*yy**2)
  else
    ubdry = 0.0d0
  end if

  return
end function ubdry 




function ubump ( g, indx, ip, iqq, isotri, it, iukk, nelemn, neqn, nnodes, node, np, xc, yc )
  ! 
  ! ubump : Calcula la sensibilidad dU/dA en la protuberancia.
  !
  !    Esta rutina establece
  !
  !      dU/dA = -uy * phi
  !      dV/dA = -vy * phi
  !
  !   Donde phi es la forma de la protuberancia.
  !
  !  Parametros:
  !
  !   indx(maxnp,2) : Contiene, para cada nodo i, el índice de las velocidades u y v en ese nodo, o 0.
  !
  !   implicit none
  !   Las demas variables son las mismas que linsys y nstokes 

  integer :: nelemn
  integer :: neqn
  integer :: nnodes
  integer :: np
  integer :: indx(np,2)
  integer :: ip
  integer :: iqq
  integer :: isotri(nelemn)
  integer :: it
  integer :: iukk
  integer :: node(nelemn,nnodes)

  real(8) :: ubump
  real(8) :: un(2)
  real(8) :: unx(2)
  real(8) :: uny(2)
  real(8) :: xc(np)
  real(8) :: xix
  real(8) :: xiy
  real(8) :: xq
  real(8) :: yc(np)
  real(8) :: yq
  real(8) :: det
  real(8) :: etax
  real(8) :: etay
  real(8) :: g(neqn)


  if ( isotri(it) == 0 ) then
    xq = xc(ip)
    yq = yc(ip)
  else
    if ( iqq == 1 ) then
      xq = 0.0d0
      yq = 0.0d0
    else if ( iqq == 2 ) then
      xq = 1.0d0
      yq = 1.0d0
    else if ( iqq == 3 ) then
      xq = 1.0d0
      yq = 0.0d0
    else if ( iqq == 4 ) then
      xq = 0.5d0
      yq = 0.5d0
    else if ( iqq == 5 ) then
      xq = 1.0d0
      yq = 0.5d0
    else if ( iqq == 6 ) then
      xq = 0.5d0
      yq = 0.0d0
    end if
    call trans(det,etax,etay,it,nelemn,nnodes,node,np,xc,xix,xiy,xq,yc,yq)
  end if

  !
  !  Calcula el valor de uy y vy (soluciones antiguas) en el nodo
  !

  call uval(etax,etay,g,indx,isotri,it,nelemn,neqn,nnodes,node,np,un,uny,unx,xc,xix,xiy,xq,yc,yq)

  if ( iukk == 1 ) then
    ubump = uny(1) * (xc(ip)-1.0d0) * (xc(ip)-3.0d0)
  else if ( iukk == 2 ) then
    ubump = uny(2) * (xc(ip)-1.0d0) * (xc(ip)-3.0d0)
  else
  !   write(*,*)'UBUMP iukk : ',iukk
    stop
  end if

  return
end function ubump






! ---------------------- Escritura de archivo de datos  ----------------------------- ! 





subroutine file_name_inc ( file_name )
  ! 
  ! file_name_inc : Itera de mayor a menor le archivo de escritura 
  ! Este lo reescribi de Python la verdad :) 
  ! 
  !  Ejemplo:
  !
  !      Input            Output
  !      -----            ------
  !      'a7to11.txt'     'a7to12.txt'
  !      'a7to99.txt'     'a8to00.txt'
  !      'a9to99.txt'     'a0to00.txt'
  !      'cat.txt'        ' '
  !      ' '              STOP!
  !

  character :: c
  integer :: change
  integer :: digit
  integer :: i
  integer :: lens
  character ( len = * ) file_name
  
  lens = len_trim ( file_name )
  if ( lens <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'Error!'
    stop
  end if
  change = 0
  do i = lens, 1, -1
    c = file_name(i:i)
    if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then
      change = change + 1
      digit = ichar ( c ) - 48
      digit = digit + 1
      if ( digit == 10 ) then
        digit = 0
      end if
      c = char ( digit + 48 )
      file_name(i:i) = c
      if ( c /= '0' ) then
        return
      end if
    end if
  end do

  if ( change == 0 ) then
    file_name = ' '
    return
  end if

  return
end subroutine file_name_inc



subroutine time_write ( f, indx, neqn, np, yc, xc )
  !
  ! time_write : Escribe en nuestro archivo de datos los valores de los nodos en las coordenadas x,y 
  ! Tambien los coeficientes de las velocidades u,v 
  ! 

  integer :: neqn
  integer :: np
  integer :: ip
  integer :: indx(np,2)
  integer :: k

  real(8) :: f(neqn)
  real(8) :: ubdry

  real(8) :: u      ! Coeficientes de la velocidad horizontal U
  real(8) :: v      ! Coeficientes de la velocidad vertical V 
  real(8) :: yc(np) ! Coordenadas y de los nodos 
  real(8) :: xc(np) ! Coordenadas x de los nodos 



  do ip = 1, np

    k = indx(ip,1)

    if ( k < 0 ) then
      u = ubdry ( 1, yc(ip) )
    else if ( k == 0 ) then
      u = 0.0d0
    else
      u = f(k)
    end if

    k = indx(ip,2)

    if ( k < 0 ) then
      v = ubdry ( 2, yc(ip) )
    else if ( k == 0 ) then
      v = 0.0d0
    else
      v = f(k)
    end if


    write ( 33, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) xc(ip), yc(ip), u, v 
    write ( 33, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) xc(ip), -yc(ip), u, -v 
    write ( 34, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) xc(ip), yc(ip), sqrt(u**2.+v**2.)  !cambio
    write ( 34, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) xc(ip), -yc(ip), sqrt(u**2.+v**2.)  !cambio 


  end do
  return

end subroutine time_write




