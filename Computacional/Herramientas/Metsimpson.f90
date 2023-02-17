       program SimpsonIntegral
       
       integer n, j
       real x2, xi, xf, h, A1, sum, F, I
       
       xi = -1.0  ! Limite inferior
       xf = 1.0  ! Limite superior
       n = 100   ! Numero de intervalos debe ser par
       h = 1.0*ABS(xf-xi)/n   ! Longitud del intervalo
       A1 = (h/3.0)*(f(xi)+f(xf)) ! Evaluada en los extremos
       I = 0
       sum = 0.0
       
       do j=1, n-1
       x2 = xi + j*h
       if (mod(j,2).eq.1) then
       sum = sum + 4.0*f(x2)   ! Numero es impar
       else
       sum = sum + 2.0*f(x2)   ! Numero es par
       end if
       I = h/3.0*sum + A1
       end do
       
       write(*,*) "El area de la integral es: ", I
       stop
       end

       function f(x)
       !f = ((x**3) + (4*(x**2)) - 10)  xi = 3,  xf = 5
       !f = ((2+cos(1+x**1.5))/sqrt(1+0.5*sin(x)))*exp(0.5*x)  x1 =0, xf=2
       !f = sqrt(16 - x**2) xi=-4. xf=4
       !f = x/2 + 1  xi=2, xi=6
       !f = log(x)**3  xi=2, xf=4
       !f = 1/(1+x)  x1=0, xf=1
       !f = exp(-x**2) !xi=0, xf=1
       !f = (5*x + 4)/(x**2 + 3*x -10) !x1=3, xf=5
       !f = 1/sqrt(5 - 7*x**2)
       
       ! Integrales dobles
       !f1 = x**2 - 2
       !f2 = x**4 - 2*x - 2 ! -1,1
       
       f1 = sqrt(x) - 1
       f2 = (x**2)/3 - 0.75
       f = f1-f2
       return
       end
