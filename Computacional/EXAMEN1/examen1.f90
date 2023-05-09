
PROGRAM EXAMEN1
    real*4 :: xi, xf
    write(*,*)'Y inicial: ';read*,y1
    write(*,*)'Y final: ';read*,y2
    CALL roots(xi,y1)
    call roots(xf,y2)
    call secant(istep,xi,y1)
    call secant(lstep,xf,y2)
    call romberg(xi,xf,10)
END PROGRAM



! DERIVADA *****************************************************************

function f(x)
    implicit none
    real*4 :: x, f
    f = 0.2*(3.0*x)**(1.5)
    return
end

function yinicial(x,y)
    real :: yinicial, X, y
    yinicial = f(x) - y
    return
end


function integrando(x)
    real :: integrando,der
    integrando = sqrt(1.0+(der(x))**2)
    return
end

function der(x)
    real:: f, X, h = 1.0e-4
    der = (f(x+h)-f(x-h))/(2*h)
    return
end


!MÉTODO DE LA SECANTE ! **************************************************************
subroutine secant(istep,raiz,y)
    real, intent(out) ::  raiz
    integer, intent(out) :: istep
    real :: a, b, dx
    dx = 0.1
    a = raiz-dx
    b = raiz+dx
    x0 = (a + b)/2.0 
    istep = 0 
    dl = 1.0-5
    x1 = x0 + dx 
     
    do while (abs(dx).gt.dl)
        d  = yinicial(x1,y) - yinicial(x0,y)
        x2 = x1 - yinicial(x1,y)*(x1-x0)/d
        x0 = x1
        x1 = x2
        dx = x1 - x0
        istep = istep + 1   
        end do
        raiz = x0

    print*, 'Raiz por secante' 
    print*, raiz
    return
    end subroutine



! ROMBERG *************************************************************
    subroutine romberg(a,b,n)

        real, intent(in) :: a , b 
        integer, intent(in) :: n 
        real::r(20,20)
        real::integrando
    
        write(*,*) ' Integracion por Romberg ' 
    
        h = b - a
        r(1,1) = (h/2.0)*(integrando(a)+integrando(b)) !primer termino
        do i = 2 , n
        s = 0
    
        do k = 1,2**(i-2)
            s = s + integrando(a + ( ( k-0.5)*h))
        end do
    
        r(i,1) = 0.5*(r(i-1,1) + (h*s)) !r(2,1) y demas
    
        h = h / 2.0
        
        end do
    
        ! Extrapolacion de richardson
              
            do j = 2, n
                do i=j,n
                    r(i,j)=r(i,j-1)+((r(i,j-1)-r(i-1,j-1))/(4**(j-1))-1)
                end do
            end do
    
            do i = 1, n
            write(*,20)i, (r(i,1))
        20  format(6(3x,i3, ' | ',f12.6))
            end do
    
        return
            
        end subroutine
    

! ENCONTRAR INTERVALO *********************************************************

subroutine roots(xi,y)
    real, allocatable :: raices(:)
    integer :: nraiz = 0 , k  , i 
    real :: a , delta  = 0.001
    real, intent(out) :: xi
    real :: dx = 0.1
    a0 = 0.0
    b = 100.0 
    a = a0 

        do while(a <= b)  
            if (yinicial(a,y)*yinicial(a + delta,y) < 0.0) nraiz = nraiz + 1
            a = a + delta 
        end do

        allocate(raices(nraiz))

        a = a0 
        k = 0 

        do while(a <= b) 
            if (yinicial(a,y)*yinicial(a + delta,y) < 0.0) then 
            k = k + 1 
            raices(k) = a
            end if 
            a = a + delta 
        end do

        do i = 1, k 
        write(*, *)'Raiz se encuentra en el intervalo : [', raices(i)-dx,',', raices(i)+dx,'].'
        xi = raices(i)
        end do 
        return

        
end subroutine  roots 