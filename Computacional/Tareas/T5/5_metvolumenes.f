        program solidos
        implicit none
        Real*8 a,b,h,i2,sum,ar,xk,f,aran,cilindro,cil
        Integer n,k
        Print*, '               Metodo de Simpson                   '
        print*,'----------------------------------------------------'
        Print*, 'Numero de particiones (par)'
        Read*, n
        Print*, 'Ingrese el intervalo de integracion'
        Read*, a,b

        print*,'Arandelas******************'
        H= abs(b-a)/real(n)
        I2= (h/3.0)*(f(a)+f(b))
        Sum= 0
        Do k=1,n-1
         Xk= a + k*h
         If (mod(k,2).eq.0) then
          Sum= sum + 2.0*f(xk) !terminos pares
         Else
          Sum= sum + 4.0*f(xk) !terminos impares
         End if
        End do
        Ar = 0
        Ar = I2 + sum*(h/3.0)
        Print*, 'I=',Ar
        aran=4*atan(1.d0)*Ar
        print*, 'El volumen del solido de revolucion es:', aran
        
        print*,'Cilindros******************'
        H= abs(b-a)/real(n)
        I2= (h/3.0)*(cil(a)+cil(b))
        Sum= 0
        Do k=1,n-1
         Xk= a + k*h
         If (mod(k,2).eq.0) then
          Sum= sum + 2.0*cil(xk) !terminos pares
         Else
          Sum= sum + 4.0*cil(xk) !terminos impares
         End if
        End do
        Ar = 0
        Ar = I2 + sum*(h/3.0)
        Print*, 'I=',Ar
        cilindro=8*atan(1.d0)*Ar
        print*, 'El volumen del solido de revolucion es:', cilindro
       stop
       end program
       
       function w(x)
       real*8 w,x
       w=(1/x)
       return
       end
       
       function g(x)
       real*8 x,g
       g=0.5
       return
       end
       
       function f(x) !Arandelas
       real*8 w,x,g,f
       f=(w(x))**2-(g(x))**2
       
       return
       end
       
       
       function cil(x)!Cilindros
       real*8 cil,x,w,g
       
       cil=w(x)*g(x)
       return
       end
