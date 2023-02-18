program fact 

    integer :: n , j, a(0:20000) = 0, len , c , num 

    n = 5000 
    j = 2 
    a(0) = 1 
    len  = 1
    c = 0 
    num = 0 


    do while (j <= n)

                    c = 0 
                    num = 0 
                
        
                    do while (c < len)

                        a(c) = a(c)*j
                        a(c) = a(c) + num 

                        num = a(c)/10
                        a(c) = mod(a(c),10)
                        c = c + 1 

                        
                    end do 


                            do while (num /=  0)
                                a(len) = mod(num,10)
                                num = num/10
                                len = len + 1 
                            end do 
                            
                        j = j + 1 
                    
    end do 

    
    len =  len  - 1 



    do while (len >= 0)

        write(*,'(1x,i0)',advance='no') a(len)

        len = len - 1

    end do 



end program fact 