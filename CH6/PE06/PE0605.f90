program PE0605
implicit none

integer :: n 

do n = 0,6
    print *,"The size of A",n," is ",100.0*2**(0.25-n/2.0),"×", &
            100.0*2**(-0.25-n/2.0)," centimetres, &
            &and ",100.0*2**(0.25-n/2.0)/2.54,"×", &
            100.0*2**(-0.25-n/2.0)/2.54," inches."
enddo

end program PE0605