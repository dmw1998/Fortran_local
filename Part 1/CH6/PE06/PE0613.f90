function taylor_sin(x)
implicit none

real :: taylor_sin
real, intent(in) :: x
integer :: n

do n = 1,7
    taylor_sin = taylor_sin + (-1.0)**(n-1) * x**(2.0*n-1.0) / gamma(2.0*n)
end do

end function taylor_sin

program PE0613
implicit none

real, external :: taylor_sin
real, parameter :: pi = 3.141592563589 
integer :: i 
real :: x

do i = 0,90
    x = i/180.0 * pi
    print *,"sin(",i,") = ",taylor_sin(x)
end do

end program PE0613