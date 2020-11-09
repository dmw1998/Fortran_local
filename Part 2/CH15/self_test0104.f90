program test_0104
implicit none

real :: x = 8E7
integer :: i 

do i = 1,5
    print '(F12.3,E12.3,EN12.3,ES12.3,G12.3)', x,x,x,x,x
    x = x/5000.0
end do

end program test_0104