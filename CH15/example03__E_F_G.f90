program e_f_and_g_editing
implicit none

real :: x = 123456.0
integer :: i 

do i = 1,7
    print '(F12.5,3X,E12.5,3X,G12.5)', x, x, x, -x, -x, -x 
    x = x/100.0
end do

end program e_f_and_g_editing