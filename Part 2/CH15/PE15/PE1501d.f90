program PE1501d
implicit none

real :: x, y
integer :: i 

open(2, file = 'PE1501.txt', status = 'old')

do i = 0,40
    read(2,*) x, y
    print '(F4.1,5X,ES14.6)', x, y
end do

close(2)

end program PE1501d