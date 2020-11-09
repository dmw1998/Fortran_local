program PE1501b
implicit none

real :: x, y
integer :: i 

open(2, file = 'PE1501.txt', status = 'old')

do i = 0,40
    read(2,*) x, y
    print '(F4.1,5X,E16.6)', x, y
end do

close(2)

end program PE1501b