program PE1501
implicit none

real :: x, y
integer :: i 

open(1, file = 'PE1501.txt', status = 'new')

do i = 0,40
    x = 0.5*i;
    y = exp(x)*sin(x)
    write (1,*) x, y
end do

close(1)

open(2, file = 'PE1501.txt', status = 'old')

do i = 0,40
    read(2,*) x, y
    print *, x, y
end do

close(2)

end program PE1501