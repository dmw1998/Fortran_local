program PE0702
implicit none

real,parameter :: pi=3.141592653589
real,dimension(-15:15) :: phi 
real :: x
integer :: i,j

do i=-30,30,2
    x = 0.1*i 
    phi(i/2) = (2.0*pi)**(-0.5)*exp(-x**2.0/2.0)
end do

do j = -15,14,5
    print *, phi(j:j+4)
end do
print *, phi(15)

end program PE0702