program tabular_output
implicit none

real,parameter :: third=1.0/3.0
real :: x
integer :: i 
do i=1,10
    x=i 
    print '(F15.4,F15.4,F15.4)', x,sqrt(x),x**third
end do

end program tabular_output