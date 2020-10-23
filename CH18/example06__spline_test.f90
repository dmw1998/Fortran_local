real function f(x)
    implicit none
    real, intent(in) :: x
    f = exp(-0.5*x*x)
end function f 

program spline_test
use spline
implicit none

! This program tests the subroutine cubic_spline

! Maximum coefficient for data points
integer, parameter :: n = 17
! Defining external function
real, external :: f 

! Local variables
integer :: error,i,j
real, dimension(0:n) :: x =                         &
    (/ -2.95, -2.6, -2.1, -1.8, -1.4, -1.0, -0.75,  &
       -0.3, -0.05, 0.2, 0.55, 0.9, 1.25, 1.6, 1.7, &
       2.1, 2.4, 3.0 /)
real, dimension(0:n) :: y
real, dimension(0:n-1) :: a,b,c,d 
real :: z,zj,yz

! Calculate y-coordinates corresponding to data values of x
call cubic_spline(x,y,a,b,c,d,error)
if (error /= 0) then
    print *, 'Error', error
    stop
end if

! Now compare interpolated values with true ones, using
! an evenly spaced set of values between -2.8 and +2.8
print '(9X,"x   exp(-0.5x**2)   Spline value"/)'
do i = 0,14
    ! Calculate z (the value to be used)
    z = -2.8 + 0.4*i 
    ! Find in which interval z lies
    do j = 0,n-1
        if (x(j) <= z .and. z <= x(j+1)) exit
    end do

    ! Calculate s(z) for x(j) <= z <= x(j+1)
    zj = z-x(j)
    yz = ((a(j)*zj + b(j))*zj + c(j))*zj + d(j)

    ! Print comparative results
    print '(6X,F6.2,2E15.6)', z,f(z),yz
end do

end program spline_test