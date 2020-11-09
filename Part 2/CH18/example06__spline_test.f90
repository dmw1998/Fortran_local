module tridiagonal_systems
    implicit none
    private
    public :: tri_solve
    
contains

    subroutine tri_solve(a,d,c,b,error)
        ! This subroutine solves a diagonally dominant tridiagonal
        ! ystem by Gaussian elimination and back subroutitution

        ! Dummy arguments
        ! Array a holds the arrray of diagonal coefficients
        ! c is the array of above-diagonal coefficients
        ! b is the array of right-hand-side coefficients
        ! and will contain the solution on exit
        real, dimension(:), intent(in) :: a,c 
        real, dimension(:), intent(out) :: d,b 
        integer, intent(out) :: error
        
        call tri_gauss(a,d,c,b,error)

        if (error == 0) call back_tri_substitution(d,c,b)

    end subroutine tri_solve

    subroutine tri_gauss(a,d,c,b,error)
        implicit none

        ! This subroutine perform Gaussian elimination with no
        ! pivoting on a tridiagonal, diagonally dominant, system
        ! of linear equations

        ! Dummy arguments
        real, dimension(:), intent(in) :: a,c 
        real, dimension(:), intent(inout) :: d,b
        integer, intent(out) :: error

        ! Local variables
        real :: m 
        integer :: n,i 

        ! Validity checks
        n = size(a)
        if (n <= 0) then
            ! There is no problem to solve
            error = -1
            return
        end if
        if (n /= size(d) .or. &
            n /= size(c) .or. &
            n /= size(d) ) then
            ! The arrays of coefficients do not have the same size
            error = -2
            return
        end if

        ! Calculate new coefficients of upper diagonal system
        do i = 1,n-1
            m = a(i+1)/d(i)
            d(i+1) = d(i+1) - m*c(i)
            b(i+1) = b(i+1) - m*b(i)
        end do
        error = 0

    end subroutine tri_gauss

    subroutine back_tri_substitution(d,c,b)
        implicit none

        ! This subroutine performs back substitution to a 
        ! tridiagonal system of linear equations that has been
        ! reduced to upper triangular form

        ! Dummy arguments
        real, dimension(:), intent(in) :: d,c 
        real, dimension(:), intent(out) :: b

        ! Local variables
        integer :: i,n 

        n = size(d)
        b(n) = b(n)/d(n)
        do i = n-1,1,-1
            b(i) = (b(i) - c(i)*b(i+1))/d(i)
        end do

    end subroutine back_tri_substitution

end module tridiagonal_systems

module spline
    use tridiagonal_systems
    implicit none
    
contains

    subroutine cubic_spline(x,y,a,b,c,d,error)
        ! This subroutine calculates the coeffiients of a 
        ! cubic spline through the set of data points with
        ! x-coordinates in the array x and corresponding
        ! y-coordinates in the array y.
        ! The coefficients of the cubic polynomials will be
        ! put in arrays a, b, c, d
        ! error will indicate the success or failure of the fit

        ! Dummy arguments
        real, dimension(0:), intent(in) :: x,y
        real, dimension(0:), intent(out) :: a,b,c,d 
        integer, intent(out) :: error

        ! Local variables
        integer :: n,i 
        real, dimension(0:size(x,1)-2) :: h     ! Automatic array
        ! Automatic arrays for tridiagonal equations
        real, dimension(0:size(x,1)-1) :: t,u,v,w 

        ! Validity checks
        n = size(x) - 1
        if (n < 1) then
            ! There is no problem to solve
            error = -1
            return
        end if
        if (n+1 /= size(y) .or. &
                n /= size(a) .or. &
                n /= size(b) .or. &
                n /= size(c) .or. &
                n /= size(d)) then
            ! The array sizes don't correspond.
            error = -2
            return
        end if

        ! Test that the x-coordinates are either strictly
        ! increasing or strictly decreasing
        if (x(0) < x(1)) then
            ! Test that x-coordinates are ordered increasingly
            do i = 1,n-2
                if (x(i) < x(i+1)) cycle
                ! x-coordinates aren't monotonically increasing
                error = -3
                return
            end do
        else if (x(0) == x(1)) then
            ! x-coordinates aren't distinct
            error = -3
            return
        else
            ! Test that x-coordinates are ordered decreasingly
            do i = 1,n-2
                if  (x(i) > x(i+1)) cycle
                ! x-coordinates aren't monotonically decreasing
                error = -3
                return
            end do
        end if

        ! Data is OK
        error = 0
        ! Set h array to interval lengthd
        do i = 0,n-1
            h(i) = x(i+1) - x(i)
        end do

        ! Fill up coefficient arrays for the tridiagonal system
        do i = 1,n-1
            t(i) = h(i-1)
            u(i) = 2.0*(h(i-1) + h(i))
            v(i) = h(i)
            w(i) = 6.0*((y(i+1)-y(i))/h(i) - (y(i)-y(i-1))/h(i-1))
        end do

        ! Set end-point conditions
        u(0) = 1.0
        v(0) = 0.0
        w(0) = 0.0
        t(n) = 0.0
        u(n) = 1.0
        w(n) = 0.0

        ! Calculate the sigma values
        call tri_solve(t,u,v,w,error)
        if (error /= 0) then
            print *,"An 'IMPOSSIBLE' error has occurred - call &
                    &consultant."
            stop
        end if

        ! Calculate the spline coefficients from the sigmas
        do i = 0,n-1
            a(i) = (w(i+1)-w(i))/(6.0*h(i))
            b(i) = w(i)/2.0
            c(i) = (y(i+1)-y(i))/h(i) - (w(i+1)+2.0*w(i))*h(i)/6.0
            d(i) = y(i)
        end do
    end subroutine cubic_spline

end module spline

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