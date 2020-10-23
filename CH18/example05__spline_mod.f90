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