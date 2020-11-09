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