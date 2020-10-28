module tridiagonal_systems_1806
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

        ! This subroutine perform Gaussian elimination with
        ! pivoting on a tridiagonal, diagonally dominant, system
        ! of linear equations

        ! Dummy arguments
        real, dimension(:) :: a,c 
        real, dimension(:) :: d,b
        integer :: error

        ! Local variables
        real :: m, temp 
        integer :: n, i, j, k
        integer, dimension(1) :: ksave

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

        do i = 1,n-1
            ! Find row with largest value of |a(j)|, j=i, ... , n
            ksave = maxloc(abs(a(i:n)))
    
            ! Check whether largest |a(j,i)| is zero
            k = ksave(1) + i - 1
            if (abs(a(k)) <= 1E-5) then
                error = -4      ! No solution possible
                return
            end if
    
            ! Interchange row i and row k, if necessary
            if (k /= i) then
                temp = a(i)
                a(i) = a(k)
                a(k) = temp
                ! Interchange corresponding elements of b,c,d
                temp = b(i)
                b(i) = b(k)
                b(k) = temp

                temp = c(i)
                c(i) = c(k)
                c(k) = temp

                temp = d(i)
                d(i) = d(k)
                d(k) = temp
            end if

            ! Calculate new coefficients of upper diagonal system
            do j = i,n-1
                m = a(j+1)/d(j)
                d(j+1) = d(j+1) - m*c(j)
                b(j+1) = b(j+1) - m*b(j)
            end do
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

end module tridiagonal_systems_1806

program PE1806
    use tridiagonal_systems_1806
    implicit none

    real, dimension(4) :: a = (/ 1, 2, 0, -1 /), &
                          b = (/ 3, 4, 4, 0 /),  &
                          c = (/ 3, -3, 2, -1 /),&
                          d = (/ 1, 1, -1, 1 /)
    integer :: error

    call tri_solve(a,b,c,d,error)

    print *, b 
    
end program PE1806