subroutine tri_gauss(a,d,c,b,error)
    implicit none

    ! This subroutine perform Gaussian elimination with no
    ! pivoting on a tridiagonal, diagonally dominant, system
    ! of linear equations

    ! Dummy arguments
    ! Array a holds the subdiagonal coefficients
    ! Array d holds the diagonal coefficients
    ! Array c holds the above-diagonal coefficients
    ! Array b holds the right-hand-side coefficients
    ! error is a variable that indicates success or failure
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
    if (n /= size(d) .or.       &
        n /= size(c) .or.       &
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