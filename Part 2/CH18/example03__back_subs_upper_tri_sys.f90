subroutine back_tri_substution(d,c,b)
    implicit none

    ! This subroutine performs back substitution to a 
    ! tridiagonal system of linear equations that has been
    ! reduced to upper triangular form

    ! Dummy arguments
    ! d is the arrray of diagonal coefficients
    ! c is the array of above-diagonal coefficients
    ! b is the array of right-hand-side coefficients
    ! and will contain the solution on exit
    real, dimension(:), intent(in) :: d,c 
    real, dimension(:), intent(out) :: b

    ! Local variables
    integer :: i,n 

    n = size(d)
    b(n) = b(n)/d(n)
    do i = n-1,1,-1
        b(i) = (b(i) - c(i)*b(i+1))/d(i)
    end do

end subroutine back_tri_substution