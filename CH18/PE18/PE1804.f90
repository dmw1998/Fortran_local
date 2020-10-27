program PE1804
    implicit none

    real, dimension(3,3) :: A1, A2, A3
    real, dimension(4,4) :: A4
    real, dimension(5,5) :: A5
    real, dimension(3) :: b1, b2, b3
    real, dimension(4) :: b4
    real, dimension(5) :: b5
    integer :: error

    A1(1,:) = (/ 2, 3, 1 /)
    A1(2,:) = (/ 1, -2, -1 /)
    A1(3,:) = (/ -2, 1, 3 /)
    b1 = (/ 4, 3, 4 /)
    call gaussian_solve(A1,b1,error)
    print '("x = ",F5.2/ "y = ", F5.2/ "z = ",F5.2/)', b1

    A2(1,:) = (/ 2, 1, -1 /)
    A2(2,:) = (/ 4, -1, -3 /)
    A2(3,:) = (/ 1, 3, 1 /)
    b2 = (/ 1, -3, 4 /)
    call gaussian_solve(A2,b2,error)
    print '("x = ",F5.2/ "y = ", F5.2/ "z = ",F5.2/)', b2

    A3(1,:) = (/ -2, -1, 4 /)
    A3(2,:) = (/ 1, 2, -2 /)
    A3(3,:) = (/ 3, 4, -6 /)
    b3 = (/ 4, 1, -1 /)
    call gaussian_solve(A3,b3,error)
    print '("x = ",F5.2/ "y = ", F5.2/ "z = ",F5.2/)', b3

    A4(1,:) = (/ 1, -2, -1, 1 /)
    A4(2,:) = (/ 3, 1, 1, -2 /)
    A4(3,:) = (/ -2, -3, 2, -1 /)
    A4(4,:) = (/ 1, 1, -1, 1 /)
    b4 = (/ 3, 3, 4, 0 /)
    call gaussian_solve(A4,b4,error)
    print '("x = ",F5.2/ "y = ", F5.2/ "z = ",F5.2/ "w = ",F5.2/)', b4

    A5(1,:) = (/ 1, 0, 0, 0, 1 /)
    A5(2,:) = (/ 0, 2, -1, 0, 0 /)
    A5(3,:) = (/ 2, 0, 0, -1, 0 /)
    A5(4,:) = (/ 0, 0, 2, 1, 0 /)
    A5(5,:) = (/ 0, 1, 0, 0, -2 /)
    b5 = (/ 1, 5, 1, -3, 3 /)
    call gaussian_solve(A5,b5,error)
    print '("x = ",F5.2/ "y = ", F5.2/ "z = ",F5.2/ "w = ",F5.2/ "t = ",F5.2)', b5

contains

subroutine gaussian_solve(a,b,error)
    ! This subroutine silves the linear system Ax = b
    ! where the coefficients of A are stored in the array a
    ! The solution is put in the array b
    ! error indicates if errors are found

    ! Dummy arguments
    real, dimension(:,:), intent(inout) :: a
    real, dimension(:), intent(inout) :: b
    integer, intent(out) :: error

    ! Reduce the equations by Gaussian elimination
    call gaussian_elimination(a,b,error)

    ! If reduction was successful, calculate solution by
    ! back substitution
    if (error == 0) call back_substitution(a,b,error)

end subroutine gaussian_solve

subroutine gaussian_elimination(a,b,error)
    ! This subroutine performs Gaussion elimination on a 
    ! system of linear equations

    ! Dummy arguments
    ! a contains the coefficients
    ! b contains the right-hand side
    real, dimension(:,:), intent(inout) :: a 
    real, dimension(:) :: b 
    integer, intent(out) :: error

    ! Local variables
    real, dimension(size(a,1)) :: temp_array    ! Automatic array
    integer, dimension(1) :: ksave
    integer :: i, j, k, n 
    real :: temp, m 

    ! Valifity checks
    n = size(a,1)
    if (n == 0) then
        error = -1      ! There is no problem to solve
        return
    end if
    if (n /= size(a,2)) then
        error = -2      ! a is not square
        return
    end if
    if (n /= size(b)) then
         error = -3     ! Size of b does not match a
         return
    end if 

    ! Dimensions of arrays are OK, so go ahead with Gaussian elimination
    error = 0
    do i = 1,n-1
        ! Find row with largest  value of |a(j,i)|, j=i, ... , n
        ksave = maxloc(abs(a(i:n,i)))

        ! Check whether largest |a(j,i)| is zero
        k = ksave(1) + i - 1
        if (abs(a(k,i)) <= 1E-5) then
            error = -4      ! No solution possible
            return
        end if

        ! Interchange row i and row k, if necessary
        if (k /= i) then
            temp_array = a(i,:)
            a(i,:) = a(k,:)
            a(k,:) = temp_array
            ! Interchange corresponding elements of b
            temp = b(i)
            b(i) = b(k)
            b(k) = temp
        end if

        ! Subtract multiples of row i from subsequent rows to 
        ! zero all subsequent coefficients of x_i
        do j = i+1,n
            m = a(j,i)/a(i,i)
            a(j,:) = a(j,:) - m*a(i,:)
            b(j) = b(j) - m*b(i)
        end do
    end do

end subroutine gaussian_elimination

subroutine back_substitution(a,b,error)
    ! This subroutine performs back substitution once a system
    ! of equations has been reduced by Gaussian elimination

    ! Dummy arguments
    ! The array a contains the coefficients
    ! The array b contains the right-hand side coefficients.
    ! and will contain the solution on exit
    ! error will be set non-zero if an error occurs
    real, dimension(:,:), intent(in) :: a 
    real, dimension(:), intent(inout) :: b 
    integer, intent(out) :: error

    ! Local variables
    real :: sum
    integer :: i,j,n 

    error = 0
    n = size(b)

    ! Solve for each variable in turn
    do i = n,1,-1
        ! Check for zero coefficient
        if (abs(a(i,i)) <= 1E-5) then
            error = -4
            return
        end if

        sum = b(i)
        do j = i+1,n
            sum = sum - a(i,j)*b(j)
        end do
        b(i) = sum/a(i,i)
    end do

end subroutine back_substitution
    
end program PE1804