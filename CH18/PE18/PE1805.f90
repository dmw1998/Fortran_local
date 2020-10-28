program PE1805
    implicit none

    real, dimension(4,4) :: A 
    real, dimension(4) :: b 
    integer :: error

    A(1,:) = (/ 10, 7, 8, 7 /)
    A(2,:) = (/ 7, 5, 6, 5 /)
    A(3,:) = (/ 8, 6, 10, 9 /)
    A(4,:) = (/ 7, 5, 9, 10 /)
    b = (/ 32, 23, 33, 31 /)

    call gaussian_solve(A,b,error)
    print '("x = ",F5.2/ "y = ", F5.2/ "z = ",F5.2/ "w = ",F5.2/)', b

    b = (/ 32.01, 22.99, 32.99, 31.01 /)

    call gaussian_solve(A,b,error)
    print '("x = ",F12.5/ "y = ", F12.5/ "z = ",F12.5/ "w = ",F12.5/)', b

    b = (/ 32.1, 22.9, 32.9, 31.1 /)

    call gaussian_solve(A,b,error)
    print '("x = ",F12.5/ "y = ", F12.5/ "z = ",F12.5/ "w = ",F12.5/)', b


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
    
end program PE1805