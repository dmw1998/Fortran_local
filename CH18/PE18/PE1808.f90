program PE1807
    implicit none

    real, dimension(100,100) :: A1, A2
    real, dimension(100) :: b1, b2
    integer :: error, i

    real :: t1,t2,t3,t4

    call cpu_time(t1)
    do i = 1, 10000
        call random_number(A1)
        call random_number(b1)
        call gaussian_solve_new(A1,b1,error)
    end do
    call cpu_time(t2)

    print "('Time for the new method was ',E14.8,' seconds.')", t2-t1

    call cpu_time(t3)
    do i = 1, 10000
        call random_number(A2)
        call random_number(b2)
        call gaussian_solve(A2,b2,error)
    end do
    call cpu_time(t4)

    print "('Time for the old method was ',E14.8,' seconds.')", t4-t3

contains

    subroutine gaussian_solve_new(a,b,error)
        ! This subroutine silves the linear system Ax = b
        ! where the coefficients of A are stored in the array a
        ! The solution is put in the array b
        ! error indicates if errors are found

        ! Dummy arguments
        real, dimension(:,:), intent(inout) :: a
        real, dimension(:), intent(inout) :: b
        integer, intent(out) :: error

        ! Reduce the equations by Gaussian elimination
        call gaussian_elimination_new(a,b,error)

        ! If reduction was successful, calculate solution by
        ! back substitution
        if (error == 0) call back_substitution_new(a,b,error)
    end subroutine gaussian_solve_new

    subroutine gaussian_elimination_new(a,b,error)
        ! This subroutine performs Gaussion elimination on a 
        ! system of linear equations

        ! Dummy arguments
        ! a contains the coefficients
        ! b contains the right-hand side
        real, dimension(:,:), intent(inout) :: a 
        real, dimension(:) :: b 
        integer,intent(out) :: error

        ! Local variable
        integer :: i, j, n 
        real :: m 

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
            ! Subtract multiples of row i from subsequent rows to 
            ! zero all subsequent coefficients of x_i
            do j = i+1,n
                m = a(j,i)/a(i,i)
                a(j,j:n) = a(j,j:n) - m*a(i,j:n)
                b(j) = b(j) - m*b(i)
            end do
            a(i+1:n,i) = 0
        end do
    end subroutine gaussian_elimination_new

    subroutine back_substitution_new(a,b,error)
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

        b(n) = b(n)/a(n,n)

        ! Solve for each variable in turn
        do i = n-1,1,-1
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
    end subroutine back_substitution_new

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
        integer,intent(out) :: error

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
end program PE1807