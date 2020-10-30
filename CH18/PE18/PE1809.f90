module solve_linear_system1809
    implicit none

    type linear_system(n)
        integer, len :: n
        real, dimension(n,n) :: A 
        real, dimension(n) :: b 
    end type linear_system

contains

subroutine gaussian_solve(lin_coeff,error)
    ! This subroutine silves the linear system Ax = b
    ! where the coefficients of A are stored in the array a
    ! The solution is put in the array b
    ! error indicates if errors are found

    ! Dummy arguments
    type(linear_system(*)) :: lin_coeff
    integer, intent(out) :: error

    ! Reduce the equations by Gaussian elimination
    call gaussian_elimination(lin_coeff,error)

    ! If reduction was successful, calculate solution by
    ! back substitution
    if (error == 0) call back_substitution(lin_coeff,error)
end subroutine gaussian_solve

subroutine gaussian_elimination(lin_coeff,error)
    ! This subroutine performs Gaussion elimination on a 
    ! system of linear equations

    ! Dummy arguments
    type(linear_system(*)) :: lin_coeff
    integer,intent(out) :: error

    ! Local variables
    real, dimension(lin_coeff%n) :: temp_array    ! Automatic array
    integer, dimension(1) :: ksave
    integer :: i, j, k
    real :: temp, m 

    ! Valifity checks
    if (lin_coeff%n == 0) then
        error = -1      ! There is no problem to solve
        return
    end if

    ! Dimensions of arrays are OK, so go ahead with Gaussian elimination
    error = 0
    do i = 1,lin_coeff%n-1
        ! Find row with largest  value of |a(j,i)|, j=i, ... , n
        ksave = maxloc(abs(lin_coeff%A(i:lin_coeff%n,i)))

        ! Check whether largest |a(j,i)| is zero
        k = ksave(1) + i - 1
        if (abs(lin_coeff%A(k,i)) <= 1E-5) then
            error = -4      ! No solution possible
            return
        end if

        ! Interchange row i and row k, if necessary
        if (k /= i) then
            temp_array = lin_coeff%A(i,:)
            lin_coeff%A(i,:) = lin_coeff%A(k,:)
            lin_coeff%A(k,:) = temp_array
            ! Interchange corresponding elements of b
            temp = lin_coeff%b(i)
            lin_coeff%b(i) = lin_coeff%b(k)
            lin_coeff%b(k) = temp
        end if

        ! Subtract multiples of row i from subsequent rows to 
        ! zero all subsequent coefficients of x_i
        do j = i+1,lin_coeff%n
            m = lin_coeff%A(j,i)/lin_coeff%A(i,i)
            lin_coeff%A(j,:) = lin_coeff%A(j,:) - m*lin_coeff%A(i,:)
            lin_coeff%b(j) = lin_coeff%b(j) - m*lin_coeff%b(i)
        end do
    end do
end subroutine gaussian_elimination

subroutine back_substitution(lin_coeff,error)
    ! This subroutine performs back substitution once a system
    ! of equations has been reduced by Gaussian elimination

    ! Dummy arguments
    ! error will be set non-zero if an error occurs
    type(linear_system(*)) :: lin_coeff
    integer, intent(out) :: error

    ! Local variables
    real :: sum
    integer :: i,j

    error = 0

    ! Solve for each variable in turn
    do i = lin_coeff%n,1,-1
        ! Check for zero coefficient
        if (abs(lin_coeff%A(i,i)) <= 1E-5) then
            error = -4
            return
        end if

        sum = lin_coeff%b(i)
        do j = i+1,lin_coeff%n
            sum = sum - lin_coeff%A(i,j)*lin_coeff%b(j)
        end do
        lin_coeff%b(i) = sum/lin_coeff%A(i,i)
    end do
end subroutine back_substitution
    
end module solve_linear_system1809

program PE1809
    use solve_linear_system1809
    implicit none
    
    ! This program defines the coefficients of a set of
    ! simultaneous linear equations, and solves them using the
    ! module procedure gaussian_solve
    
    ! Allocatable arrays for coefficients
    type(linear_system(:)), allocatable :: sys 
    
    ! Size of arrays
    integer :: ss
    
    ! Loop variables and error flag
    integer :: i,j,error
    
    ! Get size of problem
    print *,"How many equations are there?"
    read *, ss
    
    ! Allocate arrays
    allocate (linear_system(ss) :: sys)
    
    ! Get coefficients
    print *,"Type coefficients for each equation in turn"
    do i = 1,sys%n 
        read *,(sys%A(i,j),j=1,sys%n), sys%b(i)
    end do
    
    ! Attempt to solve system of equations
    call gaussian_solve(sys,error)
    
    ! Check to see if there were any errors
    if (error <= -1 .and. error >= -3) then
        print *,"Error in call to gaussian_solve"
    else if (error == -4) then
        print *,"System is degenerate"
    else
        print *," "
        print *,"Solution is"
        print '(1X,"x(",I2,") = ",F6.2)', (i,sys%b(i),i=1,sys%n)
    end if

end program PE1809