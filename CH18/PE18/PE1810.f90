module Gauss_Seiled
    implicit none
    
contains

subroutine GS_solve(A,b,x,tol,iter,error)

    real, dimension(:,:) :: A 
    real, dimension(:) :: b 
    real :: tol
    integer :: iter,error

    real, dimension(size(b)) :: x
    real :: old,sum,ea
    integer :: i, j, n 

    call rearrange_equ(A,b,error)

    if (error > 0) then
        print '("The",I3,"th colum has no nonzero entries")', error
        stop
    else if (error == -1) then
        print *, "Please recheck your input."
        stop
    end if

    n = size(b)

    iter = 0
    error = 0
    do while (iter < 100 .and. error == 0)
        iter = iter + 1
        error = 1
        do i = 1,n
            old = x(i)
            sum = b(i)
            do j = 1,n
                if (i /= j)then
                    sum = sum - A(i,j)*x(j)
                end if
            end do
            x(i) = sum
            if (x(i) /= 0 .and. error == 1) ea = (abs(x(i)-old)/x(i))*100
            if(ea > tol) error = 0
        end do
    end do

end subroutine GS_solve

subroutine rearrange_equ(A,b,error)

    real, dimension(:,:) :: A 
    real, dimension(:) :: b
    integer :: error

    real, dimension(size(b)) :: temp_array
    real :: temp_b
    integer :: i, j, n 

    n = size(b)
    error = 0

    if (size(A,2) /= n) then
        error = -1
        return
    end if

    do i = 1,n
        if (A(i,i) == 0) then
            do j = n,1,-1
                if (A(j,i) /= 0 .and. j > i) then
                    error = 0
                    temp_array = A(i,:)
                    A(i,:) = A(j,:)
                    A(j,:) = temp_array

                    temp_b = b(i)
                    b(j) = b(i)
                    b(i) = temp_b
                    return

                else if (A(j,i) /= 0 .and. j < i .and. a(i,j) /= 0) then
                    error = 0
                    temp_array = A(i,:)
                    A(i,:) = A(j,:)
                    A(j,:) = temp_array

                    temp_b = b(i)
                    b(j) = b(i)
                    b(i) = temp_b
                    return

                else
                    error = i
                end if
            end do
        end if
        if (error /= 0) return
    end do

    if (error == 0) then
        do i = 1,n 
            do j = 1,n 
                A(i,j) = A(i,j)/A(i,i)
            end do
                b(i) = b(i)/A(i,i)
        end do
    end if

end subroutine rearrange_equ

end module Gauss_Seiled

program PE1810
    use Gauss_Seiled
    implicit none

    real, dimension(:,:), allocatable :: A 
    real, dimension(:), allocatable :: b, x
    integer :: n, i, j, iter, error
    real :: tol = 1.0E-6

    print *,"How many equations are there?"
    read *, n 

    allocate(A(n,n))
    allocate(b(n))
    allocate(x(n))

    ! Get coefficients
    print *,"Type coefficients for each equation in turn"
    do i = 1,n 
        read *,(A(i,j),j=1,n), b(i)
    end do

    print *,"Type an array of initial guess."
    read *, (x(i),i=1,n)

    call GS_solve(A,b,x,tol,iter,error)

    if (error == -2) then
        print *,"No solution is found after 100 iteration."
    else
        print '("After",I3," iterations, the solution is")', iter
        print '(1X,"x(",I2,") = ",F10.5)', (j,x(j),j=1,n)
    end if
    
end program PE1810