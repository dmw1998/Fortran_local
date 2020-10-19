module Newton_Raphson_mod
contains

    subroutine newton_raphson(f,f_prime,start,epsilon, &
                            max_iter,root,error)
        implicit none

        ! This subroutine finds a root of the equation f(x) = 0
        ! using Newton-Raphson iteration
        ! The function f-prime returns the value of the derivative of
        ! the function f(x).

        ! Dummy arguments
        real, external :: f,f_prime
        real, intent(in) :: start,epsilon
        integer, intent(in) :: max_iter
        real, intent(out) :: root
        integer, intent(out) :: error
            ! error indicates the result of the processing as follows:
            ! = 0   a root was found
            ! = -1  no root fiiiund after max_iter iterations
            ! = -2  the first derivative become zero, and so no further
            !       iterations were possible
            ! = -3  the value of epsilon supplied was negtive or zero

        ! Local variables
        integer :: i 
        real :: f_val,f_der

        ! Check validity of epsilon
        if (epsilon <= 0.0) then
            error = -3
            root = huge(root)
            return
        end if

        ! Begin the iteration at the specified value of x
        root = start

        ! Repeat the iteration up to the maximum number specified
        do i = 1,max_iter
            f_val = f(root)
            ! Output latest estimate while testing
            print '(2(A,E15.6))', "root = ",root," f(root) = ",f_val

            if (abs(f_val) <= epsilon) then
                ! A root has been found
                error = 0
                return
            end if

            f_der = f_prime(root)
            if (f_der == 0.0) then
                ! f'(x)=0, so no more iterations are possible
                error = -2
                return
            end if

            ! Use Newton's iteration to obtain next approximation
            root = root - f_val/f_der
        end do

        ! Process has not converged after max_iter iterations
        error = -1

    end subroutine newton_raphson

end module Newton_Raphson_mod

real function f(x)
    implicit none
    real, intent(in) :: x
    f = x + exp(x)
end function

real function f_prime(x)
    implicit none
    real, intent(in) :: x
    f_prime = 1.0 + exp(x)
end function f_prime

program example1801
use Newton_Raphson_mod
implicit none

real, external :: f, f_prime
real :: start, epsilon, root
integer :: max_iter = 30, error

call newton_raphson(f,f_prime,start,epsilon,max_iter,root,error)
print *, root

end program example1801