module secant_mod
contains
    subroutine secant(f,start1,start2,epsilon,max_iter,     &
                    root,error)
        implicit none

        ! This subroutine calculates a root of the equation f(x) = 0
        ! by use of the secant method

        ! Dummy arguments
        real, external :: f
        real, intent(in) :: start1,start2,epsilon
        integer, intent(in) :: max_iter
        real, intent(out) :: root
        integer, intent(out) :: error

        ! Local variables
        real :: x1,x2,x3,f1,f2,f3,tempx,tempf
        integer :: i 

        ! Check validity of initial points
        if (start1 == start2) then
            error = -1
            root = huge(root)       ! Largest number
            return
        end if

        ! Check validity of epsilon
        if (epsilon <= 0.0) then
            error = -2
            root = huge(root)       ! Largest number
            return
        end if

        ! Set up initial pair of points
        x1 = start1
        x2 = start2
        f1 = f(x1)
        f2 = f(x2)

        ! Choose the x with the best function value to be
        ! the most recent estimate
        if (abs(f1) < abs(f2)) then
            tempx = x1
            tempf = f1
            x1 = x2
            f1 = f2
            x2 = tempx
            f2 = tempf
        end if

        ! Repeat the iteration up to the maximum number specified
        do i = 1,max_iter
            if (f1 == f2) then
                ! No further iterations possible
                error = -4
                root = x2
                return
            end if

            ! Calculate next approximation
            x3 = x1 - f1*(x2 - x1)/(f2 - f1)
            f3 = f(x3)

            ! Output latest approximation while testing
            print '(2(A,E15.6))', "x3 = ",x3," f(x3) = ",f3

            if (abs(f3) <= epsilon) then
                ! A root has been found
                error = 0
                root = x3
                return
            end if

            ! Update points for next iteration
            x1 = x2
            f1 = f2
            x2 = x3
            f2 = f3
        end do

        ! Process has not converged after amx_iter steps
        error = -3
        root = x2

    end subroutine secant

end module secant_mod

real function f(x)
    implicit none
    real, intent(in) :: x
    f = x + exp(x)
end function

program example1802
use secant_mod
implicit none

real, external :: f
real :: start1, start2, epsilon, root
integer :: max_iter = 30, error

call secant(f,start1,start2,epsilon,max_iter,root,error)
print *, root

end program example1802