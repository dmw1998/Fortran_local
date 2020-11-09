program PE1803
    implicit none

    real :: num1=5, num2=7, num3=2000
    integer :: n1=2,n2=7,n3=7
    real :: root1,root2,root3

    call find_nth_root(num1,n1,root1)
    print '("The square root of 5 is",F9.5)', root1

    call find_nth_root(num2,n2,root2)
    print '("The cub root of 7 is",F9.5)', root2

    call find_nth_root(num3,n3,root3)
    print '("The seventh root of 2000 is",F9.5)', root3

contains

    subroutine find_nth_root(num,n,root)

        ! This subroutine only can find one positive real nth root
        ! for given number num using Newton's method

        real, intent(in) :: num
        integer, intent(in) :: n 
        real,intent(out) :: root 

        real :: tol = 1.0E-6, x

        if (num == 0) then
            root = 0
            return
        end if

        ! We set the function as f(x) = x^n - num
        ! then we can apply the Newton's method to find zero point
        ! The zero point is the result we want

        x = num
        do while (abs(x**n - num) > tol)
            x = x - (x**n - num)/(n*x**(n-1))
        end do

        root = x 

    end subroutine find_nth_root
    
end program PE1803