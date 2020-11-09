recursive subroutine factorial(n,factorial_n)
implicit none

! Dummy arguments
integer,intent(in) :: n 
real,intent(out) :: factorial_n

! Determine wether further recusion is required
select case(n)
case (0)
    ! Recursion has reached the end
    factorial_n = 1.0

case(1:)
    ! Recursive call(s) required to obtain (n-1)!
    call factorial(n-1,factorial_n)

    ! Now calculate n!=n*(n-1)!
    factorial_n = n*factorial_n

case default
    ! n is negative - return zero as an error indicator
    factorial_n = 0.0

end select

end subroutine factorial