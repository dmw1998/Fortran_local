recursive function factional(n) result(factional_n)
    implicit none

    ! Result variable
    real :: factional_n

    ! Dummy argument
    integer,intent(in) :: n

    ! Determine wether further recursive is required
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

end function factorial