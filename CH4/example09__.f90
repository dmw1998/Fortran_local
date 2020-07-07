module my_procedures
    implicit none

contains
    subroutine problem_sub(arg1,arg2,arg3)
        implicit none 

        ! This subroutine returns the product of its first two
        ! arguments via the third argument

        integer :: arg1,arg2,arg3
        arg1 = arg2*arg3
    end subroutine problem_sub

end module my_procedures

program intent_demonstration
use my_procedures
    implicit none

    integer, parameter :: a = 2
    integer :: b=3,c=4,d 

    call problem_sub(a,b,c)
    call problem_sub(b,c,d)
end program intent_demonstration