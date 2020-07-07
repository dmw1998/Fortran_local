program intent_demonstration
    implicit none

    integer, parameter :: a=2
    integer :: b=3,c=4,d

    call problem_sub(a,b,c)
    call problem_sub(b,c,d)
    print *,"a = ",a," and b = ",b

end program intent_demonstration

subroutine problem_sub(arg1,arg2,arg3)
    implicit none

    ! This subroutine returns the product of its first two
    ! arguments via the third argument

    integer :: arg1,arg2,arg3

    arg1 = arg2*arg3

end subroutine