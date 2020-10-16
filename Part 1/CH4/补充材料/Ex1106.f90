module mod1106
implicit none
    type ta
        integer :: a
    end type ta

    interface operator(+)
        module procedure add
    end interface
contains

    integer function add(a,b)

        type(ta), intent(in) :: a,b

        add = a%a + b%a

    end function

end module mod1106

program main
use mod1106
implicit none

type(ta) :: a,b
integer :: c

a%a = 1
b%a = 2
c = a+b
write(*,*) c

end program