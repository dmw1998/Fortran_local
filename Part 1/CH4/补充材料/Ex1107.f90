module time_util1107
implicit none

    type MyTime
        integer :: hour,minute
    end type MyTime

    interface operator(.Plus.)
        module procedure add
    end interface

contains

    function add( a, b )

        type(MyTime), intent(in) :: a,b
        type(MyTime) :: add
        integer :: minutes,carry

        minutes = a%minute + b%minute
        carry = minutes/60
        add%minute = mod(minutes,60) 
        add%hour = a%hour + b%hour + carry 
        return

    end function add

    subroutine output(t)

        type(MyTime), intent(in) :: t

        write(*,"(I2,':',I2.2)") t%hour, t%minute

    end subroutine

end module time_util1107

program Ex1107
use time_util1107
implicit none

type(MyTime) :: a,b,c

a = MyTime(1,45)
b = MyTime(2,18)
c = a .plus. MyTime(2,18)

call output(c)
  
end program Ex1107
