module time_util1108
implicit none

    type time
        integer :: hour,minute
    end type time

    interface operator(+)
        module procedure add_time_time
        module procedure add_time_real
        module procedure add_real_time
    end interface

    interface operator(<)
        module procedure time_lt_time 
    end interface

    interface assignment(=)
        module procedure time_assign_real
        module procedure real_assign_time
    end interface

contains

    function add_time_time(a,b) result(add)
    implicit none

        type(time), intent(in) :: a,b
        type(time) :: add
        integer :: minutes,carry

        minutes = a%minute + b%minute
        carry = minutes/60
        add%minute = mod(minutes,60)  
        add%hour = a%hour + b%hour + carry 

    end function add_time_time

    function add_time_real(a,b)
    implicit none

        type(time), intent(in) :: a
        real, intent(in) :: b
        type(time) :: add_time_real
        type(time) :: tb

        tb%hour = int(b)
        tb%minute = int((b-tb%hour)*60)
        add_time_real = add_time_time(a,tb)

    end function add_time_real

    function add_real_time(a,b)
    implicit none

        real, intent(in) :: a
        type(time), intent(in) :: b
        type(time) :: add_real_time

        add_real_time = add_time_real(b,a)

    end function add_real_time

    logical function time_lt_time(a,b)
    implicit none

        type(time), intent(in) :: a,b

        if (a%hour < b%hour) then
            time_lt_time = .true.
        end if

        if (a%minute < b%minute) then
            time_lt_time = .true.
        end if

        time_lt_time = .false.

    end function time_lt_time

    subroutine time_assign_real(a,b)
    implicit none

        type(time), intent(out) :: a
        real, intent(in) :: b

        a%hour = int(b)
        a%minute = int((b-a%hour)*60)

    end subroutine time_assign_real

    subroutine real_assign_time(a,b)
    implicit none

        real, intent(out) :: a
        type(time), intent(in) :: b  

        a = b%hour + real(b%minute)/60.0

    end subroutine real_assign_time

    subroutine output(t)
    implicit none

        type(time), intent(in) :: t

        write(*,"(I2,':',I2.2)") t%hour, t%minute

    end subroutine output

end module time_util1108

program Ex1108
use time_util1108
implicit none

type(time) :: a,b,c
real :: rt

a = 0.5   
b = 0.1 + a 
c = a+0.6 
rt = time(1,30) + time(2,30)

call output(c)

write(*,*) rt
write(*,*) a < b

end program Ex1108
