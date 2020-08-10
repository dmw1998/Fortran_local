program PE0506b
implicit none

real :: x = 2
logical :: a,b,c,e = .true.

do while (e)
    if (mod(x,2.0) == 0) then
        a = .true.
    else
        a = .false.
    end if 

    if (mod(x,7.0) == 0) then
        b = .true.
    else
        b = .false.
    end if

    if (int(x**0.5) == x**0.5) then
        c = .true.
    else
        c = .false.
    end if 

    if ((a .and. b .and. c) .eqv. .true.) then
        print *, x," is the first even number that is &
                &divisible by 7 and is a perfevt square."
        e = .false.
    else
        x = x + 1
    end if

end do

end program PE0506b