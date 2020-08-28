program PE1106
implicit none

integer :: a,b,hcf

print *,"Please input two positive integers a and b."
read *, a,b

call findhcf(hcf,a,b)

print *,"The highest common factor of a and b is ", hcf

end program PE1106


recursive subroutine findhcf(hcf,a,b)
    implicit none

    ! This subroutine is used to find the highest common factor of a and b

    integer,intent(out) :: hcf 
    integer,intent(in) :: a, b

    integer :: r 

    r = mod(a,b)

    if (r == 0) then
        hcf = b
    else
        call findhcf(hcf,b,r)
    end if
end subroutine findhcf