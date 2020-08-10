subroutine trigonometric_fun
real :: angle

    print *,"Please input an angle x."
    read *, angle

    print *,"sin(x) = ",sin(angle)
    print *,"cos(x) = ",cos(angle)
    print *,"tan(x) = ",tan(angle)

end subroutine trigonometric_fun

program PE0603
implicit none

real :: angle
character :: answer

call trigonometric_fun

do
    print *,"Do you want to know this information of another angle? (Y/n)"
    read *, answer

    if (answer == "Y") then 
        call trigonometric_fun
    else
        exit
    end if
    
enddo

end program PE0603