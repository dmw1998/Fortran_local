program PE0615
implicit none

real :: v0=9.0, h0=200.0, f0, h1, v1, t=0

print *,"Please choose every second how many units fuel to burn."
print *,"(Input a number between 1 and 10)"
read *, f0

do
    t = t+1
    h1 = h0-v0
    v1 = v0 + 5.0 - f0
    print *, h1, v1
    if (h1 <= 0) then
        if (v1 < 10) then
            print *,"Achieved a soft landing!"
        else
            print *,"Landing speed is too large."
        end if
        exit
    else if (v1 < 0) then
        print *,"Velocity chaged direction."
        exit
    end if
    h0 = h1
    v0 = v1
end do

end program PE0615