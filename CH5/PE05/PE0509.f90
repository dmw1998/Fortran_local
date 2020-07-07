program PE0509
implicit none

real :: P, V, I

print *,"Please input for the power rating P and supply voltage V of an appliance"
read *, P, V

I = P/V

if (I<=5) then
    print *,"The suitable cable is 5 amps."
else if (I>5 .and. I<=13) then
    print *,"The suitable cable is 13 amps."
else if (I>13 .and. I<=30) then
    print *,"The suitable cable is 30 amps."
else
    print *,"WARNING: No suitable cable."
end if

end program PE0509