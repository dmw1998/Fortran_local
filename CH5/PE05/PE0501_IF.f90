program PE0501_IF
implicit none

real :: x

print *,"Please input a number."
read *, x

if (x>0) then
    print *,"Positive"
else if (x<0) then
    print *,"Negative"
else
    print *,"Zero"
end if

end program PE0501_IF