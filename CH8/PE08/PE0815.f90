program name
implicit none

real :: x

print *,"Please input a number."
read *, x

if (x == int(x)) then
    print '(I8)', int(x)
else if (abs(x/100000) < 1) then 
    print '(F9.3)', x
else
    print "(e10.3)", x
end if

end program name