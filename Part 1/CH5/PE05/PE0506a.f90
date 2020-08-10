program PE0506a
implicit none

real :: x

print *,"Please input a number."
read *, x

if (mod(x,2.0) == 0) then
    print *,"The number is even."
else
    print *,"The number is odd."
end if 

if (mod(x,7.0) == 0) then
    print *,"It is divisible by seven."
else
    print *,"It is not divisible by seven."
end if

if (int(x**0.5) == x**0.5) then
    print *,"It is a perfect square."
else
    print *,"It is not a perfect square."
end if 

end program PE0506a