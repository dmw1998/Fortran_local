program PE0505_IF
implicit none

real :: x

print *,"Please input a number 1-6."
read *, x

if (x==1) then
    print *,"One"
else if (x==2) then
    print *,"Two"
else if (x==3) then
    print *,"Three"
else if (x==4) then
    print *,"Four"
else if (x==5) then
    print *,"Five"
else if (x==6) then
    print *,"Six"
else
    print*,"Please input a number 1-6."
end if

end program PE0505_IF