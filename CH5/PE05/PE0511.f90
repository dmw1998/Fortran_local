program PE0511
implicit none

real :: t, brt
real, parameter :: pi = 3.14159265

print *,"Please inpput the valuse of the time t."
read *, t

t = mod(t,6.4)

if (t<=0.9) then
    brt = 2.5
else if (t>0.9 .and. t<=2.3) then
    brt = 3.355 - log(1.352 + cos(pi*(t-0.9)/0.7))
else if (t>2.3 .and. t<=5.2) then
    brt = 3.598 - log(1.998 + cos(pi*(t-44)/0.4))
else if (t>5.2 .and. t<=6.4) then
    brt = 2.5
end if

print *,"The brightness of the star is ",brt

end program PE0511