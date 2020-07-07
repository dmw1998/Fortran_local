program PE0507
implicit none

real :: x, tax

print *,"Please input your income."
read *, x

if (x>20000) then
    tax = 0.32*(x-20000) + 0.25*15000
else if (x<5000) then
    tax = 0
else
    tax = 0.25*(x-5000)
end if

print *,"Your income tax is ",tax

end program PE0507