function larger_num(num1,num2)
implicit none

real :: larger_num 
real,intent(in) :: num1,num2

if (num1>num2) then
    larger_num = num1
else if (num1<num2) then
    larger_num = num2
else
    larger_num = num1
end if

end function larger_num

program PE0502
implicit none

real, external :: larger_num
real :: num1, num2

print *,"Please input two numbers."
read *, num1, num2

print *, larger_num(num1,num2)

end program PE0502