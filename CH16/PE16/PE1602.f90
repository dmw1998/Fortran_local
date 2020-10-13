program PE1602
implicit none

integer :: n, i
real :: max_abs, min_abs, mean
real, pointer :: p(:)
logical :: det = .true.

print *,"How many numbers are you going to input?"
print *,"Please input an integer between 5 and 20."

do while (det)
    read *, n
    if (n < 5 .or. n > 20) then
        print *,"Fail to read the data."
        print *,"Please input an integer between 5 and 20."
    else
        det = .false.
    end if
end do

allocate(p(n))
write(*,'("Please input ",I2," numbers")') n
read *, (p(i),i=1,n)

max_abs = maxval(abs(p))
min_abs = minval(abs(p))
mean = sum(p)/n 

print *,"The largest absolute values is ", max_abs
print *,"The samllest absolute values is ", min_abs
print *,"The mean is ", mean

end program PE1602