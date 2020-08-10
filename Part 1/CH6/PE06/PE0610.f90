integer function factor(num)

integer, intent(in) :: num

integer :: i 

if (num == 1 .or. num == 2) then
    factor = 1
else
    do i = 2,num-1
        if (mod(num,i) == 0) then
            factor = i
            exit 
        else
            factor = 1
            exit
        end if
    enddo
end if 

end function factor

program PE0610
implicit none

integer, external :: factor

integer :: int, k

print *,"Please input a number that you want to know if it is a prime number."
read *, int


do k = 1,int 
    if (factor(k) == 1) then
        print *, k
    end if 
end do

end program PE0610