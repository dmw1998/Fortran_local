program ccc
implicit none

integer :: input = 1,num

do while (input /= 0)
    read *, input
    num = num + input
    print *,num
    print *," "
enddo

print *,num

end program ccc