program PE1306
implicit none

integer :: n, error, p = 2, i, j 
integer,dimension(:),allocatable :: a

print *,"Please input a maximum integer n."
read *, n 

allocate(a(n),stat=error)

if (error /= 0) then
    print *,"Fail to allocate array a."
    stop
end if

do i = 1,n 
    a(i) = i 
end do

do while (p < n)
    do i = p+1,n
        if (mod(a(i),p) == 0) a(i) = 0
    end do 

    p = p+1

    do while (a(p) == 0)
        p = p+1
    end do
end do 

do i = 1,n 
    if (a(i) /= 0) print *, a (i)
end do

end program PE1306