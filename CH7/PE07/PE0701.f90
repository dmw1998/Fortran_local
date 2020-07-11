program PE0701
implicit none

integer,dimension(20) :: arr
integer :: i,j,k,m

arr = (/(i,i=1,20)/)

do j = 1,10
    k = 21-j
    m = arr(j)
    arr(j) = arr(k)
    arr(k) = m 
end do

print *, arr

end program PE0701