program PE1404
implicit none

integer :: i, j

character(len = 2), dimension(0:15,0:9) :: table

do i = 0,12
    do j = 0,9
        table(i,j) = char(10*i+j)
    end do 
end do

print '(3X,10(I2))', (j, j=0,9)
do i = 0,12
    print '(I3,1X,10(2A))', 10*i, table(i,:)
end do

end program PE1404