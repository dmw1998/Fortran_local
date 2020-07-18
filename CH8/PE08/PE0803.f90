program PE0803
implicit none

integer :: i,j 

print '(5X,12I4)',(i,i=1,12)
print '(4X,A)',"X"
do j=1,12
    print '(I2,3X,12I4)',j,(i*j,i=1,12)
end do 

end program PE0803