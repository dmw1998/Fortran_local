program PE0806
implicit none

integer,dimension(12) :: arr 

print *,"Please input an array of twelve 5-digit numbers."
read '(12(I5,1X))', arr

print '(I6)', arr
print '(3I6)', arr
print '(12I6)',arr

end program PE0806