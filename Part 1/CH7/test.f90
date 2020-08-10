program test
implicit none

integer :: i,j
integer, dimension(100) :: arr = (/ ((0,i=1,9),10*j,j=1,10) /)

print *, arr

end program test