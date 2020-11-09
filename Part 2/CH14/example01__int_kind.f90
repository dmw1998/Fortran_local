module constants_int
implicit none
    integer,parameter :: range = selected_int_kind(20)
end module constants_int

program degree
use constants_int
implicit none

integer(kind = range) :: x, y, z, k 

x = 360_range
y = 180_range
z = 90_range
k = -180_range

print *, x, y, z, k

end program degree