module point2d
implicit none

type coord
real :: x,y
end type coord

end module point2d

subroutine distances(p1,p2,d1,d2,d)
! This subroutine calculate the distences of two points from the origin and each other
use point2d
implicit none

! Dummy declarations
type(coord), intent(in) :: p1,p2
real, intent(out) :: d1,d2,d        ! di = distence b/w pi and the origin, d = distence b/w

! Calculate and print the distance d1 of point 1 from the origin
d1 = sqrt(p1%x**2 + p1%y**2)
print *,"The distance d1 of point 1 from the origin is ",d1

! Calculate and print the distance d1 of point 2 from the origin
d2 = sqrt(p2%x**2 + p2%y**2)
print *,"The distance d2 of point 2 from the origin is ",d2

! Calculate and print the distance d between the two points
d = sqrt((p2%x-p1%x)**2+(p2%y-p1%y)**2)
print *,"The distance d between the two points is ",d

end subroutine distances

program PE0401
use point2d
implicit none
! Test the subroutine

! Variable declarations
type(coord) :: p1,p2
real :: d1,d2,d        ! di = distence b/w pi and the origin, d = distence b/w

! Read points
print *,"Please input the first piont."
read *, p1
print *,"Please input the second point."
read *, p2

! Calculate the distence
call distances(p1,p2,d1,d2,d)

end program PE0401