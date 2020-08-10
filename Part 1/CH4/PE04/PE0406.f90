function distence(p1,p2)
    use point2d
    implicit none

    type(coord), intent(in) :: p1,p2
    real :: distence

    distence = sqrt((p2%x-p1%x)**2+(p2%y-p1%y)**2)
    
end function distence

program PE0406
use point2d
implicit none

type(coord) :: p1,p2
real,external :: distence

print *,"Please input the coordinates of these two points"
read *, p1,p2

print *,"The distence between the two points is ",distence(p1,p2)

end program PE0406