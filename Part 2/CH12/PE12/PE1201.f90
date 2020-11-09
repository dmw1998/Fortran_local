module Cartesian_coord
    implicit none

    type point
        real :: x,y
    end type point

end module Cartesian_coord

module polar_coord
    implicit none

    type point
        real :: r, theta
    end type point

end module polar_coord

subroutine polar2Cartesian(polar,Cartesian)
    use Cartesian_coord, Cartesian_pt => point
    use polar_coord, polar_pt => point
    implicit none

    type(Cartesian_pt),intent(out) :: Cartesian
    type(polar_pt),intent(in) :: polar

    Cartesian%x = polar%r * cos(polar%theta)
    Cartesian%y = polar%r * sin(polar%theta)

end subroutine polar2Cartesian

subroutine Cartesian2polar(Cartesian,polar)
    use Cartesian_coord, Cartesian_pt => point
    use polar_coord, polar_pt => point
    implicit none

    type(polar_pt),intent(out) :: polar
    type(Cartesian_pt),intent(in) :: Cartesian

    polar%r = sqrt(Cartesian%x**2 + Cartesian%y**2)
    polar%theta = atan(Cartesian%y/Cartesian%x)

end subroutine Cartesian2polar

program PE1201
use Cartesian_coord, Cartesian_pt => point
use polar_coord, polar_pt => point
implicit none

type(Cartesian_pt) :: Cartesian_coord_pt1, Cartesian_coord_pt2
type(polar_pt) :: polar_coord_pt1, polar_coord_pt2

real :: k, b

print *,"Please input a polar coordinate of a point."
read *, polar_coord_pt1%r, polar_coord_pt1%theta

print *,"Please input another polar coordinate of a point."
read *, polar_coord_pt2%r, polar_coord_pt2%theta

call polar2Cartesian(polar_coord_pt1,Cartesian_coord_pt1)
call polar2Cartesian(polar_coord_pt2,Cartesian_coord_pt2)

k = (Cartesian_coord_pt2%y - Cartesian_coord_pt1%y)
b = Cartesian_coord_pt1%y - k*Cartesian_coord_pt1%x

print *,"The Cartesian coordinate of polar coordinate point (", &
        polar_coord_pt1%r,",",polar_coord_pt1%theta,") is (", &
        Cartesian_coord_pt1%x,",",Cartesian_coord_pt1%y,")."
print *,"The Cartesian coordinate of polar coordinate point (", &
        polar_coord_pt2%r,",",polar_coord_pt2%theta,") is (", &
        Cartesian_coord_pt2%x,",",Cartesian_coord_pt2%y,")."
print *,"The equation of the line joining these two points is ", &
        k,"x-y+",b,"=0."

end program PE1201