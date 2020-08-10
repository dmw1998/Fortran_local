program PE0312
implicit none

type circle
real :: x0, y0, r
end type circle

type(circle) :: circle_input
real :: c, d, e

print *,"Please input the coordinates of the centre of the circle and its radius."
read *, circle_input

! Since (x-x0)^2 + (y-y0)^2 = r ==> x^2 + y^2 - 2xx0 - 2yy0 + x0^2 + y0^2 -r = 0
c = -2.0*circle_input%x0
d = -2.0*circle_input%y0
e = circle_input%x0**2 +circle_input%y0**2 - circle_input%r

print *,"x^2 + y^2 + (",c,")x +  (",d,")y + (",e,") = 0"

end program PE0312