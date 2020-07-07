program geometry
implicit none

! A preogram to use derived types for two-demensional geomatric calculations

! Type definitions
TYPE point
    REAL :: x, y        ! Cartesian coordinates of the point
END TYPE point

TYPE line
    REAL :: a,b,c       ! coefficients of defining equation
END TYPE line

! Variable declarations
TYPE(point) :: p1,p2
TYPE(line) :: p1_to_p2

! Read data
print *,"Please type co-ordinates of first point"
read *,p1
print *,"Please type co-ordinates of second point"
read *,p2

! Calculate coefficient of equation representing the line
p1_to_p2%a = p2%y - p1%y
p1_to_p2%b = p2%x - p1%x
p1_to_p2%c = p1%y*p2%x - p2%y*p1%x

! Print result
print *,"The equation of the line joining these two points is"
print *,"ax + by + c = 0"
print *,"where a = ", p1_to_p2%a
print *,"      b = ", p1_to_p2%b
print *,"      c = ", p1_to_p2%c

end program geometry