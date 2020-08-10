program quadratic_by_CASE
implicit none

! A program to solve a quadratic equation using a CASE
! statement to distinguish between the three cases

! Constant declaration
real, parameter :: epsilon = 1E-6

! Variables declarations
real :: a,b,c,d,sqrt_d,x1,x2
integer :: selector

! Read coefficients
print *,"Please type the three coefficients a, b and c."
read *, a,b,c

! Calculate delta d and resulting case selector
d = b**2 - 4.0*a*c
selector = d/epsilon

! Calculate and print roots, if any
select case (selector)
case (1:)
    ! Two roots
    sqrt_d = sqrt(d)
    x1 = (-b+sqrt_d)/(a+a)
    x2 = (-b-sqrt_d)/(a+a)
    print *,"The equation has two roots: ",x1," and ",x2

case (0)
    ! One root
    x1 = -b/(a+a)
    print *,"The equation has one roots: ",x1

case (:-1)
    ! No roots
    print *,"The equation has no real roots."

end select

end program quadratic_by_CASE