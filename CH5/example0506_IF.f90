program quadratic_by_block_IF
implicit none

! A program to solve a quadratic equation using a block IF
! statement to distinguish between the three cases

! Constant declaration
real, parameter :: epsilon = 1E-6

! Variables declarations
real :: a,b,c,d,sqrt_d,x1,x2

! Read coefficients
print *,"Please type the three coefficients a, b and c."
read *, a,b,c

! Calculate delta d
d = b**2 - 4.0*a*c

! Calculate and print roots, if any
if (d>=epsilon) then
    ! Two roots
    sqrt_d = sqrt(d)
    x1 = (-b+sqrt_d)/(a+a)
    x2 = (-b-sqrt_d)/(a+a)
    print *,"The equation has two roots: ",x1," and ",x2

else if (d>-epsilon) then
    ! One root
    x1 = -b/(a+a)
    print *,"The equation has one roots: ",x1

else
    ! No roots
    print *,"The equation has no real roots."

end if

end program quadratic_by_block_IF