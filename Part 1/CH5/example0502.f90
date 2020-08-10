real function cube_root(x)
implicit none

! Function to calculate the cube root of a real number

! Dummy argument declaration
real, intent(in) :: x 

! Local constant
real, parameter :: epsilon = 1E-20

! Elliminate (nearly) zero case
if (abs(x)<epsilon) then
    cube_root = 0.0

! Calculate cube root by using logs
else if (x<0) then
    ! First deal with negative argument
    cube_root = -exp(log(-x)/3.0)
else
    ! Positive argument
    cube_root = exp(log(x)/3.0)
end if

end function cube_root

program example0502
implicit none

real, external :: cube_root

real :: x

print *,"Please input a number"
read *, x
print *, cube_root(x)

end program example0502