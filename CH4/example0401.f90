program whear_sowing
implicit none

! Aprogram to calculate the quantity of wheat required to
! sow a triangular field

! Variable declarations
real :: a,b,c,s,area,density,quantity
integer :: num_bags

! Read the length of the sodea of the field
print *,"Type the lengths of the three sides of the field &
        &in metres: "
read *, a,b,c

! Calculate the area of the field
s = 0.5*(a+b+c)
area = sqrt(s*(s-a)*(s-b)*(s-c))

! Read sowing density
print *,"What is the sowing density (gm/sq.m.)?"
read *, density

! Calculate quantity of the wheat and the number of the 10kg bags
quantity = density*area
num_bags = 0.0001*quantity + 0.9       ! Round up more than 1kg

! Print reasults
print *,"The area of the field is ",area,"sq. metres"
print *,"and ",num_bags," 10 kilo bags will be required"

end program whear_sowing