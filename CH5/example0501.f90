program wheat_sowing
implicit none

! A program to calculate the quatity of whear required to
! sow a triangular field

! Variable declarations
real :: a,b,c,s,area,density,quantity
integer :: num_bags

! Read the lengths of the sides of the field
print *,"Type the lengths of the three sides of the field &
        &in meters."
read *, a,b,c

! Calcluate the area of the field
s = 0.5*(a+b+c)
area = sqrt(s*(s-a)*(s-b)*(s-c))

! Read sowing density
print *,"What is the sowing density (gm/sq.m.)?"
read *, density

! Calculate quantity of wheat in grams and the number of 
! full 10 kg bags
quantity = density*area
num_bags = 0.00001*quantity     ! Any part-full bag is excluded

! Check to see if another bag is required
if (0.00001*quantity - int(0.00001*quantity )> 0.1) then
    num_bags = num_bags+1
end if

! Print results
print *,"The area of the field is ",area," sq. meters"
print *,"and ",num_bags," 10 kilo bags will be required"

end program wheat_sowing