function log_of_x_to_b(x,b)
implicit none

! This function gives the logarithm of a number x to base b

real, intent(in) :: x,b
real :: log_of_x_to_b

! Calculate base b
log_of_x_to_b = log(x)/log(b)

end function log_of_x_to_b

program test
implicit none

! Declare external function
real, external :: log_of_x_to_b

! Variable declarations
real :: x,b

print *,"Please input the number x and base b that you want to calculate the logarithm of x to b."
read *, x,b

print *,"The logarithm of a number ",x," to base ",b," is ",log_of_x_to_b(x,b)

end program test