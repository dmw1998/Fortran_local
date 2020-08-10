program complex_arithmetic
implicit none

! A program to illustrate the use of a derived type to perform complex arithmetic

! Type definition
type complex_number
real :: real_part, imaginary_part
end type complex_number

! Variable definitions
type(complex_number) :: c1, c2, sum, diff, prod

! Read data
print *,"Please supply two complex numbers"
print *,"Each complex number should be typed as two numbers,"
print *,"representing the real and imaginary parts of the number"
read *, c1, c2

! Calculate sum, difference and product
sum%real_part = c1%real_part + c2%real_part
sum%imaginary_part = c1%imaginary_part + c2%imaginary_part

diff%real_part = c1%real_part - c2%real_part
diff%imaginary_part = c1%imaginary_part - c2%imaginary_part

prod%real_part = c1%real_part*c2%real_part - c1%imaginary_part*c2%imaginary_part
prod%imaginary_part = c1%real_part*c2%imaginary_part + c1%imaginary_part*c2%real_part

! Print results
print *,"The sum of the two numbers is ",sum%real_part,"+",sum%imaginary_part,"i"
print *,"The difference between the two numbers is ",diff%real_part,"+",diff%imaginary_part,"i"
print *,"The product of the two numbers is ",prod%real_part,"+",prod%imaginary_part,"i"

end program complex_arithmetic