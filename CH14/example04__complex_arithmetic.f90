program complex_arithmetic
implicit none

complex :: a,b,c 

! Read two complex numbers
a = (12.5,8.4)
b = (6.5,9.6)
c = a*b

! Print data items and their product
print 200,a,b,c 
200 format("  a = (",F10.3,",",F10.3,")"/   &
           "  b = (",F10.3,",",F10.3,")"/   &
           "a*b = (",F10.3,",",F10.3,")")

end program complex_arithmetic