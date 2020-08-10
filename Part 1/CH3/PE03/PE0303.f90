program PE0303
implicit none

real :: x, xMinus1, xPlus2, sqxPxM2        ! xMinus is x-1, xPlus2 is x+2, sqxPxM2 is x^2+x-2

print *,"Please input a number x"
read *, x

xMinus1 = x-1
xPlus2 = x+2
sqxPxM2 = x**2 + x - 2

print *,"x-1 = ", xMinus1
print *,"x+2 = ",xPlus2
print *,"x^2+x-2 = ",sqxPxM2

end program PE0303