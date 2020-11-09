program PE1409
implicit none

real :: a, b, c, delta, sqrdelta
real :: r1, r2, x, y

print *,"Please input the coefficients of a quadratic equation"
print *,"az^2 + bz + c = 0 in the order of a, b, c."
read *, a, b, c 

delta = b**2 - 4*a*c 

if (delta < 0) then
    sqrdelta = sqrt(abs(delta))
    x = -b/(2*a)
    y = sqrdelta/(2*a)
    print 100, x,y,x,y
else
    r1 = (-b + sqrt(delta)) / (2*a)
    r2 = (-b - sqrt(delta)) / (2*a)
    print 110, r1, r2
end if 

100 format ("The roots are z_1 = ",F10.5," + ",F10.5,"i, z_2 = ",F10.5," - ",F10.5,"i")
110 format ("The roots are r_1 = ",F10.5,", r_2 = ",F10.5)

end program PE1409