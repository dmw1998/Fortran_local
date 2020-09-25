program PE1410
implicit none

integer,parameter :: p6 = selected_real_kind(6), p14 = selected_real_kind(14)
complex(kind = p6) :: a,b,c,delta,sqdelta,z1,z2
complex(kind = p14) :: aa,bb,cc,delta1,sqdelta1,z3,z4

! print *,"Please input a complex number z = x + yi in the form (x,y)."
! print *,"Please input the complex coefficients of a quadratic"
! print *,"equation ax^2 + bx + c = 0 in the order of a, b, c."
! read *, a, b, c 

a = (1,0_p6)
b = (-6.00001_p6, -7.99999_p6)
c = (-6.99993_p6, 24.00001_p6)

delta = b**2 - 4*a*c 
sqdelta = sqrt(delta)

z1 = (-b + sqdelta)/(2*a)
z2 = (-b - sqdelta)/(2*a)

print 100, z1, z2
print 120, z1, z2

aa = (1,0_p14)
bb = (-6.00001_p14, -7.99999_p14)
cc = (-6.99993_p14, 24.00001_p14)

delta1 = bb**2 - 4*aa*cc 
sqdelta1 = sqrt(delta)

z3 = (-bb + sqdelta1)/(2*aa)
z4 = (-bb - sqdelta1)/(2*aa)

print 110, z1, z2
print 120, z1, z2

100 format ("z1 = ",F10.6," + ",F10.6,"i, z2 = ",F10.6," - ",F10.6,"i")
110 format ("z1 = ",F20.14," + ",F20.14,"i, z2 = ",F20.14," - ",F20.14,"i")
120 format ("z1 = ",F10.5," + ",F10.5,"i, z2 = ",F10.5," - ",F10.5,"i")

end program PE1410