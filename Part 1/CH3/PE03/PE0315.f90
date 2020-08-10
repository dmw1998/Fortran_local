program PE0315
implicit none

real, parameter :: e = 1.6E-19, epsilon = 8.86E-12, pi = 3.1416
real :: phi, r, zz
integer :: z

print *,"Please input the distance r (m) from a particle and its charge z, which is an integer."
read *, r, zz

phi = zz*e/(4.0*pi*epsilon*r)
z = zz

print *," The Coulomb potential at distance ",r,"m from&
        & a particle with a charge of ",z,"C is ",phi,"N."

end program PE0315