real function energy(m)
implicit none

real, intent(in) :: m

real, parameter :: c=2.9979E8

! Calculate the energy equivalent of mass m
energy = m*c**2
    
end function energy

program PE0409
implicit none

real, external :: energy

real :: m

print *,"Please input the mass m."
read *,m

! Calculate and print the energy equivalent of mass m
print *,"The equivalent of mass m is ",energy(m)

end program PE0409