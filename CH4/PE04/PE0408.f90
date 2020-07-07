real function F_bw_2bodies(m1,m2,r)
implicit none

! Dummy argument declaration
real, intent(in) :: m1,m2,r

! Local variable declaration
real, parameter :: G = 6.673E-11

! Calculate the force of gravity b/w two bodies
F_bw_2bodies = G*m1*m2/r**2
    
end function F_bw_2bodies

program PE0408
implicit none

real, external :: F_bw_2bodies

real :: m1,m2,r

print *,"Please input the masses of the two bodies."
read *, m1,m2
print *,"Please input the distence between the two bodies."
read *, r

print *,"The force of gravity between two bodies is ",F_bw_2bodies(m1,m2,r)

end program PE0408