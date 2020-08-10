program PE0614
implicit none

real :: T = 15.0, atm

print *,"       T            Pressure"
do 
    atm = 0.00105*T**2 + 0.0042*T + 1.352
    if (atm > 3.2) exit
    print *,T,atm
    T = T + 1.0

end do

end program PE0614