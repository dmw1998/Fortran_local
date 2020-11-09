module constants_real
implicit none
    integer,parameter :: real_kind = selected_real_kind(6,30)
end module constants_real

program satellite
use constants_real
implicit none

real(kind = real_kind) :: r, theta, phi, k 

r = 321.172_real_kind
theta = 1.47239_real_kind
phi = 0.172341E-1_real_kind
k = 0.172341555E-31_real_kind

print *, r, theta, phi, k

end program satellite