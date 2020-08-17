program PE1002
implicit none

integer,parameter :: k4 = selected_real_kind(p=4)
integer,parameter :: k6 = selected_real_kind(p=6)
integer,parameter :: k14 = selected_real_kind(p=14)

real(kind=k4) ::  left4, right4, x4=0.00005
real(kind=k6) :: left6, right6, x6=0.00005
real(kind=k14) :: left14, right14, x14=0.00005

left4 = 1.0-cos(x4)
right4 = 2.0*sin(x4/2.0)**2.0

left6 = 1.0-cos(x6)
right6 = 2.0*sin(x6/2.0)**2.0

left14 = 1.0-cos(x14)
right14 = 2.0*sin(x14/2.0)**2.0

print *,"four decimal places: ", left4, right4
print *,"six decimal places: ", left6, right6
print *,"fourteen decimal places: ", left14, right14

end program PE1002