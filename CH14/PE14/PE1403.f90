program PE1403
implicit none

! This program calculates 1/n!

! Declarations

! four byte real
real(kind = 4) :: recip_frac_4 = 1.0

! eight byte real
real(kind = 8) :: recip_frac_8 = 1.0

! sixteen byte real
real(kind = 16) :: recip_frac_16 = 1.0

integer :: n = 0

! Loop until 1/n! is indistinguishable from zero
do
    n = n+1 
    recip_frac_4 = recip_frac_4/n 
    ! print *, n, recip_frac_4
    if (recip_frac_4 == 0) exit
end do

print *, "For type 4, the value of 1/n! was indidtinguishable from &
         &zero when n = ", n 

n = 0
do
    n = n+1 
    recip_frac_8 = recip_frac_8/n 
    ! print *, n, recip_frac_8
    if (recip_frac_8 == 0) exit
end do

print *, "For type 8, the value of 1/n! was indidtinguishable from &
         &zero when n = ", n 

n = 0
do
    n = n+1 
    recip_frac_16 = recip_frac_16/n 
    ! print *, n, recip_frac_16
    if (recip_frac_16 == 0) exit
end do

print *, "For type 16, the value of 1/n! was indidtinguishable from &
         &zero when n = ", n 
         
end program PE1403