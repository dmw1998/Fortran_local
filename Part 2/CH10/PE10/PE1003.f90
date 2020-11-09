program PE1003
implicit none

! This program calculates factorial 200

! Variable and constant daclarations
integer,parameter :: kind12 = selected_real_kind(p=12)
real(kind=kind12) :: log_factorial,mantissa,stirling, &
                     log_stirling,twopi
integer :: exponent, i 

! Loop to calculate 200! by adding logarithms
log_factorial = 0.0
do i=1,200
    log_factorial = log_factorial + log10(real(i,kind12))
end do 

! Obtain exponent and mantissa from logarithm and print
exponent = int(log_factorial)
mantissa = fraction(log_factorial)
print *,"200! is ",mantissa," times 10 to the powere &
        &of ",exponent

! Now calculate it using Stirling's approximation
twopi = 2.0_kind12*atan(1.0_kind12)
stirling = 200.5_kind12*log(200.0_kind12) - &
           200.0_kind12 + 0.5_kind12*log(twopi)

! Convert from log to base e to log to base 10
log_stirling = stirling/log(10.0_kind12)

! Obtain exponent and mantissa from logrithm and print
exponent = int(log_stirling)
mantissa = fraction(log_stirling)
print *,"Stirling's formula for 200! gives ",mantissa, &
        " times 10 to the power of ",exponent
        
end program PE1003