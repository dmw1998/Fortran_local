module constants01
implicit none

! This module contains the physical and other constants
! for use with the program youngs_modulus

! Define a real kind type q with at least 6 decimal
! digits and an exponent range from 10^30 to 10^(-30)
integer,parameter :: q = selected_real_kind(p=6,r=30)

! Define pi
real(kind=q),parameter :: pi = 3.1415925636_q

! Define the size of the largest problem set that can be processed
integer,parameter :: max_dat=100

end module constants01

program youngs_modulus
use constants
implicit none

! This program calculates Young's modulus for a piece of wire
! using experimental data, and also calculates the unstretched
! length of the wire

! Input variables
real(kind=q),dimension(max_dat) :: wt,len 
real(kind=q) :: diam 
integer :: n_sets

! Other variables
real(kind=q) :: k,l,e,g=9.18
integer :: i 

! Read data
print *,"How many sets of data?"
read *,n_sets

! End execution if too much or too little data
select case (n_sets)
case(max_dat+1: )
    print *,"Too much data!"
    print *,"Maximum permitted is ",max_dat," data sets"
    stop
case( :1)
    print *,"Not enough data!"
    print *,"There must be at least 2 data sets"
    stop
end select

print *,"Type the data in pairs: weight (in lbs), &
        &length (in inches)"
do i = 1,n_sets
    print '("Data set ", I4, ": ")', i 
    read *,wt(i),len(i)
end do 

print *,"What is the diameter of the wire (in ins.)?"
read *,diam 

! Convert mass to weight
wt = g*wt

! Calculate least squares fit
call least_squares_line(n_sets,wt,len,k,l)

! Calculate Young's modulus
e = (4.0_q*l)/(pi*diam*diam*k)

! Print results
print '(//,5X,"The unstressed length of the wire is",&
        F7.3,"ins.")',l 
print '(5X,"Its Youngs modulus is ",E10.4, &
        " lbs/in/sec/sec"//)',e

end program youngs_modulus

subroutine least_squares_line(n,x,y,a,b)
use constants
implicit none

! This subroutine calculates the least squared fit line ax+b
! to the x-y data pairs

external :: dot_prodct

! Dummy arguments
integer,intent(in) :: n 
real(kind=q),dimension(n),intent(in) :: x,y 
real(kind=q),intent(out) :: a,b 

! Local variables
real(kind=q) :: sum_x,sum_y,sum_xy=0.0,sum_x_sq=0.0
integer :: i 

! Calculate sums
sum_x = sum(x)
sum_y = sum(y)

do i = 1,n 
    sum_xy = sum_xy + x(i)*y(i)
end do

do i = 1,n 
    sum_x_sq = sum_x_sq + x(i)*x(i)
end do 

! Calculate coefficients of least square fit line
a = (sum_x*sum_y - n*sum_xy)/(sum_x*sum_x - n*sum_x_sq)
b = (sum_y - a*sum_x)/n 

end subroutine least_squares_line