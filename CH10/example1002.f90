module constants02
implicit none

! Define a real kind type q with at least 6 decimal
! digits and an exponent range from 10^30 to 10^(-30)
integer,parameter :: q = selected_real_kind(p=6,r=30)

end module constants02

program zero_find
use constants02
implicit none

! This program finds a root of the equation f(x)=0 in a
! specified interval to within a specified tolerance of the
! true root, by using the bisection method

! Input variables
real(kind=q),external :: f 
real(kind=q) :: left,right,tolerance
integer :: maximum_iterations

! Other variables
real(kind=q) :: zero,delta
integer :: number_of_bisections,err

! Get range and tolerance information
print *,"Give the maximum number of iterations allowed"
read *,maximum_iterations

! Calculatie root by the bisection method
call bisect(f,left,right,tolerance,maximum_iterations,&
            zero,delta,number_of_bisections,err)

! Determine type of result
select case (err)
case (0)
    print *,"The zero is ",zero,"+- ",delta
    print *,"obtained after ",number_of_bisections, &
            " bisections."
case (-1)
    print *,"The input is bad."
case(-2)
    print *,"The maximum number of iterations has been &
            &exceeded."
end select       

end program zero_find

subroutine bisect(f,xl_start,xr_start,tolerance,maximum_iterations,zero,delta,num_bisecs,error)
use constants02
implicit none 

! This subroutine attempts to find a root in the interval
! xl_start to xr_start using the bisection method

! Dummy arguments
real(kind=q),intent(in) ::  xl_start,xr_start,tolerance
integer,intent(in) :: maximum_iterations
real(kind=q),intent(out) :: zero,delta
integer,intent(out) :: num_bisecs,error

! Function used to define equation whose roots are required
real(kind=q),external :: f

! Local variables
real(kind=q) :: x_left,x_mid,x_right,v_left,v_mid,v_right

! Initialize the zero-bounding interval and the function
! values at the end points
if (xl_start < xr_start) then
    x_left = xl_start
    x_right = xr_start
else
    x_left = xr_start
    x_right = xl_start
end if

v_left = f(x_left)
v_right = f(x_right)

! Validity check
if (v_left*v_right >= 0.0 .or. tolerance <= 0.0 .or. &
    maximum_iterations < 1) then
    error = -1
    return
end if 

do num_bisecs = 0,maximum_iterations
    delta = 0.5*(x_right-x_left)
    x_mid = x_left + delta
    if (delta < tolerance) then
        ! Convergence criteria satisfied
        error = 0
        zero = x_mid
        return
    end if 

    v_mid = f(x_mid)

    If (v_left*v_mid < 0.0) then 
        ! A root lies in the left half of the interval
        ! Contract the bounding interval to the left half
        x_right = x_mid
        v_right = v_mid
    else
        ! A root lies in the right half of the interval
        ! Contract the bounding interval to the right half
        x_left = x_mid
        v_left = v_mid
    end if
end do

! The maximum number od iterations has been exceeded 
error = -2
zero = x_mid

end subroutine bisect

! An example of f(x)
function f(x)
use constants02
implicit none 

! Function type
real(kind=q) :: f 

! dummy argument
real(kind=q),intent(in) :: x 

f = x + exp(x)

end function f 