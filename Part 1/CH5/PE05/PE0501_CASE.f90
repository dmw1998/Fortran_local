program PE0501_CASE
implicit none

real :: x
real, parameter :: epsilon = 1E-15
integer :: selector

print *,"Please input a number."
read *, x

selector = x/epsilon

select case (selector)
case (1:)
    print *,"Positive"
case (0)
    print *,"Zero"
case (:-1)
    print *,"Negative"
end select

end program PE0501_CASE