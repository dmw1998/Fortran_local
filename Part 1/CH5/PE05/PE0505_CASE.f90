program PE0505_CASE
implicit none

integer :: x

print *,"Please input a number 1-6."
read *, x

select case(x)
case(1)
    print *,"One"
case(2)
    print *,"Two"
case(3)
    print *,"Three"
case(4)
    print *,"Four"
case(5)
    print *,"Five"
case(6)
    print *,"Six"
case default
    print*,"Please input a number 1-6."
end select

end program PE0505_CASE