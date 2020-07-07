program PE0510
implicit none

integer :: num
real :: gross_cost, discount, net_cost

print *,"Please input the number of watches required."
read *, num

select case(num)
case (1)
    discount = 0
case (2:4)
    discount = 0.05
case (5:9)
    discount = 0.1
case (10:29)
    discount = 0.15
case (30:99)
    discount = 0.2
case (100:299)
    discount = 0.25
case (300:)
    discount = 0.3
end select

print *,"The gorss cost is ￡", num*15.0
print *,"The discount is ￡", discount*num*15.0
print *,"The net cost is ￡", (1.0-discount)*num*15.0

end program PE0510