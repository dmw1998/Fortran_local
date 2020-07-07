program PE0308
implicit none

real, parameter :: alpples = 675.0/4.0, butter = 75.0/4.0, sugar = 150.0/4.0, breadcrumbs = 100.0/4.0, cream = 150.0/4.0
real :: apl, btr, sgr, bdcb, crm
integer :: number
real :: n

print *,"Please input the number of people coming to dinner"
read *, number

n = number
apl = n*alpples
btr = n*butter
sgr = n*sugar
bdcb = n*breadcrumbs
crm = n*cream

print *, apl," g of apples"
print *, btr," g of butter"
print *, sgr," g of sugar"
print *, bdcb," g of breadcrumbs"
print *, crm," ml of cream"

end program PE0308