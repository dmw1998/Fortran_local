program PE1302
implicit none

character(25),dimension(7,2) :: table

table(1,1) = 'Monday'
table(2,1) = 'Tuesday'
table(3,1) = 'Wednesday'
table(4,1) = 'Thursday'
table(5,1) = 'Friday'
table(6,1) = 'Saturday'
table(7,1) = 'Sunday'
table(1,2) = '0700,1800,20,1800,300,30'
table(2,2) = '0700,1800,20,1800,300,30'
table(3,2) = '0700,1800,20,1800,300,30'
table(4,2) = '0700,1800,20,1800,300,30'
table(5,2) = '0700,1800,20,1800,300,30'
table(6,2) = '0730,2400,20'
table(7,2) = '0730,2400,20'

print '(7(A28))', table

end program PE1302