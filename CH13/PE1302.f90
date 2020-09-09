program PE1302
implicit none

integer,dimension(2,34) :: table_Weekends
integer,dimension(2,44) :: table_Workdays
integer :: i,j,hour

print *, "The time table for Sundays and Saturdays"

table_Weekends(1,1) = 7
table_Weekends(2,1) = 30
hour = 8
do i = 2,34,2
    do j = 0,1
        table_Weekends(1,i+j) = hour
        table_Weekends(2,i+j) = 30*j
    end do
    hour = hour + 1
end do

print '(5X,I2.2,":",I2.2)', table_Weekends

print *, "The time table for other days"

table_Workdays(1,44) = 23
table_Workdays(2,44) = 0
hour = 7
do i = 1,33,3
    do j = 0,2
        table_Workdays(1,i+j) = hour
        table_Workdays(2,i+j) = 20*j
    end do
    hour = hour + 1
end do

do i = 34,43,2
    do j = 0,1
        table_Workdays(1,i+j) = hour
        table_Workdays(2,i+j) = 30*j
    end do
    hour = hour + 1
end do 

print'(5X,I2.2,":",I2.2)', table_Workdays

end program PE1302