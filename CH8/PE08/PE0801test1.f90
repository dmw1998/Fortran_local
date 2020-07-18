program unit_test_1
implicit none

integer,parameter :: in=5, out=6

integer :: num1,num2

write (unit=*,fmt=*) "Please type a 4 digit integer "
read (unit=*,fmt=*) num1
write (unit=out,fmt=*) "Please type a 3 digit integer "
read (unit=in,fmt=*) num2
write (unit=out,fmt=*) "The numbers you tyoed were &
                        &as follows"
print '(" ",I4," and",I4)', num1,num2

end program unit_test_1