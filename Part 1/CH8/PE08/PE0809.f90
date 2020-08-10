program PE0809
implicit none

real :: open_balance
real,dimension(3) :: debit,credit
integer :: i 

print *,"Please input the open balance."
read '(F6.2)', open_balance

print *,"Please input the debits or credit."
do i=1,3
read '(2F5.2)',debit(i),credit(i)
end do 

print '("Opening balance:",10X, F6.2)',open_balance

print '("Transactions:")'
write (*,200)
200 format ("Debit",5X,"Credit",10X,"Total")

do i=1,3
if (debit(i) /= 0) then
    open_balance = open_balance-debit(i)
    print '(F5.2,21X,F6.2)',debit(i),open_balance
else if (credit(i) /= 0) then
    open_balance = open_balance+credit(i)
    print '(10X,F5.2,11X,F6.2)',credit(i),open_balance
end if 
end do

print *," "
print '("Closing balance:",10X,F6.2)',open_balance

end program PE0809