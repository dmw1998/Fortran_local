program PE0807
implicit none

integer,dimension(5),parameter :: Stano = (/1,2,3,4,5/)
real,dimension(5) :: arrival, dep 

integer :: i 

print *,"Please input the arrival time and departure time at each station."
do i=1,5
print '("Station no.",I1)',i 
read '(2F4.2)',arrival(i),dep(i)
end do  

print '(A11,3X,A7,3X,A9)',"Station no.","Arrival","Departure" 
write (*,100)
100 format (11('-'),3X,7('-'),3X,9('-'))

do i=1,5
print '(3X,I5,7X,F4.2,7X,F4.2)',Stano(i),arrival(i),dep(i)
end do 

end program PE0807