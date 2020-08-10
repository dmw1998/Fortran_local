program test1
implicit none

integer :: a,b
real :: c,d 

read 101,a,b,c,d 
print 200,a,b,b-a,c,d,c-d 

101 format(T6,I4,TL6,I4,TL6,F4.1,TL6,F4.2)
200 format(5X,I4," minus",I5," is",I5,TR4,F6.2, &
           " minus",F6.2," is",F8.3)

end program test1