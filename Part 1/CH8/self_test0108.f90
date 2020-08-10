program test2
implicit none

character(len=6) :: a,b,c 

read '(A8,T1,A4,T1,A)', a,b,c 
print '(5X,A8,5X,A4,5X,A)', a,b,c 
print '(5X,A,5X,A,5X,A)', a,b,c 

end program test2