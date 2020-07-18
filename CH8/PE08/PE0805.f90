program PE0805
implicit none

integer :: n1,n2

print *,"Please input two 3-digit integer numbers"
read '(I3,1X,I3)', n1,n2

print '("The positive difference between ",I3," and ",I3," is ",I3)',n1,n2,abs(n1-n2)
print '("The product between ",I3," and ",I3," is ",I6)',n1,n2,n1*n2

end program PE0805