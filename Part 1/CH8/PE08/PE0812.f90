program PE0812
implicit none

real,dimension(5) :: A = (/ 20.6,31.2,10.9,15.4,12.1 /)
real,dimension(5) :: B = (/ 16.9,20.2,30.7,30.2,30.0 /)
real,dimension(5) :: CC = (/ 90.6,100.2,98.7,117.2,88.6 /),sqrm
real,dimension(3) :: mean,std_dev
real :: mua,mub,muc
integer :: i,j

mua = sum(A)/5.0
mub = sum(B)/5.0
muc = sum(CC)/5.0

mean = (/ mua, mub ,muc /)

do i=1,5
    sqrm(i) = (A(i)-mua)**2
enddo
std_dev(1) = sqrt(1.0/5.0*sum(sqrm))

do i=1,5
    sqrm(i) = (B(i)-mub)**2
enddo
std_dev(2) = sqrt(1.0/5.0*sum(sqrm))

do i=1,5
    sqrm(i) = (CC(i)-muc)**2
enddo
std_dev(3) = sqrt(1.0/5.0*sum(sqrm))

print '(19X,"Reaction A",2X,"Reaction B",2X,"Reaction C")'
do i = 1,5
    print '(18X,F5.1,7X,F5.1,7X,F6.1)',A(i),B(i),CC(i)
enddo
print '(14X,"Mean:",F5.2,7X,F5.2,8X,F5.2)',(mean(j),j=1,3)
print '("Standard deviation:",F5.2,7X,F5.2,8X,F5.2)',(std_dev(j),j=1,3)

end program PE0812