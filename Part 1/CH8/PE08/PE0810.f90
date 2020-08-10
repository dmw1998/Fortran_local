program PE0810
implicit none

real,dimension(13) :: theta, phi, resul
integer :: i,j 

theta = (/ (0.25*i,i=0,12) /)
phi = (/ (0.25*i,i=0,12) /)

print '(5X,13(F4.2,1X))',(phi(i),i=1,13)

do i=1,13
    resul = (/ (sin(theta(i))*sin(phi(j)) - (cos(theta(i)-phi(j))-cos(theta(i)+phi(j)))/2.0,j=1,13) /)
    print '(F4.2, 13(1X,F4.2))',theta(i),resul
enddo

end program PE0810