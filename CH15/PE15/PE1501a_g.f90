program PE1501g
implicit none

real,dimension(0:40) :: x, y
integer :: i 

open(2, file = 'PE1501.txt', status = 'old')

do i = 0,40
    read(2,*) x(i), y(i)
end do

close(2)

print *, "(a)"
print '(F4.1,5X,F16.6)', (x(i), y(i), i = 0,40)
print *, "(b)"
print '(F4.1,5X,E16.6)', (x(i), y(i), i = 0,40)
print *, "(c)"
print '(E9.3,5X,E16.6)', (x(i), y(i), i = 0,40)
print *, "(d)"
print '(F4.1,5X,ES14.6)', (x(i), y(i), i = 0,40)
print *, "(e)"
print '(F4.1,5X,EN16.6)', (x(i), y(i), i = 0,40)
print *, "(f)"
print '(G8.3,5X,G16.6)', (x(i), y(i), i = 0,40)
print *, "(g)"
print '(F4.1,5X,G16.6)', (x(i), y(i), i = 0,40)

end program PE1501g