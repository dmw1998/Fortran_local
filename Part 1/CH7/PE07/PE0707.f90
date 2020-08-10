program PE0707
implicit none

real,dimension(1000:1999) :: marks
integer :: index,i 
real :: ave,std 

do i=1,size(marks)
    print *,"Please input the identifying number and the marks of the student."
    read *, index, marks(index)
end do 

ave = sum(marks)/size(marks)

print *,"The average is",ave

std = 1.3*ave
print *, std

print *,"Students obtianing distinction and their marks:"

do index = 1000,1999
    if (marks(index) >= std) then
        print *, index, marks(index)
    end if 
end do 

end program PE0707