program examination_marks
implicit none

! This peogram prints statistics about a set of exam results

! Variable declarations
integer :: i, number, mark, &
          sum=0, maximum=huge(1),minimum=huge(1)        ! HUGE is a generic function we can use it to provide the largest possible integer value
real :: average

! Read number to read and process marks
print *,"How many marks are there?"
read *, number
print *,"Please type ",number,"marks: "

! Loop to read and process marks
do i = 1,number
    read *,mark
    ! On each pass, update sum,maximum and minimum
    sum = sum+mark
    IF (mark>maximum) maximum=mark   
    IF (mark<minimum) minimum=mark  
end do

! Calculate average mark and output results
average = real(sum)/number
print *,"Highest mark is ",maximum
print *,"Lowest mark is ",minimum
print *,"Average mark is ",average

end program examination_marks