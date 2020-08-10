module Global_Data
implicit none
save

type person
character(len=15) :: first_name
character(len=20) :: last_name
character :: sex    ! M or F
integer :: marital_status, Age
real :: height, weight
end type person

end module Global_Data


subroutine input(people, number_people)
use Global_Data
implicit none

! An input subroutine for a survey

! Dummy arguments
type(person),dimension(:) :: people
integer :: number_people

! Local variables
integer :: i,max_people

! Store maximum number of allowable data sets
max_people = size(people)

! Display data format
print *,"Type data as follows:"
print *,"Cols.  1-15  First name"
print *,"Cols. 21-40  Last name"
print *,"         43  Sex (F=female, M=male)"
print *,"         45  Marital status (0=single,1=married,"
print *,"             2=widowed, 3=divorced, 4=cohabiting,"
print *,"             9=unknown)"
print *,"Cols. 47,48  Age (in yeas)"
print *,"Cols. 51-53  Height (in cm)"
print *,"Cols. 56-62  Weight (in kg in the form kkk.ggg)"
print *," "
print *,"Data should be terminated be the words &
        &'END OF DATA' typed in cols. 1-11"
print *," "

! Loop to read data
do i=1,max_people
    read '(A15,5X,A20,2X,I1,1X,I1,1X,I2,2X,F3.2,2X,F7.3)', &
         people(i)

    ! Check if this is the terminator record
    if (people(i)%first_name(1:11) == "END OF DATA") exit

end do

! Check to see if a terminator was found
if (i>max_people) then
    print *,"Maximum number of records (",max_people,") read"
    print *,"with no terminator - input halted"
    ! Sace number of data records read
    number_people = max_people
else
    number_people = i-1
end if

end subroutine input