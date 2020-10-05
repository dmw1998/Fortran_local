module typedef
implicit none

type student
  integer :: Chinese, English, Math
end type

end module typedef

program Ex0916
use typedef
implicit none

integer :: students, i 
type(student), allocatable :: s(:)
character(len=80) :: filename = "data.txt"
integer, parameter :: fileid = 10

write (*,*) "How many students are in the class?"
read (*,*) students
allocate(s(students), stat=i)
if (i /= 0) then
	write (*,*) "Allocate buffer fail."
	stop
end if

open (fileid, file=filename)

do i=1,students
	write (*,'("Please input the Chinese, English and Math&
            	& grades of the student ",I2)') i
	read (*,*) s(i)%Chinese, s(i)%English, s(i)%Math
	write (fileid,'("ID:",I2,/" Chinese:",I4," English:",I4," Math:",I4)') &
			i, s(i)
end do

close(fileid)

end program Ex0916
