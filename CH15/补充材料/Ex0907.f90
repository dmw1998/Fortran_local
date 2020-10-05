program Ex0907
use typedef
implicit none
type(student) :: s
character(len=80) :: filename = "data.txt"
integer, parameter :: fileid = 10
logical :: alive
integer :: error, no

inquire(file=filename, exist=alive)

if ( .not. alive ) then
	write(*,*) trim(filename)," doesn't exist."
	stop
end if

open(fileid, file=filename)

do while(.true.)
	read(fileid,"(3X,I2,/,9XI4,9XI4,6XI4)",iostat=error) no,s
	if ( error/=0 ) exit
	write(*,'("ID: ",I2," Chinese: ",I3," English: ",I3," Math: ",I3)') no,s
end do

close(fileid)

end program Ex0907
