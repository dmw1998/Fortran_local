program ex0913
implicit none
integer, parameter :: fileid = 10
character(len=20)  :: filename = "list.bin"
real :: hit
integer :: player, error
logical :: alive

inquire(file=filename, exist=alive)
if ( .not. alive ) then
    write(*,*) trim(filename)," doesn't exist."
    stop
end if

open(unit=fileid, file=filename, form="unformatted",&
    access="direct", recl=4, status="old")

do while(.true.)
    write(*,'("Which player?")')
    read (*,*) player
    read(fileid, rec=player, iostat=error) hit
    if ( error/=0 ) exit
    write(*,'("The percent of hit:",F5.2)') hit
end do

close(fileid)
  
end program