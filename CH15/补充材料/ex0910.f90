program Ex0910
  implicit none
  integer, parameter :: fileid = 10
  character(len=20)  :: filename = "list.txt"
  integer :: player
  real :: hit
  integer :: error
  logical :: alive
  
  inquire(file=filename, exist=alive)
  if ( .not. alive ) then
    write(*,*) trim(filename)," doesn't exist."
	stop
  end if

  open(unit=fileid, file=filename, access="direct",&
       form="formatted", recl=6, status="old")

  do while(.true.)
    write(*,"('Which time of hit do you want to know?')")
	  read (*,*) player
	  read(fileid, fmt="(F4.2)", rec=player, IOSTAT=error) hit
	  if (error /= 0) exit
	  write(*,"('The hit percent is 'F4.2)") hit
  end do

  close(fileid)

end program Ex0910