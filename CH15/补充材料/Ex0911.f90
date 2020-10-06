program Ex0911
implicit none

integer, parameter :: fileid = 10
character(len=20) :: fileidname = "Ex0911data.txt"
integer :: player, error
real :: hit

open(unit=fileid,file=fileidname,access="direct", &
     form="formatted", recl=6, status="replace")

do while (.true.)
    write (*,'("Which player?")')
    read (*,*) player

    if (player < 1 .or. player > 9) exit

    write (*,'("What percent of hit?")')
    read (*,*) hit

    write (fileid,fmt='(F5.2)',rec=player,iostat=error) hit
    if (error /= 0) exit
end do

close(fileid)

end program Ex0911