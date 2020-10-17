module NewGraphLib
use TextGraphLib
implicit none

contains

    subroutine OutputToFile(filename)
    implicit none

        character(len=*), intent(in) :: filename
        character(len=10) :: fmt="(xxxA)"
        integer :: i

        if ( .not. allocated(screen) ) return

        open(10,file=filename,status="replace")

        write(fmt(2:4), "(I3.3)") ScreenWidth

        do i=1, ScreenHeight
            write(10, fmt) screen(i,:)
        end do

        close(10)

    end subroutine OutputToFile

end module NewGraphLib

program main
use NewGraphLib
implicit none

call SetScreen(20,20)
call ClearScreen()
call DrawCircle(10,10,8)
call DrawLine(14,6,6,14)
call UpdateScreen()
call OutputToFile("test.txt")

end program
