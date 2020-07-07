module time_form
    implicit none
    save
    type time
    integer :: h,min,sec
    end type time
end module time_form


function change_time_form(s)
    use time_form
    implicit none
    integer :: s
    type(time) :: change_time_form

    change_time_form%min = s/60
    change_time_form%sec = s-change_time_form%min*60
    change_time_form%h = change_time_form%min/60
    change_time_form%min = change_time_form%min-change_time_form%h*60
    
end function change_time_form

program PE0411
use time_form
implicit none

type(time), external :: change_time_form

integer :: s
type(time) :: times

print *,"Please input the time interval in seconds."
read *,s

times = change_time_form(s)

print *,times%h,"hours, ",times%min,"minutes and ",times%sec,"seconds."

end program PE0411