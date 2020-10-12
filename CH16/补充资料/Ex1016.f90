module linklist1016

    type student
        integer :: num
        integer :: Chinese, English, Math, Science, Social
    end type student

    type datalink
        type(student) :: item
        type(datalink), pointer :: next
    end type datalink

contains

    function SearchList(num, head)
    implicit none

        integer :: num
        type(datalink), pointer :: head, p
        type(datalink), pointer :: SearchList

        p=>head
        nullify(SearchList)
        do while(associated(p))
            if (p%item%num==num) then
                SearchList => p
                return
            end if
            p=>p%next
        end do

    end function

end module linklist1016

program Ex1016
use linklist1016
implicit none

character(len=20) :: filename
character(len=80) :: tempstr
type(datalink), pointer :: head
type(datalink), pointer :: p
integer :: i,error,size

write(*,*) "filename:"
read(*,*) filename
open(10, file=filename, status="old", iostat=error)
if (error /= 0) then
    write(*,*) "Open file fail!"
    stop
end if

allocate(head)
nullify(head%next)
p => head
size = 0
read(10, '(A80)') tempstr 
do while(.true.)
    read(10,fmt=*, iostat=error) p%item
    if (error /= 0) exit
    size = size+1
    allocate(p%next, stat=error)
    if (error /= 0) then
        write(*,*) "Out of memory!"
        stop
    end if
    p => p%next
    nullify(p%next)
end do
write(*,'("Total: ",I3," student(s)")') size

do while(.true.)
    write(*,*) "Which student's grades do you want to know?"
    read (*,*) i
    if (i<1 .or. i>size) exit
    p => SearchList(i,head)
    if (associated(p)) then
        write(*,'(4(A,I3,", "),A,I3)') "Chinese",p%item%Chinese,&
                            "English",p%item%English,&
                            "Math",p%item%Math,&
                            "Science",p%item%Science,&
                            "Social",p%item%Social
    else
        exit
    end if
end do
write(*,'("The No.",I3," student does not exit. The program is end.")') i

end program Ex1016