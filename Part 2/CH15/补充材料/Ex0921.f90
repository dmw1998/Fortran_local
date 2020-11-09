module typedef0921
  type student
	integer :: Chinese, English, Math, Natural, Social
	integer :: total
  end type
end module

program Ex0921
use typedef0921
implicit none

integer, parameter :: fileid=10, students=20
character(len=80)  :: tempstr
type(student) :: s(students) 
type(student) :: total
integer :: i, num, error

open(fileid, file="grades.txt",status="old", iostat=error)
if ( error/=0 ) then
    write(*,*) "Open grades.txt fail."
    stop
end if

read(fileid, '(A80)') tempstr 
total=student(0,0,0,0,0,0)
do i=1,students
    read(fileid,*) num, s(i)%Chinese, s(i)%English, &
                    s(i)%Math, s(i)%Natural, s(i)%Social      

    s(i)%Total = s(i)%Chinese + s(i)%English + &
                    s(i)%Math + s(i)%Natural + s(i)%Social 

    total%Chinese = total%Chinese + s(i)%Chinese
    total%English = total%English + s(i)%English
    total%Math    = total%Math + s(i)%Math
    total%Natural = total%Natural + s(i)%Natural
    total%Social  = total%Social + s(i)%Social
    total%Total   = total%Total + s(i)%Total
end do

write(*,'(1X,7A9)') "座号","中文","英文","数学","自然","社会","总分"
do i=1,students
    write(*,'(7I7)') i, s(i)
end do

write(*,'(3X,A7,6F7.1)') "平均", &
    real(total%Chinese)/real(students),&
    real(total%English)/real(students),&
    real(total%Math)   /real(students),&
    real(total%Natural)/real(students),&
    real(total%Social) /real(students),&
    real(total%Total)  /real(students)

end program Ex0921