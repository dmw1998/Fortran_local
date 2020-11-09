program namelist_output
implicit none

integer :: a=987, b=-123
integer,dimension(3:5) :: c=(/3,4,5/)
real :: x=6.54, y=0.00009876
complex :: q=(4.5,6.7)
character(len=22) :: s="The cat sat on the mat"
namelist /xyz/a,b,c,x,y,q,s 

write (unit=*,NML=xyz)

end program namelist_output