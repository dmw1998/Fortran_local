program name
implicit none

real :: p, q
real,dimension(7) :: x
real(kind=selected_real_kind(12,30)) :: y, z
integer :: record_length

inquire (iolength=record_length) p,q,x,y,z
print *,"The record length is ",record_length

end program name