program space_pointer
implicit none

integer,dimension(:),allocatable :: a 
real,dimension(:,:),pointer :: p 
integer :: alloc_error, dealloc_error
integer :: i            ! Loop control variable
integer :: n            ! Size of diagonal

! Read input data
open (unit=1,file="diagonal",status="old",action="read")
read (unit=1,fmt=*) n           ! Size of diagonal
allocate(a(n),stat=alloc_error)
if (alloc_error /= 0) then
    print *, "Couldn't allocate space for a"
    stop
end if
read (unit=1,fmt=*) a 

! Allocate space for p.
allocate(p(size(a,1),size(a,1)),stat=alloc_error)
if (alloc_error /= 0) then
    print *, "Couldn't allocate space for p"
    stop
end if

! Space for p allocated
p = 0.0                     ! Set elements of p to zero
do i=1,size(a,1)            ! Set diagonal of p to the elements of a
    p(i,i) = a(i)
end do

! Calculate using p

! Deallocate a and p.
deallocate(a,p,stat=dealloc_error)
if (dealloc_error /= 0) then
    print *, "Couldn't deallocate space for a and p"
    stop
end if

! Other calculations


end program space_pointer