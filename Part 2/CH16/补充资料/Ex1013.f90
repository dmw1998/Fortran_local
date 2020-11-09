module typedef1013
implicit none

    type datalink
        integer :: i
        type(datalink), pointer :: prev 
        type(datalink), pointer :: next 
    end type datalink

end module typedef1013

program Ex1013
use typedef1013
implicit none

type(datalink), target  :: node1,node2,node3
type(datalink), pointer :: p
integer :: i

node1 = datalink(1, null(), node2)
node2 = datalink(2, node1 , node3)
node3 = datalink(3, node2 , null())

write(*,*) "In order"
p=>node1
do while(.true.)
    write(*,*) p%i
    if (.not. associated(p%next)) exit
    p=>p%next
end do  

write(*,*) "Reverse oreder"
p=>node3
do while(.true.)
    write(*,*) p%i
    if (.not. associated(p%prev)) exit
    p=>p%prev
end do  

end program Ex1013
