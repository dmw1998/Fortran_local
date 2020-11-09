module typedef1010
  implicit none

  type datalink
    integer :: i
    type(datalink), pointer :: next
  end type datalink

end module typedef1010

program Ex1010
use typedef1010
implicit none

type(datalink) , target  :: node1,node2,node3
integer :: i

node1%i=1
node1%next=>node2
node2%i=2
node2%next=>node3
node3%i=3
nullify(node3%next)

write(*,*) node1%i
write(*,*) node1%next%i      
write(*,*) node1%next%next%i 

end program Ex1010
