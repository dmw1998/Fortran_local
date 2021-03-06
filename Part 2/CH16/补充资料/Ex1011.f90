module typedef1011
  implicit none

  type datalink
    integer :: i
    type(datalink), pointer :: next
  end type datalink

end module typedef1011

program Ex1011
use typedef1011
implicit none

type(datalink), target  :: node1,node2,node3
type(datalink), pointer :: p
integer :: i

p=>node1
node1%i=1
node1%next=>node2
node2%i=2
node2%next=>node3
node3%i=3
nullify(node3%next)

do while(.true.) 
    write(*,*) p%i
    if ( .not. associated(p%next) ) exit
    p => p%next 
end do

end program
