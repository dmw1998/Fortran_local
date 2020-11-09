module data_type
implicit none

! Derived type to record transaction data

type request
    integer :: machine
    integer :: customer
    character(len=8) :: date
    character(len=4) :: time
    real :: amount
    type(request), pointer :: next
end type request

end module data_type

module linked_list
use data_type
implicit none

! This module contains the procedures to manipulate the linked
! list representing the outstanding transaction requests

contains
    subroutine init(head,tail)
        ! Initialize the empty list

        ! Dummy arguments
        Type(request),pointer :: head, tail

        nullify(head,tail)      ! No successor
    end subroutine init

    subroutine add(new,head,tail)
        ! Add a new item to the end of the list

        ! Dummy arguments
        type(request), pointer :: new, head, tail

        ! Check to see if list is empty
        if (associated(head)) then
            ! List is not empty
            tail%next => new        ! Attach new request
            nullify(new%next)       ! At end of list
            tail => new             ! Reset tail pointer
        else
            ! List is empty
            head => new             ! Start up list with new
            tail => new
            nullify(tail%next)      ! No successor
        end if
    end subroutine add

    subroutine delete(head,tail,first)
        ! Return a pointer to the first item in the linked
        ! list, and remove it from the list

        ! Dummy arguments
        type(request), pointer :: head, tail, first

        ! Check to see if list is empty
        if (associated(head)) then
            ! List in not emty
            ! Check if more than one item in the list
            if (associated(head%next)) then
                ! More than one item in the list
                first => head       ! Return pointer to first item
                head => head%next   ! Remoove item from list
            else
                ! Only one item in the list
                first => head       ! Return pointer to first item
                nullify(head,tail)  ! List is now empty
            end if
        
        else
            ! List is empty
            nullify(first)          ! Return no element
        end if
    end subroutine delete

    subroutine list(head)
        ! List the contents of the list

        ! Dummy argument
        type(request), pointer :: head

        ! Local variable
        type(request), pointer :: ptr 

        print *, " "
        print *,"Pending Request List"

        ! Check whether list is empty
        if (.not. associated(head)) then
            ! List is empty - print message
            print *,"List is Empty"
        else
            ! List contains at least one item
            ! Set local pointer to head of list
            ptr => head

            ! Loop to point all items in the list
            do
                ! Print details of this request item
                print *, ptr%machine, ptr%customer,ptr%date,    &
                         ptr%time,ptr%amount

                ! Set pointer to next item
                ptr => ptr%next
                ! Exit loop if there are no more items in the list
                if (.not. associated(ptr)) exit 
            end do
        end if 
    end subroutine list

end module linked_list

program bank
use linked_list
implicit none
! This program simulates the operation of the cash machines

! Declarations
integer :: i=1, j=1, m
real :: x=100.0
type(request), pointer :: head, tail
type(request), pointer :: item, first

! Initialize empty list
call init(head,tail)

! Loop to add four items to the list
do m = 1,4
    ! Create a transaction request
    call make(i,j,x,item)

    ! Add it to the list
    call add(item,head,tail)

    ! Print the current state of the list
    call list(head)
end do

! Loop to remove si item from the list
do m = 1,6
    ! Remove item from head of list
    call delete(head,tail,first)

    ! Check to see if any item was removed
    if (associated(first)) then
        ! An item was removed - print it
        print *," "
        print *,"Request to be processed is:"
        print *,first%machine,first%customer,first%date,    &
                first%time,first%amount
    end if

    ! Print items remaining in list
    call list(head)
end do

contains

    subroutine make(i,j,x,item)
        ! Subroutine for simulating input requests

        ! Dummy variables
        integer, intent(inout) :: i,j
        real, intent(inout) :: x
        type(request), pointer :: item

        ! Local variable
        integer :: err

        ! Create a new transaction record
        allocate(item, stat=err)
        ! Check that it was created successfully
        if (err /= 0) then
            ! Print error message and terminate processing
            print *,"Machine out of memory"
            stop
        end if

        ! Assign a value to each field of the new record
        item%machine = i 
        item%customer = j
        item%date = "06091993"
        item%time = "1215"
        item%amount = x
        i = i+1
        j = j+2
        x = x+10
    end subroutine make

end program bank