module typedef1601
implicit none

    type datalink
        integer :: number
        type(datalink), pointer :: next
    end type datalink

contains

    subroutine outputlist(list)
    implicit none

        type(datalink), pointer :: list, p

        p => list
        do while(associated(p))
            write(*,*) p%number
            p => p%next
        end do

    end subroutine outputlist

    subroutine delete_item(num,head)
    implicit none

        integer :: num
        type(datalink), pointer :: head, item, next

        item => head
        do while (associated(item))
            if (item%number == num) then
                next => item%next
                deallocate(item)
            else
                nullify(item%next)
                item => item%next
            end if
        end do

    end subroutine delete_item

    function SearchList(num,head)
    implicit none

        integer :: num
        type(datalink), pointer :: head, p
        type(datalink), pointer :: SearchList

        p => head
        nullify(SearchList)
        do while(associated(p))
            if (p%number == num) then
                SearchList => p
                return
            end if
            p => p%next
        end do

    end function SearchList

end module typedef1601


program PE1601
use typedef1601
implicit none

integer :: n, i, j, error, a, b
type(datalink), pointer :: p, prime, item
type(datalink), pointer :: prime_num, int_num

print *,"Please input a maximum integer n."
read *, n 

if (n <= 1) then
    print *,"There are no prime numbers."
end if

allocate(prime)
prime = datalink(2,null())
p => prime
do i = 3,n
    allocate(p%next, stat=error)
    if (error /= 0) then
        write(*,*) "Out of memory!"
        stop
    end if
    p%next = datalink(i,p)
    p => p%next
end do
call outputlist(prime)

i = 2
do
    prime_num => SearchList(i,prime)
    a = prime_num%number
    print *,"a is ", a
    print *,"n/a = ", n/a
    do j = 1,n/a
        b = (1+j)*a
        print *,"delete (1+j)*a = ", b
        call delete_item(b,prime)
        ! int_num => SearchList(b,prime)
        ! if (associated(int_num)) then
        !     b = int_num%number
        !     print *,"b is ", b
        !     if (mod(a,b) == 0) then
        !         call delete_item(b,prime)
        !         call outputlist(prime)
        !     end if
        ! end if
        call outputlist(prime)
    end do
    i = prime_num%next%number
end do

call outputlist(prime) 

end program PE1601

