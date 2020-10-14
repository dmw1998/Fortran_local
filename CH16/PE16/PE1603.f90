module typedef1603
implicit none

type datalink
    character(len=1) :: char
    character(len=20) :: word
    type(datalink), pointer :: prev 
    type(datalink), pointer :: next
end type datalink

contains

    subroutine delitem(item)

        type(datalink), pointer :: item
        type(datalink), pointer :: prev, next

        prev=>item%prev
        next=>item%next
        deallocate(item)
        if (associated(prev)) prev%next=>next
        if (associated(next)) next%prev=>prev
        item=>next

    end subroutine delitem

    subroutine build_link(str, head)

        character(len=*) :: str
        character(len=1) :: char_temp
        type(datalink), pointer :: p, head
        integer :: n, i, error

        str = trim(str)
        n = len(str)

        allocate(head)
        read (str,'(A1)') head%char
        read (str,'(A1)') head%word
        p => head
        do i = 2,n
            allocate(p%next, stat=error)
            if (error /= 0) then
                write(*,*) "Out of memory!"
                stop
            end if
            read (str,'(A1)') p%next%char
            read (str,'(A1)') p%next%word
            p => p%next
        end do
        
    end subroutine build_link

    subroutine read_words(head)

        type(datalink), pointer :: head, p1, p2

        p1 => head%next
        p2 => head
        do while(associated(p1) .and. associated(p2))
            if (p1%char /= " ") then
                p2%word = p2%word//p1%char
                p2%next => p1%next
                p1 => p1%next
            else
                p2%next => p1%next
                p1 => p1%next
            end if
        end do

    end subroutine read_words

    subroutine outputlist(list)
    implicit none

        type(datalink), pointer :: list, p

        p=>list
        do while( associated(p))
            write(*,*) p%word
            p=>p%next
        end do

    end subroutine outputlist

end module typedef1603

program PE1603
use typedef1603
implicit none

character(len=200) :: sentence
type(datalink),pointer :: words

print *, "Please input a sentence."
read *, sentence

call build_link(sentence,words)

call read_words(words)

call outputlist(words)

end program PE1603