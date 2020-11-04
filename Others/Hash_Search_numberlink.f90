module numberlink
    implicit none

    integer, parameter :: n = 10

    type link
        integer :: num 
        type(link), pointer :: next
    end type link

    type(link), target :: linking(n)        ! Store hash table
    type(link), pointer :: proc             ! temporary pointer

    integer, dimension(n) :: array = &
        (/ 21, 53, 71, 19, 61, 81, 3, 17, 44, 93 /)
    
contains

subroutine InitialLink()

    integer :: i 

    do i = 1,n 
        linking(i) = link(0,null())
    end do

end subroutine InitialLink

integer function hash(key)

    integer :: key

    hash = key/10 + 1

    return

end function hash 

subroutine insert(data,pos)

    integer :: data, pos        ! data number and its position
    integer :: index            ! Result after hashing

    index = hash(data)
    proc => linking(index)

    proc%num = pos

    allocate(proc%next)

    proc => proc%next

    proc%num = 0
    nullify(proc%next)

end subroutine insert

subroutine hash_search(key)

    integer :: key              ! data want to search
    integer :: index            ! Result after hashing

    index = hash(key)

    proc => linking(index)      ! Let proc point to the position of hash(index) in the link

    do
        if (proc%num == 0) then
            print *, "Not found."
            return
        end if
        if (array(proc%num) == key) then
            print *, proc%num, " ", key
            return
        end if
        if (associated(proc%next)) proc => proc%next
    end do
    return

end subroutine hash_search

subroutine outputlink()

    integer :: i 

    do i = 1,n 
        proc => linking(i)
        write (*,'(1X,I2,":")', advance="NO") i 
        do while (associated(proc%next))
            write(*,'("=>",I2)', advance="NO") array(proc%num)
            proc => proc%next
        end do
        write (*,*)
    end do

end subroutine outputlink
    
end module numberlink

program hash_searching
    use numberlink
    implicit none

    integer :: key, i 

    call InitialLink()

    do i = 1,n 
        call insert(array(i),i)
    end do

    print *, "Which data do you want to find?"
    read *, key

    call hash_search(key)
    
end program hash_searching