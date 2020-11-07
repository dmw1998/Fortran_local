module DefForTree
    implicit none

    type data 
        integer :: n            ! Put data
        integer :: repeat       ! Times of repeating
        type(data), pointer :: left     ! Left branch
        type(data), pointer :: right    ! Right branch
    end type data

end module DefForTree

module bin_tree
    use DefForTree
    implicit none
    private
    type(data), pointer :: tree, action
    integer, save :: numbers = 0
    public :: add, TraceTree
    
contains

subroutine add(n)

    integer, intent(in) :: n 

    integer :: error
    type(data), pointer :: new 
    integer :: level = 1 

    numbers = numbers+1

    print '("Get: ",I4," numbers")', numbers

    allocate(new,stat=error)

    if (error /= 0) then
        print *,"Out of memory!"
        stop
    end if

    write(*,'("root ")', advance="no")

    new%repeat = 1
    new%n = n 
    nullify(new%right, new%left)

    if (numbers == 1) then
        action => new 
        tree => new 
        write(*,'(": new")')
        return
    end if

    action => tree

    do 
        level = level+1

        if (n > action%n) then
            if (associated(action%right)) then
                action => action%right 
                write(*,'("-> R")', advance="no")
            else
                action%right => new 
                action => new 
                write(*,'("-> R: new")')
                exit
            end if

        else if (n < action%n) then
            if (associated(action%left)) then
                action => action%left 
                write(*,'("-> L")', advance="no")
            else
                action%left => new 
                action => new 
                write(*,'("-> L: new")')
                exit
            end if

        else if (n == action%n) then
            action%repeat = action%repeat+1
            deallocate(new)
            write(*,'(": Repeat")')
            return
        end if
    end do

end subroutine add

subroutine TraceTree()

    call show_tree(tree)

end subroutine TraceTree

recursive subroutine show_tree(tree)

    type(data), pointer :: tree 

    if (associated(tree)) then
        call show_tree(tree%left)
        call show_data(tree)
        call show_tree(tree%right)
    end if

end subroutine show_tree

subroutine show_data(tree)

    type(data), pointer :: tree 

    integer :: i 

    do i = 1,tree%repeat
        write(*,*) tree%n 
    end do

end subroutine show_data
    
end module bin_tree

program main
    use bin_tree
    implicit none

    integer :: num 

    do
        print *,"Please input an integer."
        print *,"Input 0 to exit."
        read *, num 

        if (num == 0) exit

        call add(num)
    end do

    call TraceTree()
    
end program main