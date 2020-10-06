module storage
    implicit none

    ! Field lengths for contact data
    integer,parameter :: name_len = 15
    integer,parameter :: title_len = 20
    integer,parameter :: sex_len = 1
    integer,parameter :: phone_len = 20
    integer,parameter :: street_len = 40
    integer,parameter :: city_len = 20
    integer,parameter :: state_len = 20
    integer,parameter :: zip_len = 10

    ! Derived type for contact data
    type contact
        character(len=name_len) :: first_name, last_name
        character(len=title_len) :: title
        character(len=sex_len) :: sex
        character(len=phone_len) :: phone
        character(len=street_len) :: street
        character(len=city_len) :: city
        character(len=state_len) :: state
        character(len=zip_len) :: zip
    end type contact

    ! Derived type to create an array of pointers to oobjects
    ! of type contact
    type contact_pointer
        type(contact), pointer :: pointer_to_contact
    end type contact_pointer

    ! Golbal data
    integer :: n            ! Number of data records

    ! Array of contacts
    type(contact), allocatable, dimension(:), &
        target,save :: contacts

    ! Array of pointers to array of contacts
    type(contact_pointer), allocatable, dimension(:), &
        save :: p_contacts

end module storage

program sort_contants
use storage
implicit none
! This program sorts a list of contacts into alphabetic
! order and then prints the contacts in that order

! Declaration
integer :: error

! Open data file
open(unit=1,file="contact data",status="old",action="read")

! Read number of data records
allocate (contacts(n),p_contacts(n),stat=error)
if (error /= 0) then
    print *,"Allocation error"
    stop
end if

! Read all contact data
read (unit=1,fmt=*) contacts

close (unit=1)

! Sort data into order
call sort

! Print sorted list
call display

! Dellocate arrays before ending
deallocate(contacts,p_contacts,stat=error)
if (error /= 0) then
    print *,"Error deallocating contacts and p_contacts"
end if

end program sort_contants

subroutine sort
    use storage
    implicit none
    ! This subroutine sorts the array p_contacts based on
    ! the alphabetic order of the last_name field of the array
    ! contacts using an injection sort

    ! Local variables
    integer :: i, j             ! Loop control variables

    ! Initialize pointer list
    p_contacts(1)%pointer_to_contact => contacts(1)

    ! Main sorting loop
    main: do i = 2,n

        ! Check current contact against contacts in list so far
        do j = 1,i-1
            if (contacts(i)%last_name < &
                p_contacts(j)%pointer_to_contact%last_name) then

                ! Shift last part of p_contacts array down
                p_contacts(j+1:i) = p_contacts(j:i+1)
                !Insert current contact in list
                p_contacts(j)%pointer_to_contact => contacts(i)

                ! Return to find position for next contact
                cycle main
            end if
        end do

        ! Current contact comes after all items already in list
        ! Insert it at the end
        p_contacts(i)%pointer_to_contact => contacts(i)
    end do main

end subroutine sort

subroutine display
    use storage
    implicit none
    ! This subroutine prints the names of people in the contact
    ! list sorted by their last names

    ! Local variable
    integer :: i            ! Loop control variable

    ! Print alphabetical list of last names
    do i = 1,n
        print '(5X,A,1X,A)',                                &
              p_contacts(i)%pointer_to_contact%first_name,  &
              p_contacts(i)%pointer_to_contact%last_name
    end do

end subroutine display