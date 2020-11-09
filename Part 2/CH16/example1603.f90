module contact_database
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
    type contact_data
        character(len=name_len) :: first_name, last_name
        character(len=title_len) :: title
        character(len=sex_len) :: sex
        character(len=phone_len) :: phone
        character(len=street_len) :: street
        character(len=city_len) :: city
        character(len=state_len) :: state
        character(len=zip_len) :: zip
    end type contact_data

    ! Derived type for binary tree containing contacts
    type contact_tree
        type(contact_data) :: data
        type(contact_tree), pointer :: left, right
    end type contact_tree

contains

    recursive subroutine insert_contact(contact,database)
        ! This subroutine inserts a contact in the binary tree

        ! Dummy arguments
        type(contact_data) :: contact
        type(contact_tree), pointer :: database

        ! Check if (sub)tree is empty
        if (.not. associated(database)) then
            ! (sub)tree is empty, so insert contact at root
            allocate (database)
            database%data = contact
            nullify(database%left)
            nullify(database%right)

        ! Compare contact and the root of the (sub)tree
        else if ((contact%last_name < database%data%last_name)      &
            .or.((contact%last_name == database%data%last_name)     &
            .and.(contact%first_name < database%data%first_name)))  &
                                                                then
            ! Contact comes first, so insert it in the left branch
            call insert_contact(contact, database%left)

        else
            ! Insert contact in the right branch
            call insert_contact(contact,database%right)
        end if 
    end subroutine insert_contact

    recursive subroutine print_names(database)
        ! This subroutine prints the (sub)tree elements in order

        ! Dummy argument
        type(contact_tree), pointer :: database

        if (associated(database)) then
            call print_names(database%left)
            print '(5X,A,1X,A)', database%data%first_name,   &
                                database%data%last_name
            call print_names(database%right)
        end if
    end subroutine print_names

end module contact_database

program sort_contacts
use contact_database
implicit none

! This program sorts a lisst of contacts into alphabetic
! order and then prints the contacts in that order

! Declarations
type(contact_data) :: contact_details
type(contact_tree), pointer :: contacts      ! Database
integer :: ios

! Ensure that contact database tree in empty
nullify(contacts)

! Open data file
open(unit=1,file="contact data",status="old",action="read")

! Loop to read contact details and insert them in the tree
do 
    read(unit=1,fmt=*,iostat=ios) contact_details
    ! Test for the end of file
    if (ios < 0) then
        ! All data read and inserted
        close(1)
        exit
    else
        ! Insert this contact in the tree
        call insert_contact(contact_details,contacts)
    end if
end do

! All contacts now in database, so print names in order
call print_names(contacts)

end program sort_contacts