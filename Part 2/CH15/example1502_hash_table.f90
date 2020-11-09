module file_control_hash_table
implicit none
private
public :: max_names,max_len,record_number,rec_len,  &
          master_file_name,master_file 

! This module contains the data and procedure required to 
! find the record_number for a specified chemical

integer,parameter :: max_names=5, max_len=20
character(len=max_len),dimension(max_names) :: chemical = &
        (/ "HCl   ","H2SO4 ","CuSO3 ","FeSO3 ","NaCl  " /) 
! Full list of chemicals in the same order as in the master file
character(len=20) :: master_file_name = "Chemical_supplies"
integer,parameter :: master_file = 7

! Note that the calculation of the record length is processor
! dependent; the following assume one unit per character
! and four units per real value
integer,parameter :: char_store=1, real_store=4
integer,parameter :: rec_len = char_store*(max_len+8) + &
                               real_store*4
                    
contains

integer function record_number(item)
    ! This function returns the index of item in the hash
    ! table chemical (in the module)

    ! Dummy argument
    character(len=max_len),intent(in) :: item

    ! Local variable
    integer :: i, start

    ! Calculate start point for table search
    start = mod(ichar(item(1:1))*ichar(item(2:2)),max_names)+1

    ! Search for item name in hash table
    do i = start,max_names
        if (chemical(i) == item) then
            ! Match made - return index
            record_number = i 
            return
        else if (chemical(i) == " ") then
            ! Empty cell means item is not in table - return 0
            record_number = 0
            return
        end if
    end do

    ! No match yet made, and all cells full

    ! Search remainder of table
    do i = 1,start-1
        if (chemical(i) == item) then
            ! Match made - return index
            record_number = i 
            return
        else if (chemical(i) == " ") then
            ! Empty cell means item is not in table - return 0
            record_number = 0
            return
        end if
    end do

    ! Complete table has been searched without either finding
    ! a match or finding an empty cell
    ! Return -1 to indicate that table is full
    record_number = -1

end function record_number

end module file_control_hash_table