module file_control
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

! integer,parameter :: char_store=1, real_store=4
! integer,parameter :: rec_len = char_store*(max_len+8) + &
!                                real_store*4

! A better approach, using the INQUIRE statement, would be to replace
! upper two lines by
integer :: rec_len
                    
contains

integer function record_number(item)
    ! This function returns the index of item in the look-up
    ! table chemical (in the module)

    ! Dummy argument
    character(len=max_len),intent(in) :: item

    ! Local variable
    integer :: i 

    ! Search for item name in look-up table
    do i = 1,max_names
        if (item == chemical(i)) exit
    end do

    ! Check to see if a match was found
    if (i > max_names) then
        ! No match made - return zero
        record_number = 0
    else
        record_number = i 
    end if

end function record_number

end module file_control

program stock_control
use file_control
implicit none

! This program updates a stock control file of chemicals
! and produce a list of order requests

! Fields in the master file records
character(len=max_len) :: name 
real :: current_stock, re_order, unit_order, unit_stock,max_stock
character(len=8) :: units

! Other variables
real :: amount, quantity
integer :: i, ios, record, num_units

! Add a line for refined module
inquire (iolength=rec_len) name, current_stock, units,      &
                            re_order,unit_stock,max_stock

! Open master file
open (unit = master_file, file = master_file_name,      &
      access = "direct", recl = rec_len, iostat = ios)
if (ios /= 0) then
    ! Error during opening master file
    print *,"Error during opening of master file"
    print *,"Please check file and try again"
    stop
end if

! Update master file
print *,"Type stock issues as chemical followed by quantity"
print *,"issued (negative) or received (positive)"
print *,"To end data type the word UPDATE"
do
    read *, name, amount
    ! Get record number for this chemical
    record = record_number(name)

    ! Check if this was a valid name
    if (record > 0) then
        ! Update master file
        read (unit = master_file, rec = record, iostat = ios) name, &
              current_stock, units, re_order,unit_stock, max_stock
        if (ios /= 0) then
            ! Error during reading - ignore this data
            print *,"Error during reading this chemial's &
                    &record in the master file."
            print *,"The record is unchanged"

        else
            ! Update current stock and rewrte record
            current_stock = current_stock + amount
            write (unit = master_file, rec = record, iostat = ios)  &
                   name, current_stock, units, re_order,            &
                   unit_stock,max_stock
            if (ios /= 0) then 
                ! Error during writing
                print *,"Error during updating this chemical's  &
                        &record in the master file."
                print *,"The record may now be incorrect!"
            end if
        end if
    
    else
        ! Invalid chemical name - check if end of data
        if (name == "UPDATE") then
            exit
        else
            ! Unknown chemical - print error message
            print *, name, " is not in the master file."
            print *,"This line of data has been ignored!"
        end if
    end if

    ! All updating now carried out - search for chemicals
    ! in need of recordering
    print '(" ","The following items need recording:"/  &
            "0","Chemical",T30,"Quantity"//)'
    
    do i = 1,max_names
        read (unit = master_file, rec = record, iostat = ios) name,     &
              current_stock, units, re_order,unit_stock, max_stock
        if (ios /= 0) then
            ! Error during reading - ignore this data
            print *,"Error during checking this chemical's &
                    & record in the master file."
            print *,"No check for recordering made!"
        
        else
            ! Check if stocks are below recorder level
            if (current_stock < re_order) then
                ! More stock required - calculate order quatity
                quantity = max_stock - current_stock
                num_units = int(quantity/unit_order)
                quantity = num_units*unit_order
                print '(" ",A,",T30,F8.0," ",A," (in units of ", &
                        I8,1X,A3,")")',name, quantity, units, unit_order, units
                ! An error occurs here, cannot be fixed.
            end if
        end if
    end do

end do

end program stock_control