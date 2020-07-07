character(len=*) function full_name(first_name,last_name)
implicit none

! Function to join two names to form a ful name with a
! single space between the first and last names

! Dummy argument declarations
character(len=*), intent(in) :: first_name,last_name

! Use ADJUSTL to remove redundant leading blanks, and TRIM
! to remove redundant blanks at the end of first_name
full_name = trim(ADJUSTL(first_name)) // " " //  &
            ADJUSTL(last_name)

! We can also give two local variables: adjustl(first_name) and adjustl(last_name)

end function full_name