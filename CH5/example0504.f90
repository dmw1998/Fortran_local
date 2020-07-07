character function change_case(char)
    implicit none

    ! This function changes the case of its argument (if it
    ! is alphabetic)

    ! Dummy argumnet
    character, intent(in) :: char

    ! Local constant
    integer, parameter :: upper_to_lower = ichar("a")-ichar("A")

    ! Check if argument is lower case alphabetic, upper case
    ! alphabetic, or non-alphabetic
    if ("A"<=char .and. char<="Z") then
        ! Upper case - convert to lower case
        change_case = achar(ichar(char)+upper_to_lower)

    else if ("a"<=char .and. char<="z") then
        ! Lower case - convert to upper case
        change_case = achar(ichar(char)-upper_to_lower)

    else 
        ! Not alphabetic
        change_case = char
    
    end if 

end function change_case

program example0504
implicit none

character, external :: change_case
character :: char

print *,"Please in put a character."
read *, char

print *,change_case(char)

end program example0504