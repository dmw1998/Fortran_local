character function next_char()
    implicit none

    ! This functiono returns the next character from a
    ! buffer, refiling the buffer from the keyboard
    ! when necessary

    ! Local variables
    character(len=80),save :: buffer=" "
    integer,save :: len, chars_left = 0
    integer :: char_pos

    ! Test to see if any characters left
    if (chars_left == 0) then 
        ! No characters left - refill buffer
        print *,"?"
        read(unit=*,fmt='(A)') buffer

        ! Set len to length without any trailing blanks
        len = len_trim(buffer)
        chars_left = len
    end if

    ! Return next character in buffer
    char_pos = len-chars_left+1
    next_char = buffer(char_pos:char_pos)
    chars_left = chars_left-1
end function next_char

program PE1101
implicit none

! This program uses the function next_char to read
! a character string from the keyboard

! Function declaration
character,external :: next_char

! Variable declarations
character(len=500) :: message
integer :: i, pos=1

print *,"Please type a sentence of no more than 500 &
        &character terminated by a period."
print *,'Type one line at time when prompted by a "?"'

! Loop to read the sentence
do
    message(pos:pos) = next_char()

    ! Check if this was the terminating character
    if (message(pos:pos)==".") exit
    pos = pos + 1
end do

! Print message
print '("The sentence you typed was:"/(A60))', &
    (message(i:min(i+60,pos)),i=1,pos,60)

end program PE1101