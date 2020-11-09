program PE1105
implicit none

character(len=500) :: string, key
character,dimension(500) :: chara, intkey

print *,"Please input the keyword."
read *, key

print *,"Please input the message you want to encrypt."
read *, string

key = trim(key)
string = trim(string)

call str2char(string,chara)
call str2char(key,intkey)

end program PE1105

subroutine str2char(str, chara)
    implicit none

    character(len=*), intent(in) :: str
    character,dimension(len(str)),intent(out) :: chara

    integer :: i 

    do i = 1,len(str)
        chara(i) = str(i:i)
    end do

end subroutine str2char

subroutine char2int(chara,numbs)
    implicit none

    character,dimension(:),intent(in) :: chara
    integer,dimension(size(chara)),intent(out) :: numbs

    integer :: i 

    do i = 1,size(chara)
        numbs(i) = ichar(chara(i))
    end do

end subroutine char2int

subroutine int2char(numbs,chara)
    implicit none

    integer,dimension(:),intent(in) :: numbs
    character,dimension(size(numbs)),intent(out) :: chara

    integer :: i 

    do i = 1,size(numbs)
        chara(i) = char(numbs)
    end do

end subroutine int2char

subroutine encrypt(message,key,charcode)
    implicit none

    character(len=*), intent(in) :: message
    character(len=*), intent(in) :: key
    character,dimension(:),optional:: charcode

    character,dimension(len(message)) :: charmessage
    character,dimension(len(key)) :: charkey

    integer,dimension(size(charmessage)) :: intmessage
    integer,dimension(size(charkey)) :: intkey
    integer,dimension(:) :: intcode
    character(len=len(key)),dimension(:) :: code

    integer :: i, j, max, start

    call str2char(message,charmessage)
    call str2char(key,charkey)

    call char2int(charkey,intkey)
    call char2int(charmessage,intmessage)

    max = len(message)/len(key)

    do i = 1,max+1
        start = i*len(key)-1
        do j = 1, len(key)
            intmessage(start+j) = intmessage(start+j) + key(j)
        end do
    end do

    call int2char(intmessage,charcode)

    do i = 1,max+1
        start = i*len(key)+1
        code(i) = charcode(j)//charcode(j+1)
    end do

end subroutine encrypt

