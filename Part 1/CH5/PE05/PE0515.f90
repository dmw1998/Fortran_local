logical function within(str1,str2)

character(len=*) :: str1, str2
integer :: ind

ind = index(str1,str2)

if (ind == 0) then
    within = .false.
else 
    within = .true.
end if 

end function within

program PE0515
implicit none

logical, external :: within

character(len = 88) :: str1,str2

print *,"Please input the first character string with quotation marks."
read *, str1
print *,"Please input the second character sting with quotation marks."
read *, str2

if (within(trim(str1),trim(str2)) .eqv. .true.) then
    print *,"The phrase '",trim(str2),"' is contained within '",trim(str1),"'."
else
    print *,"The phrase '",trim(str2),"' is not contained within '",trim(str1),"'."
end if

end program PE0515
