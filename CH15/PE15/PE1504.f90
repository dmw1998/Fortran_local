program PE1504
implicit none

character,dimension(26) :: alphabet = &
   (/"A","B","C","D","E","F","G","H", &
     "I","J","K","L","M","N","O","P", &
     "Q","R","S","T","U","V","W","X", &
     "Y","Z"/)
character(len=20) :: filename
integer :: i, ios
logical :: file_exists

print *,"What is the name for the output file?"
do
    read '(A)',filename
    inquire (file=filename,exist=file_exists)
    if (.not.file_exists) exit 

    print '("The file ",A," already exists. Please &
            &give another name for the output file")', &
            filename
end do 

open (unit=7,file=filename,status="new",iostat=ios)

if (ios /= 0) then
    print '("Error ",I5," while opening ",A)', filename
    stop
end if

write (unit=7,fmt='(A)',iostat=ios) (alphabet(i), i=1,26)
if (ios /= 0) then
    print '("Error ",I5," while writing to ",A)', filename
    stop
end if

close(7)

end program PE1504