program Ex0920
implicit none

integer :: a(3)
namelist /na/ a

open(10,file="Ex0920.txt",status="replace")

write(10,*) "Happy birthday"
write(10,*) "&na a = 1,2,3 /"

close(10)

open(11,file="Ex0920.txt")

read(11,nml=na)
write(*,'(3I2)') a

close(11)

end program Ex0920