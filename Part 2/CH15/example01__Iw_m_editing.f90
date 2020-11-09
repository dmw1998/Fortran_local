program extended_integer_editing
implicit none

integer i 
do i = -10,10,5
    print '(I5,I5.2,I5.0)', i, i, i 
end do 

end program extended_integer_editing