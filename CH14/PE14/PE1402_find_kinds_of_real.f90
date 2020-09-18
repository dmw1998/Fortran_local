program find_kinds_of_real
implicit none

   real :: largereal
   print *, huge(largereal)
   
end program find_kinds_of_real

! huge() function gives the largest number that can be held by 
! the specific integer data type.