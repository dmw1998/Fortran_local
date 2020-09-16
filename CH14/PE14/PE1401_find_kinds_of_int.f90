program find_kinds_of_int
implicit none

   integer :: largeint
   print *, huge(largeint)
   
end program find_kinds_of_int

! huge() function gives the largest number that can be held by 
! the specific integer data type.