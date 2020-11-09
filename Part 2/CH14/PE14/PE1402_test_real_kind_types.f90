program test_real_kind_types
implicit none

! four byte real
real(kind = 4) :: shorter

! eight byte real
real(kind = 8) :: short

! ten byte real
real(kind = 10) :: long

! sixteen byte real
real(kind = 16) :: longer

print *, huge(shorter)
print *, huge(short)
print *, huge(long)
print *, huge(longer)

end program test_real_kind_types

! kind 2, returns an error -- Kind 2 not supported for type REAL
! kind 32, returns an error -- Kind 32 not supported for type REAL