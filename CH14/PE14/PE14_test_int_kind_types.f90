program test_int_kind_types
implicit none

! two byte integer
integer(kind = 2) :: shorter

! four byte integer
integer(kind = 4) :: short 

! eight byte integer
integer(kind = 8) :: long

! sixteen byte integer
integer(kind = 16) :: longer

print *, huge(shorter)
print *, huge(short)
print *, huge(long)
print *, huge(longer)

end program test_int_kind_types

! From kind 17, returns an error -- Kind 17 not supported for type INTEGER