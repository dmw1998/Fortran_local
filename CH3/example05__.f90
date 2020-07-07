program character_example
implicit none

Character(len=3) :: string_1
Character(len=4) :: string_2, string_3

string_1 = "End"
string_2 = string_1
string_3 = "Final"

print *, string_1, string_2, string_3

end program character_example