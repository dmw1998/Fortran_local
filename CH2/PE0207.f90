program exercise_2_7
implicit none

Character(len=2) :: hh, mm      ! Define as strings. It can be len=* as well.

print *,"What time is it?"
print *,"Please input the time by the form of hours and minutes separated by space."

read *,hh, mm
print *,"The time is ",mm, " minutes after ",hh

end program exercise_2_7