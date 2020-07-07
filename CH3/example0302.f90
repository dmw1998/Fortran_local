program welcom
implicit none

! This program manipulates character strings to produce a properly formatted welcom message

! Variable declarations
character(len=20) :: title, first_name, last_name
character(len=40) :: full_name

! Ask for name, etc.
print *,"Please give your full name in the form requested"
print *,"Title (Mr./Mrs./Ms./Professor/etc): "
read *, title

print *,"First name: "
read *, first_name

print *,"Last name"
read *, last_name

! Create full name
full_name = TRIM(title)//" "//TRIM(first_name)//" "//last_name      ! TRIM removes any trailing blanks from the character string provide as its argumnt

! Print message
print *,"Welcom ",full_name
print *,"May I call you ",TRIM(first_name),"?"      ! This TRIM ensures that the question mark at the end of the question comes immediately after the name 

end program welcom