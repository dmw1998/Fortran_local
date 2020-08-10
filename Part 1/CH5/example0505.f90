program seasons
implicit none

! A program to calculate in which season a specified data lies

! Variable declarations
character(len=10) :: date
character(len=2) :: month

! Read date
print *,"please type a date in the form yyyy-mm-dd"
read *, date

! Extract month number
month = date(6:7)

! Print season
select case (month)
case ("08":"10")
    print *, date, " is in the spring."
case ("11","12","01":"03")
    print *, date, " is in the summer."
case ("04","05")
    print *, date, " is in the autumn."
case ("06","07")
    print *, date, " is in the winter."
case default
    print *, date, " is not a valid date"
end select

end program seasons