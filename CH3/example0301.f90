program centigrade_to_fahrenheit
implicit none

! A program to convert a Centigrade temoerature to Fahrenheit

! Variable declarations
real :: temp_c, temp_f1, temp_f2     ! temp_c: centigrade temperature; temp_f: fahrenheit_temperature

! Ask for Centigrade temperature
print *,"What is the Centigrade temperature?"
read *, temp_c

! Convert it to Fahrenheit (Using the formula F=9C/5+32)
temp_f1 = 9.0*temp_c/5.0 + 32.0
temp_f2 = 1.8*temp_c + 32.0

! Print both temperatures
print *, temp_c,"C = ",temp_f1,"F"
print *, temp_c,"C = ",temp_f2,"F"

end program centigrade_to_fahrenheit