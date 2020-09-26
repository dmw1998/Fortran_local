program colon_editing
implicit none

real :: a = 3.5, b = 7.2

print 201, a, b, a+b
print 202, a, b, a+b

201 format ("1", "With no colon:"/                      &
            "The sum of ", F5.2, " and ", F5.2, " is ", &
            F6.2/" Their product is ", F8.2)
202 format ("0", "With a colon:"/                       &
            "The sum of ", F5.2, " and ", F5.2, " is ", &
            F6.2:/" Their product is ", F8.2)

end program colon_editing