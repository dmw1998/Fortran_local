program PE0804
implicit none

integer :: dd,yyyy 
character(len=3) :: mmm

print *,"Please input a date in the form dd mmm yyy, &
        where dd and yyyy are numeric, and mmm is a 3-letter &
        representation of the month."
read '(I2,1X,A3,1X,I4)',dd,mmm,yyyy

select case(mmm)
case("Jan")
    print '(I2,1X,"January",1X,I4)',dd,yyyy
case("Feb")
    print '(I2,1X,"February",1X,I4)',dd,yyyy
case("Mar")
    print '(I2,1X,"March",1X,I4)',dd,yyyy
case("Apr")
    print '(I2,1X,"April",1X,I4)',dd,yyyy
case("May")
    print '(I2,1X,"May",1X,I4)',dd,yyyy
case("Jun")
    print '(I2,1X,"June",1X,I4)',dd,yyyy
case("Jul")
    print '(I2,1X,"July",1X,I4)',dd,yyyy
case("Aug")
    print '(I2,1X,"August",1X,I4)',dd,yyyy
case("Sep")
    print '(I2,1X,"September",1X,I4)',dd,yyyy
case("Oct")
    print '(I2,1X,"October",1X,I4)',dd,yyyy
case("Nov")
    print '(I2,1X,"November",1X,I4)',dd,yyyy
case("Dec")
    print '(I2,1X,"December",1X,I4)',dd,yyyy
end select

end program PE0804