program PE1102
use geometric_data
implicit none

type(line) :: circle 
type(point) :: pt 
type(line) :: tangline,tangline1,tangline2
real :: x0,y0,r,x1,y1,k,b,k1,b1,k2,b2
real :: dis, delta

print *,"Please input the x0, y0 and r for a equation of circle (x-x0)^2 + (y-y0)^2 = r^2."
read *,x0,y0,r

! circle = x0,y0,r

print *,"Please input the point (x1,y1)."
read *,x1,y1
! pt = x1,y1

dis = (x1-x0)**2 + (y1-y0)**2

if (dis < r**2) then
    print *,"There are no tangent line for this point since it is inside the circle."

else if (dis == r**2)  then
    k = (x0-x1)/(y1-y0)
    b = (r**2 + x0*(x1-x0) + y0*(y1-y0))/(y1-y0)
    ! tangline = k,-1,b
    print *,"The tangent line of the circle is "
    print '(F9.6,"x-y+(",F9.6,")=0")',k,b

else
    delta = (-2*(x0-x1)*(y0-y1))**2 - 4*((x0-x1)**2-r**2)*((y0-y1)**2-r**2)
    print *,delta
    if (delta < 0) then 
        return
    else if (delta == 0) then 
        k1 = (2*(x0-x1)*(y0-y1))/(2*((x0-x1)**2-r**2))
        b1 = -k1*x1+y1
    else
        k1 = (2*(x0-x1)*(y0-y1) + sqrt(delta))/(2*((x0-x1)**2-r**2))
        k2 = (2*(x0-x1)*(y0-y1) - sqrt(delta))/(2*((x0-x1)**2-r**2))
        b1 = -k1*x1+y1
        b2 = -k2*x1+y1

        if (abs(k1)<abs(k2)) then
            k = k1
            k1 = k2
            k2 = k
            b = b1
            b1 = b2
            b2 = b
        end if

        if (abs(k1) > 10.0**(10) .and. abs(k2) >= 10**(-10)) then
            print '("One of the tangent lines of the circle is x= ",F9.6)',x1
            print *,"The other tangent line of the circle is "
            print '(F9.6,"x-y+(",F9.6,")=0")',k2,b2
        else if (abs(k1) <= 10.0**(10) .and. abs(k2) < 10**(-10)) then
            print '("One of the tangent lines of the circle is y= ",F9.6)',y1
            print *,"The other tangent line of the circle is "
            print '(F9.6,"x-y+(",F9.6,")=0")',k2,b2
        else if (abs(k1) > 10.0**(10) .and. abs(k2) < 10**(-10)) then
            print '("One of the tangent lines of the circle is x= ",F9.6)',x1
            print '("The other tangent line of the circle is y= ",F9.6)',y1
        else
            print *,"One of the tangent lines of the circle is "
            print '(F9.6,"x-y+(",F9.6,")=0")',k1,b1
            print *,"The other tangent line of the circle is "
            print '(F9.6,"x-y+(",F9.6,")=0")',k2,b2
        end if 

    end if

    ! if (abs(x0-x1)==r) then
    !     print '("One of the tangent lines of the circle is x= ",F9.6)',x1
    ! end if
    ! if (abs(y0-y1)==r) then
    !     print '("One of the tangent lines of the circle is y= ",F9.6)',y1
    ! end if

end if

end program PE1102