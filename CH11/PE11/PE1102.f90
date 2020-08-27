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
circle%a = x0
circle%b = y0
circle%c = r

print *,"Please input the point (x1,y1)."
read *,x1,y1
pt%x = x1
pt%y = y1

dis = (x1-x0)**2 + (y1-y0)**2

if (dis < r**2) then
    print *,"There are no tangent line for this point since it is inside the circle."

else if (dis == r**2)  then
    k = (x0-x1)/(y1-y0)
    b = (-x1*(x0-x1) + y1*(y1-y0))/(y1-y0)
    tangline%a = k
    tangline%b = -1
    tangline%c = b
    print *,"The tangent line of the circle is "
    print '(F9.6,"x-y+(",F9.6,")=0")',k,b

else
    if ((x0-x1)**2-r**2 == 0) then
        if ((y1-y0)**2 - r**2 /= 0) then
            k2 = (r**2 - (y1-y0)**2)/(2*(x0-x1)*(y1-y0))
            b2 = -k2*x1+y1
            print '("One of the tangent lines of the circle is x= ",F9.6)',x1
            tangline1%a = 1
            tangline1%b = 0
            tangline1%c = -x1
            print *,"The other tangent line of the circle is "
            print '(F9.6,"x-y+(",F9.6,")=0")',k2,b2
            tangline2%a = k2
            tangline2%b = -1
            tangline2%c = b2
        else
            print '("One of the tangent lines of the circle is x= ",F9.6)',x1
            tangline1%a = 1
            tangline1%b = 0
            tangline1%c = -x1
            print '("The other tangent line of the circle is y= ",F9.6)',y1
            tangline2%a = 0
            tangline2%b = 1
            tangline2%c = -y1
        end if        

    else 
        if ((y1-y0)**2 - r**2 == 0) then 
            k2 = (-2*(x0-x1)*(y1-y0))/((x0-x1)**2-r**2)
            b2 = -k2*x1 + y1
            print '("One of the tangent lines of the circle is y= ",F9.6)',y1
            tangline1%a = 0
            tangline1%b = 1
            tangline1%c = -y1
            print *,"The other tangent line of the circle is "
            print '(F9.6,"x-y+(",F9.6,")=0")',k2,b2
            tangline2%a = k2
            tangline2%b = -1
            tangline2%c = b2
        else
            delta = (-2*(x0-x1)*(y0-y1))**2 - 4*((x0-x1)**2-r**2)*((y0-y1)**2-r**2)
            k1 = (2*(x0-x1)*(y0-y1) + sqrt(delta))/(2*((x0-x1)**2-r**2))
            k2 = (2*(x0-x1)*(y0-y1) - sqrt(delta))/(2*((x0-x1)**2-r**2))
            b1 = -k1*x1+y1
            b2 = -k2*x1+y1
            print *,"One of the tangent lines of the circle is "
            print '(F9.6,"x-y+(",F9.6,")=0")',k1,b1
            tangline1%a = k1
            tangline1%b = -1
            tangline1%c = b1
            print *,"The other tangent line of the circle is "
            print '(F9.6,"x-y+(",F9.6,")=0")',k2,b2
            tangline2%a = k2
            tangline2%b = -1
            tangline2%c = b2
        end if
    end if
end if

end program PE1102