subroutine triangle_type(a,b,c,type)
real,intent(in)  :: a,b,c
character(len=12),intent(out) ::  type

    if (a+b>c .and. a+c>b .and. b+c>a .and. a-b<c .and. a-c<b .and. b-c<a) then
        if (a==b .or. a==c .or. b==c) then
            if (a==b .and. b==c) then
                type = "equilat."
            else
                type = "isosceles."
            end if
        else
            type = "triangle."
        end if
    else
        type = "not triangle."
    end if
    
end subroutine triangle_type

program PE0514
implicit none

real :: a,b,c
character(len=12) ::  type

print *,"Please input three numbers."
read *, a,b,c

call triangle_type(a,b,c,type)

print *,"It is ",type

end program PE0514