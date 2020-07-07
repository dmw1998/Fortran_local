program PE0608
implicit none

integer :: time = 0, i = 0
real :: T, q, k, y

print *,"Please input the initial temperature."
read *, T 

do 
    q = 2000.0/(T+273.16)
    k = exp(-q)
    y = 1 - exp(-k*time)

    if (y < 0.95) then
        if (time-60*i == 60) then
            i = i + 1
            print *,"After",i,"min, the yield is",y 
        end if 
        time = time + 1
    else
        exit
    end if
 
end do

print *," "
print *,"It cost ",i," min",(time-60*i),"sec to reach 95%."

end program PE0608