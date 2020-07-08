module sheet
implicit none
save
    type sheet_size
    real :: long, wide
    end type sheet_size
end module sheet

subroutine cal_ratio(ratio, num_discs, diameter, sheet1)
use sheet
implicit none

real,intent(out) :: ratio
type(sheet_size),intent(in) :: sheet1
integer,intent(in) :: num_discs
real,intent(in) :: diameter

integer :: num_long, num_wide, num_sheet1, num_sheet
real :: sheet_area, discs_area
real, parameter :: pi = 3.141592653589

num_long = ceiling(sheet1%long / diameter)
num_wide  = ceiling(sheet1%wide / diameter)
num_sheet1 = num_long * num_wide
num_sheet = ceiling((1.0*num_discs) / (1.0*num_sheet1))

sheet_area = num_sheet * sheet1%long * sheet1%wide
discs_area = (0.5*diameter)**2 * pi * (1.0*num_discs)
ratio = (sheet_area-discs_area)/discs_area
    
end subroutine cal_ratio


program PE0608
use sheet
implicit none

integer :: num_discs, i 
real :: diameter
type(sheet_size) :: num_sheet1
real, parameter :: long1=2,long2=5,long3=10
real, parameter :: wide1=2,wide2=4,wide3=6
real :: wastage1,wastage2,wastage3,wastage_min

print *,"Please input the number and diameter (less than one meter) of discs required."
read *, num_discs, diameter

do i=1,3
    do j=1,3
        




do i=1,3
    select case (i)
    case (1)
        call cal_ratio(wastage1, num_discs, diameter, sheet1)
    case (2)
        call cal_ratio(wastage2, num_discs, diameter, sheet2)
    case (3)
        call cal_ratio(wastage3, num_discs, diameter, sheet3)
    end select
end do

wastage_min = min(wastage1,wastage2,wastage3)

if (wastage_min == wastage1)  then
    print *,"Better to choose the sheet of size",sheet1%long,"by",sheet1%wide
else if (wastage_min == wastage2)  then
    print *,"Better to choose the sheet of size",sheet2%long,"by",sheet2%wide
else
    print *,"Better to choose the sheet of size",sheet3%long,"by",sheet3%wide
end if 

end program PE0608