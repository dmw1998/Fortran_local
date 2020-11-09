module personnel_records
implicit none

type records
    private
    character(len=10) :: first_name
    character(len=10) :: last_name
    character(len=10) :: department
    character(len=8) :: birthday        ! yyyymmdd
    real :: salary

contains
end module personnel_records