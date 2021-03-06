subroutine input(unit,max_datasets,survey_data,num_datasets, &
                 error_code)
use error_codes
use survey_details
implicit none

! This subroutine reads up to max_datasets records prepared
! as follows, returning the number  read in num_datasets

! Columns 1-20  Name
!           23  Sex (M or F)
!           25  Job status (1-5)
!        28,29  Age - for status 1, 4 or 5
!        28-31  Monthly salary - for status 2 and 3
!        32-34  Other monthly income - for status 3
!        32-34  Months unemployed - for status 4
!           31  Special code (1-3) - for status 5

! Arguments
integer,intent(in) :: unit, max_datasets
integer,intent(out) :: num_datasets,error_code
type(survey_info),dimension(:),intent(out) :: survey_data

! Local variables
character(len=30) :: data_file
character(len=10) :: input_record
character(len=20) :: name
character :: sex
integer :: i,ios,status,age,months,code
real :: salary,income

! Ask for name of data file
! A maximum of theree attempts will be allowed to open the file
do i = 1,3
    print *,"Type name of data file"
    read '(A)', data_file
    ! Open file at beginning
    open (UNIT = unit, FILE = data_file, POSITION = "REWIND",    &
          IOSTAT = ios)
    if (ios == 0) exit
    ! Error when opening file - try again
    print *,"Unable to open file - please try again"
end do

! If open was unsuccessful after 3 attempts return error = -1
if (ios /= 0) then
    error_code = -1
    return
end if 

! Loop to read data
do i = 1,max_datasets
    ! Read next data record
    read (UNIT = unit, FMT = '(A20,2X,A1,1X,I1,2X,A10)',  &
          IOSTAT = ios) name, sex, status, input_record
    ! Check for errors and end of file
    select case (ios)
    case (end_file)             ! End of file - no more data
        exit
    case (1:)                   ! Error during reading
        error_code = -2
        exit
    end select

    ! Read remaining data from internal file according to status code
    select case (status)

    case (ft_ed)
        ! Read age and set other items to unused code
        read (UNIT = unit, FMT = '(I2)') age 
        months = unused
        salary = unused
        income = unused
        code = unused

    case (ft_job)
        ! Read salary details and set other items to unused
        read (UNIT = unit, FMT = '(F4.0)') salary
        age = unused
        income = unused
        months = unused
        code = unused

    case (pt_job)
        ! Read income details and set other items to unused
        read (UNIT = unit, FMT = '(F4.0,2X,F4.0)') salary,income
        age = unused
        months = unused
        code = unused
    
    case (no_job)
        ! Read age and months unemployed
        ! and set other items to unused
        read (UNIT = unit, FMT = '(I2,2X,I3)') age,months
        salary = unused
        income = unused
        code = unused

    case (at_home)
        ! Read age and code, and set other items to unused
        read (UNIT = unit, FMT = '(I2,1X,I1)') age, code 
        salary = unused
        income = unused
        months = unused
    
    end select

    ! Record is noe fully input, so copy to main data array
    survey_data(i) = survey_info (name, sex, status, age, &
                                months, code, salary, income)
        
end do 

! All data input - check if end of file was read
if (i > max_datasets) error_code = -3

! Save number of records read and return
num_datasets = i - 1 

end subroutine input