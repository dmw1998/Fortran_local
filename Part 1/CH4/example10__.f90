module data_design
implicit none

! declare derived types
type data
integer :: day,month,year
end type data

type time
integer :: hours,mins
real :: seconds
end type time

! Declare global variables
type(data) :: experiment_data
type(time) :: start_time,end_time
integer :: sample_number
character(len=20) :: material_type
real :: stert_measurement_1,stert_measurement_2, &
        stert_measurement_3,stert_measurement_4, &
        end_measurement_1, end_measurement_2, &
        end_measurement_3,end_measurement_4, &
        stats_measurement_1,stats_measurement_2, &
        stats_measurement_3,property_1,property_2, &
        property_3,property_4

end module data_design