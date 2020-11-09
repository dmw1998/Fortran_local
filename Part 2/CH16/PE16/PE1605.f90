module deftype1605
implicit none

    type vector
        private
        integer :: length
        real, dimension(:), pointer :: elements
    end type vector

    interface operator(+)
        module procedure add
    end interface

    interface operator(-)
        module procedure sub
    end interface

    interface operator(*)
        module procedure sca_mul
    end interface

    interface assignment(=)
        module procedure arr_equals_vec
        module procedure vec_equals_arr
    end interface

contains

subroutine allocating(arr,vec)

    real, dimension(:), intent(in), target :: arr
    type(vector), intent(out) :: vec

    vec%length = size(arr)
    allocate(vec%elements(vec%length))

    vec%elements => arr

end subroutine allocating

subroutine deallocating(vec)

    type(vector) :: vec

    vec%length = 0
    nullify(vec%elements)
    deallocate(vec%elements)

end subroutine deallocating

function add(arr1,arr2)

    real, dimension(:), intent(in) :: arr1,arr2
    real, dimension(size(arr1)), target :: add

    type(vector) :: vec, vec1, vec2

    call allocating(arr1,vec1)
    call allocating(arr2,vec2)
    call allocating(add,vec)

    if (vec1%length /= vec2%length) then
        print *,"Fail to execute. Please input two vectors with the same size."
        stop
    else
        vec%elements = vec1%elements + vec2%elements
        vec%length = vec1%length
    end if

    call deallocating(vec)
    call deallocating(vec1)
    call deallocating(vec2)

end function add

function sub(arr1,arr2)

    real, dimension(:), intent(in) :: arr1,arr2
    real, dimension(size(arr1)) :: sub

    type(vector) :: vec, vec1, vec2

    call allocating(arr1,vec1)
    call allocating(arr2,vec2)
    call allocating(sub,vec)

    if (vec1%length /= vec2%length) then
        print *,"Fail to execute. Please input two vectors with the same size."
        stop
    else
        vec%elements = vec1%elements - vec2%elements
        vec%length = vec1%length
    end if

    call deallocating(vec)
    call deallocating(vec1)
    call deallocating(vec2)

end function sub

function sca_mul(sca,arr)

    real, intent(in) :: sca
    real, dimension(:), intent(in) :: arr
    real, dimension(size(arr)) :: sca_mul
    type(vector) :: vec

    sca_mul = arr
    call allocating(sca_mul,vec)

    vec%elements = sca * vec%elements

    call deallocating(vec)

end function sca_mul

subroutine arr_equals_vec(arr,vec)

    real, dimension(:), intent(out), target :: arr
    type(vector), intent(in) :: vec

    integer :: i 

    do i = 1,vec%length
        arr(i) = vec%elements(i)
    end do

end subroutine arr_equals_vec

subroutine vec_equals_arr(vec,arr)

    type(vector), intent(out) :: vec
    real, dimension(:), intent(in), target :: arr

    integer :: i

    vec%length = size(arr)
    allocate(vec%elements(vec%length))

    call allocating(arr,vec)

end subroutine vec_equals_arr

end module deftype1605

program PE1605
use deftype1605
implicit none

real,dimension(3) :: arr1 = (/ 1, 2, 3 /), arr2 = (/ -1, 2, -3 /)
real,dimension(4) :: arr3 = (/ 3, 6, -9, 13/)



end program PE1605