type(vector) function int_times_vector(n,v)
! This function multiplies every element of the vector by integer n

    ! Dummy arguments
    integer,intent(in) :: n
    type(vector),intent(in) :: v 

    ! Local variable
    integer :: i 

    int_times_vector%length = v%length
    do i = 1,v%length
        int_times_vector%element(i) = n*v%element(i)
    end do 
end function int_times_vector

type(vector) function vector_times_int(v,n)
! This function multiplies every element of the vector v by the integer n

    ! Dummy arguments
    integer,intent(in) :: n
    type(vector),intent(in) :: v 

    ! Local variable
    integer :: i 

    int_times_vector%length = v%length
    do i = 1,v%length
        int_times_vector%element(i) = n*v%element(i)
    end do 
end function vector_times_int

type(vector) function real_times_vector(p,v)
! This function multiplies every element of the vector by real number p

    ! Dummy arguments
    real,intent(in) :: p
    type(vector),intent(in) :: v 

    ! Local variable
    integer :: i 

    real_times_vector%length = v%length
    do i = 1,v%length
        real_times_vector%element(i) = p*v%element(i)
    end do 
end function real_times_vector

type(vector) function vector_times_real(v,p)
! This function multiplies every element of the vector by real number p

    ! Dummy arguments
    real,intent(in) :: p
    type(vector),intent(in) :: v 

    ! Local variable
    integer :: i 

    vector_times_real%length = v%length
    do i = 1,v%length
        vector_times_real%element(i) = p*v%element(i)
    end do 
end function vector_times_real

interface operator(*)
    module procedure int_times_vector
    module procedure vector_times_int
    module procedure real_times_vector
    module procedure vector_times_real
end interface

public :: operator(*)