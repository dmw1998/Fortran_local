module FFT_Mod
    implicit none

    integer, parameter :: p = selected_real_kind(15)
    integer, parameter :: FFT_Forward = 1
    integer, parameter :: FFT_Inverse = -1

contains

subroutine fcFFT(x,forback)

    ! Subroutine FFT, Cooley-Tukey, radix-2
    complex(Kind=p), intent(inout) :: x(:)
    integer, intent(in) :: forback

    real(Kind=p), parameter :: pi = 3.141592654_p
    integer :: n, i, j, k, ncur, ntmp, itmp
    real(Kind=p) :: e
    complex(Kind=p) :: ctmp

    n = size(x)
    ncur = n
    do
        ntmp = ncur
        e = 2.0_p * pi / ncur
        ncur = ncur/2
        if (ncur < 1) exit
        do j = 1,ncur
            do i = j,n,ntmp
                itmp = i + ncur
                ctmp = x(i) - x(itmp)
                x(i) = x(i) + x(itmp)
                x(itmp) = ctmp * exp(forback * cmplx(0_p,e*(j-1),kind=p))
            end do   
        end do
    end do

    j = 1
    do i = 1,n-1
        if (i < j) then
            ctmp = x(j)
            x(j) = x(i)
            x(i) = ctmp
        end if
        k = n/2
        do while(k < j)
            j = j - k
            k = k / 2
        end do
        j = j + k
    end do
    Return
end Subroutine fcFFT

end module FFT_Mod

program main
    use FFT_Mod
    implicit none

    integer :: i
    integer, parameter :: N = 8
    complex(Kind=p) :: x(N) = [36.d0,21.d0,33.d0,44.d0,55.d0,63.d0,73.d0,38.d0 ]
    
    call fcFFT(x, FFT_Forward)
    do i = 1,N
        write (*,*) x(i)
    end do
    call fcFFT(x, FFT_Inverse)
    do i = 1,N
        write (*,*) x(i)/N
    end do  

end program main