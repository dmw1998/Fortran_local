program golf_double
    implicit none

    real(8) :: up, tp, v, theta, T, uxval, uyval, u, phi
    real(8), parameter :: pi = 3.14159265358979323846_8
    real(8) :: gamma, ux, uy

    ! Set initial variables for Michelle
    up = 1.0_8 / sqrt(3.0_8)
    tp = 2.6E7_8
    v = 0.5_8
    theta = 30.0_8 * pi / 180.0_8

    ! Call fuunctions to get variables for Ben
    T = gamma(up) * tp 
    uxval = ux(up,theta,v)
    uyval = uy(up,theta,v)
    u = sqrt(uxval**2 + uyval**2)
    phi = atan2(uyval,uxval)

    ! Display results
    print *, "T = ", T 
    print *, "ux = ", uxval
    print *, "uy = ", uyval
    print *, "u = ", u 
    print *, "phi = ", phi

    ! Produce the plot of gamma(v) vs. v
    call plotGamma
    
end program golf_double

real(8) function ux(up,theta,v)
    implicit none

    real(8) :: up, theta, v, upx

    upx = up * cos(theta)
    ux = (upx+v) / (1.0_8+v*upx)

end function ux

real(8) function uy(up,theta,v)
    implicit none

    real(8) :: up, theta, v, upx, upy
    real(8) :: gamma

    upx = up * cos(theta)
    upy = up * sin(theta)
    uy = upy / (gamma(v) * (1.0_8+v*upx))

end function uy 

subroutine plotGamma
    use dislin
    implicit none

    integer , parameter :: n = 1000
    integer :: i 
    real(8), dimension(n) :: xv, yg
    real(8) :: v, gamma

    v = 0.0
    do i = 1,n
        xv(i) = v 
        yg(i) = gamma(v)
        v = v + 0.001_8
    end do

    call metafl('XWIN')
    call disini
    call titlin('gamma(v) vs. v',1)
    call name('v','x')
    call name('gamma(v)','y')
    call graf(-0.05_8,1.05_8,0._8,.1_8,-1._8,26._8,0._8,5._8)
    call grid(1,1)
    call title
    call color('red')
    call curve(vx,yg,n)
    call disfin

end subroutine plotGamma