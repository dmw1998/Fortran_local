program PE1407
implicit none

complex :: w, z

print *,"A complex number is in the form of (real, imag)."
print *,"Please input a complex number w."
read *, w
print *,"Please input a complex number z."
read *, z

print '("w+z = ",F6.2," i + (",F6.2,") j")', w+z
print '("z_bar = ",F6.2," i + (",F6.2,") j")', conjg(z)
print '("w_bar = ",F6.2," i + (",F6.2,") j")', conjg(w)
print '("z^2 = ",F6.2," i + (",F6.2,") j")', z**2
print '("z*z_bar = ",F6.2," i + (",F6.2,") j")', z*conjg(z)

end program PE1407