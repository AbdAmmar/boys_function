

! _______________________________________________________________________________________
!
!                       Boys function for complex-valued arguments z
!
!        F_m(z) = \int_0^{1} \exp(-z t^2) t^{2 m} dt, with z = x + \imath y

complex*16 function cboys_f(n, z)

  implicit none

  integer,    intent(in) :: n
  complex*16, intent(in) :: z

  integer                :: i, mmax
  double precision       :: sq_op5 = dsqrt(0.5d0), sqpi = dsqrt(dacos(-1.d0))
  double precision       :: z_mod, z_re, z_im 
  double precision       :: sq_z_re, sq_z_im
  double precision       :: n_tmp
  complex*16             :: sq_z, z_inv, z_exp

  complex*16             :: cboys_f_smallz, cpx_erf

  z_re  = REAL (z)
  z_im  = AIMAG(z)
  z_mod = dsqrt(z_re*z_re + z_im*z_im)

  if(z_mod < 10.d0) then
    ! small z

    if(z_mod .lt. 1.d-10) then

      cboys_f = 1.d0 / dble(n + n + 1)

    else

      cboys_f = cboys_f_smallz(n, z)

    endif

  else
    ! large z

    if(z_mod .gt. 40.d0) then

      n_tmp   = dble(n) + 0.5d0
      cboys_f = 0.5d0 * gamma(n_tmp) / (z**n_tmp)

    else

      ! get \sqrt(z)
      sq_z_re = sq_op5 * dsqrt(z_re + z_mod)
      sq_z_im = 0.5d0 * z_im / sq_z_re 
      sq_z    = sq_z_re + (0.d0, 1.d0) * sq_z_im

      z_exp = zexp(-z)
      z_inv = (1.d0, 0.d0) / z

      cboys_f = 0.5d0 * sqpi * cpx_erf(sq_z_re, sq_z_im) / sq_z
      mmax = n
      if(mmax .gt. 0) then
        do i = 0, mmax-1
          cboys_f = ((dble(i) + 0.5d0) * cboys_f - z_exp) * z_inv
        enddo
      endif

    endif

  endif

end function cboys_f
! _______________________________________________________________________________________


! _______________________________________________________________________________________
!
!                       Boys function for small arguments z

complex*16 function cboys_f_smallz(n, z)

  implicit none
  integer,    intent(in)      :: n
  complex*16, intent(in)      :: z

  integer,          parameter :: kmax = 50
  double precision, parameter :: eps = 1.d-13

  integer                     :: k
  double precision            :: delta_mod
  complex*16                  :: z_k, ct, delta_k

  ct  = 0.5d0 * zexp(-z) * gamma(dble(n) + 0.5d0)
  z_k = (1.d0, 0.d0)

  cboys_f_smallz = ct * z_k / gamma(dble(n) + 1.5d0)

  do k = 1, kmax

    z_k            = z_k * z
    delta_k        = ct * z_k / gamma(dble(n+k) + 1.5d0)
    cboys_f_smallz = cboys_f_smallz + delta_k

    delta_mod = dsqrt(REAL(delta_k)*REAL(delta_k) + AIMAG(delta_k)*AIMAG(delta_k))
    if(delta_mod .lt. eps) return

  enddo

  if(delta_mod > eps) then
    write(*,*) ' pb in cboys_f_smallz !'
    write(*,*) ' n, z      = ', n, z
    write(*,*) ' delta_mod = ', delta_mod
    write(*,*) ' increase kmax or decrease eps'
    stop 1
  endif

end function cboys_f_smallz
! _______________________________________________________________________________________



