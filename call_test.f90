
program call_test

  implicit none

  character(len=60), parameter :: FMT0 = "(2X, I5, 4X, 2(F22.15, 2X, F22.15, 2X, F22.15, 4X))"

  integer                      :: i, j, imax, nb_test
  double precision             :: z_re, z_im
  complex*16                   :: z, Fz

  complex*16, external         :: cboys_f

  imax    = 20
  nb_test = 10

  do i = 0, imax

    ! --- --- --- ---
    ! small |z|

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 1d-12
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 1d-10
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 1d-7
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 1d-4
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 1d-2
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 1d-1
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 1d0
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 10d0
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    ! ---
    ! --- --- --- ---

    ! ---

    ! --- --- --- ---
    ! large |z|

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 15d0
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 20d0
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 25d0
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 30d0
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 35d0
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 40d0
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 45d0
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 50d0
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 60d0
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 80d0
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 100d0
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    do j = 1, nb_test
      call random_number(z_re)
      call random_number(z_im)
      z  = (z_re + (0.d0, 1.d0) * (z_im - 0.5d0)) * 1000d0
      Fz = cboys_f(i, z)
      write(*, FMT0) i, real(z) , aimag(z) , dsqrt(real(z)**2  + aimag(z)**2 ) &
                      , real(Fz), aimag(Fz), dsqrt(real(Fz)**2 + aimag(Fz)**2)
    enddo

    ! ---
    ! --- --- --- ---

  enddo

end program call_test
