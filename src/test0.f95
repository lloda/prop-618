! (test.f95) -*- coding: utf-8; mode: f90-mode -*-
! 2019-04
! tests for prop.f95, atmospheres.f95

program test0
  use prop
  use atmospheres

  integer :: n

  n = init()
  n = n + test_p835()
  n = n + test_p839_rain_height()
  n = n + test_p838_coeffs()
  n = n + test_p618_rain()
  n = n + test_p676_gas()
  n = n + test_p676_att_e2s()

  write(*, *) achar(10), n, ' errors.'
  stop n

contains

  real function rel_error(a, b) &
       result(e)
    real :: a, b
    if (a==b) then
       e = 0
    else
       e = 2.*abs(a-b)/(abs(a)+abs(b))
    end if
  end function rel_error

  integer function num_test(legend, expected, actual, aspec, rspec) &
       result(ne)

    character(len=*), intent(in) :: legend
    real, intent(in) :: expected, actual
    real, intent(in), optional :: aspec, rspec

    ne = 0
    if (present(aspec)) then
       block
         real :: err
         err = abs(actual-expected)
         if (err<=aspec) then
            write(*, '(A, A, ES12.6, A, ES12.6, A, ES8.2, A, ES8.2, A)') &
                 legend, ' = ', actual, ' (exp ', expected, ') aerror ', err, ' ≤ ', aspec, ''
         else
            ne = ne + 1
            write(*, '(A, A, ES12.6, A, ES12.6, A, ES8.2, A, ES8.2, A)') &
                 legend, ' = ', actual, ' (exp ', expected, ') aerror ', err, ' > ', aspec, ' (*)'
         end if
       end block
    end if
    if (present(rspec)) then
       block
         real :: err
         err = rel_error(actual, expected)
         if (err<=rspec) then
            write(*, '(A, A, ES12.6, A, ES12.6, A, ES8.2, A, ES8.2, A)') &
                 legend, ' = ', actual, ' (exp ', expected, ') rerror ', err, ' ≤ ', rspec, ''
         else
            ne = ne + 1
            write(*, '(A, A, ES12.6, A, ES12.6, A, ES8.2, A, ES8.2, A)') &
                 legend, ' = ', actual, ' (exp ', expected, ') rerror ', err, ' > ', rspec, ' (*)'
         end if
       end block
    end if
  end function num_test

  ! ---------------------
  ! p835_ref
  ! ---------------------

  integer function test_p835_ref(hg, Pspec, rhospec, Tspec) &
       result(ne)

    real :: hg, Pspec, rhospec, Tspec
    real :: P, rho, T
    integer :: error = 99
    character(len=20) :: hgs

    call p835_ref(hg, P, rho, T, error)
    if (error /= 0) then
       ne = error
    else
       ne = 0
    end if
    write(hgs, '(A, F8.3)') 'hg = ', hg
    ne = ne + num_test(trim(hgs) // ' P', Pspec, P, rspec=1e-4)
    ne = ne + num_test(trim(hgs) // ' ρ', rhospec, rho, rspec=1e-4)
    ne = ne + num_test(trim(hgs) // ' T', Tspec, T, rspec=1e-4)

  end function test_p835_ref

  integer function test_p835() &
       result(ne)

    write(*, *) achar(10), 'p835'
    ne = 0
    ne = ne + test_p835_ref(h_geom(0.), 1013.25, 7.5, 288.15)
    ne = ne + test_p835_ref(h_geom(11.), 226.3226, 3.0359952365e-2, 216.65)
    ne = ne + test_p835_ref(85.9999, 0.00373405025, 1.5864e-18, 186.9461)
    ne = ne + test_p835_ref(86.0, 0.00373405025, 1.5864e-18, 186.8673)
    ne = ne + test_p835_ref(91.0, 1.5380782489e-3, 1.302154e-19, 186.8673)
    ne = ne + test_p835_ref(100.0, 3.2012436405e-4, 1.446562e-21, 195.081344)

  end function test_p835

  ! ---------------------
  ! p839_rain_height
  ! ---------------------

  integer function test_p839_rain_height_ref(lat, lon, hspec) &
       result(ne)

    real :: lat, lon, hspec
    character(len=30) :: line

    write(line, '(A, F8.3, A, F7.3, A)') 'lat =', lat, ' lon =', lon, ' h '
    ne = num_test(trim(line), hspec, p839_rain_height(lat, lon), rspec=1e-15)

  end function test_p839_rain_height_ref

  integer function test_p839_rain_height() &
       result(ne)

    write(*, *) achar(10), 'p839_rain_height'
    ne = 0
    ne = ne + test_p839_rain_height_ref(0., 0., 4.926)
    ne = ne + test_p839_rain_height_ref(0., 0., 4.926)
    ne = ne + test_p839_rain_height_ref(90., 0., 2.456)
    ne = ne + test_p839_rain_height_ref(88.5, 0., 2.796)
    ne = ne + test_p839_rain_height_ref(-90., 0., 3.24)

  end function test_p839_rain_height

  ! ---------------------
  ! p838_coeffs
  ! ---------------------

  integer function test_p838_coeffs_ref(fghz, kht, aht, kvt, avt) &
       result(ne)

    real, intent(in) :: fghz, kht, aht, kvt, avt
    real :: kh, ah, kv, av
    character(len=30) :: line

    write(line, '(A, F6.1, A, F7.3, A)') 'f(Ghz) =', fghz
    call p838_coeffs(fghz, kh, ah, kv, av)
    ne = 0
    ne = ne + num_test(trim(line) // ' kₕ', kht, kh, rspec=2e-4)
    ne = ne + num_test(trim(line) // ' aₕ', aht, ah, rspec=1e-4)
    ne = ne + num_test(trim(line) // ' kᵥ', kvt, kv, rspec=2e-4)
    ne = ne + num_test(trim(line) // ' aᵥ', avt, av, rspec=1e-4)

  end function test_p838_coeffs_ref

  integer function test_p838_coeffs() result(ne)

    write(*, *) achar(10), 'p838_coeffs'
    ne = 0
    ne = ne + test_p838_coeffs_ref(8., 0.004115, 1.3905, 0.003450, 1.3797);
    ne = ne + test_p838_coeffs_ref(37., 0.3789, 0.8890, 0.3633, 0.8621);
    ne = ne + test_p838_coeffs_ref(99., 1.3594, 0.6826, 1.3601, 0.6775);

  end function test_p838_coeffs

  ! ---------------------
  ! p618_rain
  ! ---------------------

  integer function test_p618_rain_ref(id, attpt, latdeg, londeg, hs, fghz, eldeg, taudeg, p, r001) &
       result(ne)

    integer :: id
    real :: attpt, fghz, taudeg, latdeg, londeg, hs, eldeg, p, r001, r001_
    character(len=200) :: line
    write(line, '(I2, A, F7.3, A, F7.3, A, F7.3, A, F7.3, A, F7.3, A, F7.3, A, F7.3, A, F7.3, A)') &
         id, ' f ', fghz, ' τ ', taudeg, ' lat ', latdeg, ' lon ', londeg, &
         ' hₛ ', hs, ' el ', eldeg, ' p ', p, ' R₀₀₁ ', r001, ' -> Ap '
    ne = 0
    r001_ = -1.
    ne = ne + num_test(trim(line), attpt, p618_rain(latdeg, londeg, hs, fghz, eldeg, taudeg, p, r001), rspec=5e-15)
    ne = ne + num_test(trim(line), attpt, p618_rain(latdeg, londeg, hs, fghz, eldeg, taudeg, p, r001_), rspec=5e-4)
  end function test_p618_rain_ref

  integer function test_p618_rain() &
       result(ne)

    integer ierror, i
    real, allocatable :: c(:, :)
    open(1, file='../data/P618/P618-13A_Rain.txt')   ! FIXME install path
    allocate(c(9, 64), STAT=ierror)
    read(1, *) c
    c = transpose(c)

    ne = 0
    write(*, *) achar(10), 'p618_rain, rows as in ITU-e2s-val (CG-3M3J-13-ValEx-Rev4_2.xlsx / P618-13 A_Rain)'
    do i=1,size(c, 1)
       ne = ne + test_p618_rain_ref(i+22, c(i, 9), c(i, 1), c(i, 2), c(i, 3), c(i, 4), c(i, 5), c(i, 6), c(i, 7), c(i, 8))
    end do

  end function test_p618_rain

  ! ---------------------
  ! p676_gas
  ! ---------------------

  integer function test_p676_gas() &
       result(ne)

    real :: go, gw

    ne = 0

    ! 50% humidity - [g0, gw] = p676d11_ga(20.0, 1013.25, 11.508, 300.0)

    write(*, *) achar(10), 'p676 50%'
    call p676_gas(0, 20.0, 1013.25, vapor_pressure(11.508, 300.), 300.0, go, gw)
    ne = ne + num_test('g₀', 0.0106557034883802, go, rspec=5e-15)
    ne = ne + num_test('gw', 0.145950346093096, gw, rspec=5e-15)

    ! 30% humidity - [g0, gw] = p676d11_ga(20.0, 1013.25, 6.9045, 300.0)

    write(*, *) achar(10), 'p676 30%'
    call p676_gas(0, 20.0, 1013.25, vapor_pressure(6.9045, 300.), 300.0, go, gw)
    ne = ne + num_test('g₀', 0.0105877535759878, go, rspec=5e-15)
    ne = ne + num_test('gw', 0.0867126146205278, gw, rspec=5e-15)

    write(*, *) achar(10), 'ITU-e2s-val (CG-3M3J-13-ValEx-Rev4_2.xlsx / P676-11 SpAtt eq. 1 to 9)'
    block
      real, dimension(5) :: fghz = (/ 12, 20, 60, 90, 130 /)
      real, dimension(5) :: got = (/ 0.0086982640687736, 0.0118835504778076, 14.6234747964861000, &
           0.0388697110724235, 0.0415090835995228 /)
      real, dimension(5) :: gwt = (/ 0.009535388220246, 0.097047304815112, 0.154841840636247, &
           0.341973394422181, 0.751844703646129 /)
      integer :: i

      do i = 1, size(fghz, 1)
         call p676_gas(0, fghz(i), 1013.25, vapor_pressure(7.5, 288.15), 288.15, go, gw)
         ne = ne + num_test('g₀', got(i), go, rspec=8e-15)
         ne = ne + num_test('gw', gwt(i), gw, rspec=8e-15)
      end do
    end block

    write(*, *) achar(10), 'ITU-e2s-val (CG-3M3J-13-ValEx-Rev4_2.xlsx / P676-11 SpAtt eq. 22-23)'
    block
      real, dimension(5) :: fghz = (/ 12, 20, 60, 90, 130 /)
      real, dimension(5) :: got = (/ 0.0086982632089130, 0.0118835474853413, 14.6234770052713000, &
           0.0388696217227390, 0.0415090337921489 /)
      real, dimension(5) :: gwt = (/ 0.00948862715511772, 0.09694095785382280, 0.15334819552225700, &
           0.33927973759541800, 0.74582386063772800 /)
      integer :: i

      do i = 1, size(fghz, 1)
         call p676_gas(1, fghz(i), 1013.25, vapor_pressure(7.5, 288.15), 288.15, go, gw)
         ne = ne + num_test('g₀', got(i), go, rspec=8e-15)
         ne = ne + num_test('gw', gwt(i), gw, rspec=8e-15)
      end do
    end block

  end function test_p676_gas

  ! ---------------------
  ! p676 total path
  ! ---------------------

  integer function test_p676_att_e2s() &
       result(ne)

    ne = 0

    block
      integer ierror, i
      character(256) :: iomsg
      character(len=200) :: line
      real, allocatable :: c(:, :)

      write(*, *) achar(10), 'ITU-e2s-val (CG-3M3J-13-ValEx-Rev4_2.xlsx / P676-11 A_Gas)'

      open(1, file='../data/P676/P676-11A_Gas.csv') ! FIXME install path
      allocate(c(31, 64), STAT=ierror)
      read(1, *)
      read(1, *)
      read(1, *, iostat=ierror, iomsg=iomsg) c
      c = transpose(c)
      if (ierror/=0) then
         write(*, *) 'cannot open ' // '../data/P676/P676-11A_Gas.csv' // trim(iomsg)
         return
      end if

      do i=1, 64
         write(line, '(I2, A)') i+21, ' A_gas'
         ne = ne + num_test(trim(line), c(i, 31), &
              p676_att_e2s(c(i, 7), c(i, 8), c(i, 11), c(i, 10), c(i, 9), c(i, 18), c(i, 3)), &
              rspec=5e-15)
      end do
    end block

  end function test_p676_att_e2s

end program test0
