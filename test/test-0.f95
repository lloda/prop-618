! (test-0.f95) -*- coding: utf-8; mode: f90 -*-
! tests for prop.f95, atmospheres.f95

! (c) lloda@sarc.name 2019
! This library is free software; you can redistribute it and/or modify it under
! the terms of the GNU Lesser General Public License as published by the Free
! Software Foundation; either version 3 of the License, or (at your option) any
! later version.

program test0

  use config
  use prop
  use atmospheres

  integer :: n

  n = 0
  n = n + prop_init()
  n = n + atmospheres_init()
  n = n + test_p835()
  n = n + test_p839_rain_height()
  n = n + test_p838_coeffs()
  n = n + test_p618_rain()
  n = n + test_p676_gas_specific()
  n = n + test_p676_gas()
  n = n + test_p840_Lred()
  n = n + test_p840_clouds()
  n = n + test_p618_scint()
  n = n + test_p1511_topoh()
  n = n + test_p618_att_tot_ppc()

  write(*, *) achar(10), n, ' errors.'
  stop n

contains

  real function rel_error(a, b) &
       result(e)
    real :: a, b
    if (a==b) then
       e = 0
    else if (a==0.) then
       e = b
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
         err = rel_error(expected, actual)
         if (err<=rspec) then
            write(*, '(A, A, ES12.6, A, ES12.6, A, ES8.2, A, ES8.2, A)') &
                 legend, ' = ', actual, ' (exp ', expected, ') rerror ', err, ' ≤ ', rspec, ''
         else
            ne = ne + 1
            write(*, '(A, A, ES20.14, A, ES20.14, A, ES8.2, A, ES8.2, A)') &
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
    write(hgs, '(A, F9.4)') 'hg = ', hg
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

    write(line, '(A, F8.3, A, F7.3, A)') 'lat =', lat, ' lon =', lon, ' hr '
    ne = num_test(trim(line), hspec, p839_rain_height(lat, lon), rspec=5e-15)

  end function test_p839_rain_height_ref

  integer function test_p839_rain_height() &
       result(ne)

    ne = 0

    write(*, *) achar(10), 'p839 rain height (CG-3M3J-13-ValEx-Rev4_2.xlsx / P839-4 Rain_Height)'
    block
      real, dimension(8) :: lat = (/ 3.133, 22.9, 23., 25.78, 28.717, 33.94, 41.9, 51.5 /)
      real, dimension(8) :: lon = (/ 101.7, -43.23, 30., -80.22, 77.3, 18.43, 12.49, -0.14 /)
      real, dimension(8) :: hr = (/ 4.9579744, 4.15877866666667, 4.52800000000000, 4.56946133333333, &
           5.25820404444445, 2.56330275555556, 3.04749333333333, 2.45273333333333 /)
      integer :: i
      do i=1, size(hr, 1)
         ne = ne + test_p839_rain_height_ref(lat(i), lon(i), hr(i))
      end do
    end block

    write(*, *) achar(10), 'p839, other cases'
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

  integer function test_p618_rain_ref(id, attpt, lat, lon, hs, fghz, el, taudeg, p, r001) &
       result(ne)

    integer :: id
    real :: attpt, fghz, taudeg, lat, lon, hs, el, p, r001
    character(len=200) :: line
    write(line, '(I2, A, F7.3, A, F7.3, A, F7.3, A, F7.3, A, F7.3, A, F7.3, A, F7.3, A, F7.3, A)') &
         id, ' f ', fghz, ' τ ', taudeg, ' lat ', lat, ' lon ', lon, &
         ' hₛ ', hs, ' el ', el, ' p ', p, ' R₀₀₁ ', r001, ' -> Ap '
    ne = 0
    ne = ne + num_test(trim(line), attpt, p618_rain(lat, lon, hs, fghz, el, taudeg, p, r001), rspec=5e-15)
  end function test_p618_rain_ref

  integer function test_p618_rain() &
       result(ne)

    character(256) :: iomsg
    integer ierror, i
    character(len=200) :: line
    real, allocatable :: x(:, :), c(:, :)

    write(*, *) achar(10), 'CG-3M3J-13-ValEx-Rev4_2.xlsx / R001 from P618-13 A_Rain'
    open(1, file=(test_datadir // 'P618/P618-13A_Rain.txt'), action='read', iostat=ierror)
    ne = ierror
    allocate(x(9, 64))
    allocate(c(64, 9))
    read(1, *, iostat=ierror, iomsg=iomsg) x
    c = transpose(x)
    if (ierror/=0) then
       write(*, *) 'cannot open ' // test_datadir // 'P618/P618-13A_Rain.txt ... ' // trim(iomsg)
       return
    end if

    do i=1, size(c, 1)
       block
         real lat, lon, r001
         lat = c(i, 1)
         lon = c(i, 2)
         r001 = c(i, 8)
         write(line, '(I2, A, F7.3, A, F7.3, A)') i+22, ' lat ', lat, ' lon ', lon, ' r001 '

         ! FIXME p837_rainfall_rate isn't the same method used for R₀₀₁ column
         ! (see comment in validation table)

         ne = ne + num_test(trim(line), r001, p837_rainfall_rate(lat, lon), rspec=5e-4)
       end block
    end do

    write(*, *) achar(10), 'p618_rain, CG-3M3J-13-ValEx-Rev4_2.xlsx / P618-13 A_Rain'
    do i=1, size(c, 1)
       ne = ne + test_p618_rain_ref(i+22, c(i, 9), c(i, 1), c(i, 2), c(i, 3), &
            c(i, 4), c(i, 5), c(i, 6), c(i, 7), c(i, 8))
    end do

    deallocate(c)
    deallocate(x)

  end function test_p618_rain

  ! ---------------------
  ! p676_gas_specific
  ! ---------------------

  integer function test_p676_gas_specific() &
       result(ne)

    real :: go, gw

    ne = 0

    ! 50% humidity - [g0, gw] = p676d11_ga(20.0, 1013.25, 11.508, 300.0)

    write(*, *) achar(10), 'p676 50%'
    call p676_gas_specific(0, 20.0, 1013.25, vapor_pressure(11.508, 300.), 300.0, go, gw)
    ne = ne + num_test('g₀', 0.0106557034883802, go, rspec=5e-15)
    ne = ne + num_test('gw', 0.145950346093096, gw, rspec=5e-15)

    ! 30% humidity - [g0, gw] = p676d11_ga(20.0, 1013.25, 6.9045, 300.0)

    write(*, *) achar(10), 'p676 30%'
    call p676_gas_specific(0, 20.0, 1013.25, vapor_pressure(6.9045, 300.), 300.0, go, gw)
    ne = ne + num_test('g₀', 0.0105877535759878, go, rspec=5e-15)
    ne = ne + num_test('gw', 0.0867126146205278, gw, rspec=5e-15)

    write(*, *) achar(10), 'CG-3M3J-13-ValEx-Rev4_2.xlsx / P676-11 SpAtt eq. 1 to 9'
    block
      real, dimension(5) :: fghz = (/ 12, 20, 60, 90, 130 /)
      real, dimension(5) :: got = (/ 0.0086982640687736, 0.0118835504778076, 14.6234747964861000, &
           0.0388697110724235, 0.0415090835995228 /)
      real, dimension(5) :: gwt = (/ 0.009535388220246, 0.097047304815112, 0.154841840636247, &
           0.341973394422181, 0.751844703646129 /)
      integer :: i

      do i=1, size(fghz, 1)
         call p676_gas_specific(0, fghz(i), 1013.25, vapor_pressure(7.5, 288.15), 288.15, go, gw)
         ne = ne + num_test('g₀', got(i), go, rspec=8e-15)
         ne = ne + num_test('gw', gwt(i), gw, rspec=8e-15)
      end do
    end block

    write(*, *) achar(10), 'CG-3M3J-13-ValEx-Rev4_2.xlsx / P676-11 SpAtt eq. 22-23'
    block
      real, dimension(5) :: fghz = (/ 12, 20, 60, 90, 130 /)
      real, dimension(5) :: got = (/ 0.0086982632089130, 0.0118835474853413, 14.6234770052713000, &
           0.0388696217227390, 0.0415090337921489 /)
      real, dimension(5) :: gwt = (/ 0.00948862715511772, 0.09694095785382280, 0.15334819552225700, &
           0.33927973759541800, 0.74582386063772800 /)
      integer :: i

      do i=1, size(fghz, 1)
         call p676_gas_specific(1, fghz(i), 1013.25, vapor_pressure(7.5, 288.15), 288.15, go, gw)
         ne = ne + num_test('g₀', got(i), go, rspec=8e-15)
         ne = ne + num_test('gw', gwt(i), gw, rspec=8e-15)
      end do
    end block

  end function test_p676_gas_specific

  ! ---------------------
  ! p676 total path
  ! ---------------------

  integer function test_p676_gas() &
       result(ne)

    character(256) :: iomsg
    integer :: ierror, i
    character(len=200) :: line
    real, allocatable :: c(:, :), x(:, :)

    write(*, *) achar(10), 'CG-3M3J-13-ValEx-Rev4_2.xlsx / P676-11 A_Gas'
    open(1, file=(test_datadir // 'P676/P676-11A_Gas.csv'), action='read', iostat=ierror)
    ne = ierror
    allocate(x(31, 64))
    allocate(c(64, 31))
    read(1, *)
    read(1, *)
    read(1, *, iostat=ierror, iomsg=iomsg) x
    c = transpose(x)
    if (ierror/=0) then
       write(*, *) 'cannot open ' // test_datadir // 'P676/P676-11A_Gas.csv ... ' // trim(iomsg)
       return
    end if

    ! Not many decimals in p835 (the segments of the interpolation don't even
    ! match) and I haven't programmed it the same way (cf P.619 §C.6), so just
    ! rspec to 1e-6.
    do i=1, 64
       write(line, '(I2, A, F10.7, A)') i+21, ' hₛ ', c(i, 3), ' Pdry'
       block
         real :: P, rho, T
         integer :: error
         call p835_ref(c(i, 3), P, rho, T, error)
         ne = ne + num_test(trim(line), c(i, 11), P, &
              rspec=5e-6)
       end block
    end do

    write(*, *) achar(10)

    do i=1, 64
       write(line, '(I2, A)') i+21, ' Vt'
       ne = ne + num_test(trim(line), c(i, 18), &
            p836_V(c(i, 1), c(i, 2), c(i, 5), c(i, 3)), &
            rspec=5e-15)
    end do

    write(*, *) achar(10)

    do i=1, 64
       write(line, '(I2, A)') i+21, ' A_gas'
       ne = ne + num_test(trim(line), c(i, 31), &
            p676_gas(c(i, 7), c(i, 8), c(i, 11), c(i, 10), c(i, 9), c(i, 18), c(i, 3)), &
            rspec=5e-15)
    end do

    deallocate(c)
    deallocate(x)

  end function test_p676_gas


  integer function test_p1511_topoh() &
       result(ne)

    integer :: i
    character(len=200) :: line
    real, dimension(4) :: lat = (/ 51.5, 41.9, 33.94, 25.78 /)
    real, dimension(4) :: lon = (/ -0.14, 12.49, 18.43, -80.22 /)
    real, dimension(4) :: hs = (/ 0.0691642239999998, 0.0567010449919997, 0., 0.0000751071354880102 /)

    ne = 0
    write(*, *) achar(10), 'Values for P1511 from CG-3M3J-13-ValEx-Rev4_2.xlsx / P676-11 Att_Tot'
    do i=1, 4
       write(line, '(A, F7.3, A, F7.3, A)') 'lat ', lat(i), ' lon ', lon(i), ' hₛ'
       ne = ne + num_test(trim(line), hs(i), p1511_topoh(lat(i), lon(i)), rspec=5e-15)
    end do

  end function test_p1511_topoh

  ! ---------------------
  ! p840_Lred
  ! ---------------------

  integer function test_p840_Lred() &
       result(ne)

    integer :: i
    character(len=200) :: line
    real, dimension(32) :: lat = (/ 3.133, 3.133, 3.133, 3.133, 22.9, 22.9, 22.9, 22.9, 23., 23., 23., &
         23., 25.78, 25.78, 25.78, 25.78, 28.717, 28.717, 28.717, 28.717, 33.94, 33.94, 33.94, 33.94, 41.9, &
         41.9, 41.9, 41.9, 51.5, 51.5, 51.5, 51.5 /)
    real, dimension(32) :: lon = (/ 101.7, 101.7, 101.7, 101.7, -43.23, -43.23, -43.23, -43.23, 30., &
         30., 30., 30., -80.22, -80.22, -80.22, -80.22, 77.3, 77.3, 77.3, 77.3, 18.43, 18.43, 18.43, 18.43, &
         12.49, 12.49, 12.49, 12.49, -0.14, -0.14, -0.14, -0.14/)
    real, dimension(32) :: p = (/ 0.1, 0.15, 0.3, 0.35, 0.1, 0.15, 0.3, 0.35, 0.1, 0.15, 0.3, 0.35, 0.1, &
         0.15, 0.3, 0.35, 0.1, 0.15, 0.3, 0.35, 0.1, 0.15, 0.3, 0.35, 0.1, 0.15, 0.3, 0.35, 0.1, 0.15, 0.3, 0.35/)

    ! the last value of Lred is missing from CG-3M3J-13-ValEx-Rev4_2.xlsx, so it's only a regression here.

    real, dimension(32) :: Lred = (/ 3.80525120796445, 3.74451232860507, 3.63095776597333, 3.59494611097836, &
         2.82993166918519, 2.61542833066688, 2.15256093108148, 2.0304247957397, 0.443821013333333, 0.367758573782867, &
         0.252495970370371, 0.230476913654263, 3.52927514027852,  3.36805310943291, 3.09003116666667, 2.98280225960877, &
         4.23072601368889, 4.00495166454066, 3.64194330426469, 3.55006805438526, 1.47628567661235, 1.34266249702586, &
         1.11763012935704, 1.06127889148178, 1.49845951814321, 1.41141171895762, 1.25417612763457, 1.21423952416498, &
         1.90329848661728, 1.8038036039028,  1.64128907698765, 1.59372131785357 /)

    ne = 0
    write(*, *) achar(10), 'CG-3M3J-13-ValEx-Rev4_2.xlsx / P840-7 Lred annual avg'
    do i=1,size(Lred, 1)
       write(line, '(A, F7.3, A, F7.3, A, F5.2, A)') &
            'lat ', lat(i), ' lon ', lon(i), ' p ', p(i), ' Lred '
       ne = ne + num_test(trim(line), Lred(i), p840_lred(lat(i), lon(i), p(i)), rspec=5e-15)
    end do

  end function test_p840_Lred

  ! ---------------------
  ! p840_clouds
  ! ---------------------

  integer function test_p840_clouds() &
       result(ne)

    integer :: i
    character(len=200) :: line
    real, dimension(64) :: freq = (/ 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, &
         14.25, 29., 29., 29., 29., 29., 29., 29., 29., 29., 29., 29., 29., 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, &
         14.25, 29., 29., 29., 29., 29., 29., 29., 29., 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, &
         14.25, 14.25, 14.25, 29., 29., 29., 29., 29., 29., 29., 29., 29., 29., 29., 29. /)
    real, dimension(64) :: el = (/ 31.07694309, 40.23202374, 46.35969261, 31.07694309, 40.23202374, 46.35969261, &
         31.07694309, 40.23202374, 46.35969261, 31.07694309, 40.23202374, 46.35969261, 31.07694309, 40.23202374, &
         46.35969261, 31.07694309, 40.23202374, 46.35969261, 31.07694309, 40.23202374, 46.35969261, 31.07694309, &
         40.23202374, 46.35969261, 22.27833468, 52.6789929, 22.27833468, 52.6789929, 22.27833468, 52.6789929, &
         22.27833468, 52.6789929, 22.27833468, 52.6789929, 22.27833468, 52.6789929, 22.27833468, 52.6789929, &
         22.27833468, 52.6789929, 48.24116215, 85.80457401, 20.14348033, 48.24116215, 85.80457401, 20.14348033, &
         48.24116215, 85.80457401, 20.14348033, 48.24116215, 85.80457401, 20.14348033, 48.24116215, 85.80457401, &
         20.14348033, 48.24116215, 85.80457401, 20.14348033, 48.24116215, 85.80457401, 20.14348033, 48.24116215, &
         85.80457401, 20.14348033 /)
    real, dimension(64) :: lat = (/ 51.50, 41.90, 33.94, 51.50, 41.90, 33.94, 51.50, 41.90, 33.94, 51.50, 41.90, &
         33.94, 51.50, 41.90, 33.94, 51.50, 41.90, 33.94, 51.50, 41.90, 33.94, 51.50, 41.90, 33.94, 22.90, 25.78, &
         22.90, 25.78, 22.90, 25.78, 22.90, 25.78, 22.90, 25.78, 22.90, 25.78, 22.90, 25.78, 22.90, 25.78, 28.72, &
         3.13, 9.05, 28.72, 3.13, 9.05, 28.72, 3.13, 9.05, 28.72, 3.13, 9.05, 28.72, 3.13, 9.05, 28.72, 3.13, 9.05, &
         28.72, 3.13, 9.05, 28.72, 3.13, 9.05 /)
    real, dimension(64) :: lon = (/ -0.14, 12.49, 18.43, -0.14, 12.49, 18.43, -0.14, 12.49, 18.43, -0.14, 12.49, &
         18.43, -0.14, 12.49, 18.43, -0.14, 12.49, 18.43, -0.14, 12.49, 18.43, -0.14, 12.49, 18.43, -43.23, -80.22, &
         -43.23, -80.22, -43.23, -80.22, -43.23, -80.22, -43.23, -80.22, -43.23, -80.22, -43.23, -80.22, -43.23, &
         -80.22, 77.30, 101.70, 38.70, 77.30, 101.70, 38.70, 77.30, 101.70, 38.70, 77.30, 101.70, 38.70, 77.30, &
         101.70, 38.70, 77.30, 101.70, 38.70, 77.30, 101.70, 38.70, 77.30, 101.70, 38.70 /)
    real, dimension(64) :: p = (/ 1.0, 1.0, 1.0, 0.5, 0.5, 0.5, 0.3, 0.3, 0.3, 0.2, 0.2, 0.2, 1.0, 1.0, 1.0, 0.5, &
         0.5, 0.5, 0.3, 0.3, 0.3, 0.2, 0.2, 0.2, 1.0, 1.0, 0.5, 0.5, 0.3, 0.3, 0.2, 0.2, 1.0, 1.0, 0.5, 0.5, 0.3, &
         0.3, 0.2, 0.2, 1.0, 1.0, 1.0, 0.5, 0.5, 0.5, 0.3, 0.3, 0.3, 0.2, 0.2, 0.2, 1.0, 1.0, 1.0, 0.5, 0.5, 0.5, &
         0.3, 0.3, 0.3, 0.2, 0.2, 0.2/)
    real, dimension(64) :: Ac = (/ 0.455170459072589, 0.263385167779463, 0.187794088495546, 0.534572163608203, &
         0.323038696870614, 0.239237966790239, 0.591367445049188, 0.361147414594881, 0.287229102870297, &
         0.624487479434006, 0.388639772242126, 0.320696768163836, 1.77247154423347, 1.02564370283047, &
         0.731285766461689, 2.08166837159296, 1.25793949602101, 0.931612498095673, 2.30283391121517, &
         1.40633800564994, 1.11849396498756, 2.43180607388744, 1.51339552804038, 1.24881983126982, &
         0.541832931262248, 0.533175055116892, 0.857467924537396, 0.639566064651145, 1.05602769261742, &
         0.72266885091129, 1.20844207942235, 0.760937894750366, 2.10994240344096, 2.07622791517559, &
         3.33905126319519, 2.4905233361613, 4.11225947945009, 2.81413248292522, 4.70577375121096, &
         2.96315531575139, 0.685600784011251, 0.622148172435011, 0.657648222808941, 0.831794456174625, &
         0.654899220497927, 0.718160404031759, 0.907730891752371, 0.677159302522241, 0.752444540774524, &
         0.958302613147588, 0.690306160732548, 0.771115493091996, 2.66978635397404, 2.42269661828394, &
         2.56093676074585, 3.23907664663614, 2.55023191759493, 2.79657621660043, 3.53477942907573, &
         2.63691452446004, 2.93008149046854, 3.73170990935901, 2.68810948144674, 3.00278772837749 /)
    real :: Lred

    ne = 0
    write(*, *) achar(10), 'CG-3M3J-13-ValEx-Rev4_2.xlsx / P840-7 A_Clouds'
    do i=1, size(freq, 1)
       Lred = p840_Lred(lat(i), lon(i), p(i))
       write(line, '(I2, A, F7.3, A, F7.3, A, F7.3, A, F7.3, A, F5.2, A)') &
            20+i, ' freq ', freq(i), ' el ', el(i), ' lat ', lat(i), ' lon ', lon(i), ' p ', p(i), ' Ac '
       ne = ne + num_test(trim(line), Ac(i), p840_clouds(freq(i), el(i), Lred), rspec=5e-15)
    end do

  end function test_p840_clouds

  ! ---------------------
  ! p840_clouds
  ! ---------------------

  integer function test_p618_scint() &
       result(ne)

    integer :: i
    character(len=200) :: line
    real, dimension(64) :: freq = (/ 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, &
         14.25, 29., 29., 29., 29., 29., 29., 29., 29., 29., 29., 29., 29., 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, &
         14.25, 29., 29., 29., 29., 29., 29., 29., 29., 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, 14.25, &
         14.25, 14.25, 14.25, 29., 29., 29., 29., 29., 29., 29., 29., 29., 29., 29., 29. /)
    real, dimension(64) :: el = (/ 31.07694309, 40.23202374, 46.35969261, 31.07694309, 40.23202374, 46.35969261, &
         31.07694309, 40.23202374, 46.35969261, 31.07694309, 40.23202374, 46.35969261, 31.07694309, 40.23202374, &
         46.35969261, 31.07694309, 40.23202374, 46.35969261, 31.07694309, 40.23202374, 46.35969261, 31.07694309, &
         40.23202374, 46.35969261, 22.27833468, 52.6789929, 22.27833468, 52.6789929, 22.27833468, 52.6789929, &
         22.27833468, 52.6789929, 22.27833468, 52.6789929, 22.27833468, 52.6789929, 22.27833468, 52.6789929, &
         22.27833468, 52.6789929, 48.24116215, 85.80457401, 20.14348033, 48.24116215, 85.80457401, 20.14348033, &
         48.24116215, 85.80457401, 20.14348033, 48.24116215, 85.80457401, 20.14348033, 48.24116215, 85.80457401, &
         20.14348033, 48.24116215, 85.80457401, 20.14348033, 48.24116215, 85.80457401, 20.14348033, 48.24116215, &
         85.80457401, 20.14348033 /)
    real, dimension(64) :: lat = (/ 51.5, 41.9, 33.94, 51.5, 41.9, 33.94, 51.5, 41.9, 33.94, 51.5, 41.9, 33.94, 51.5, &
         41.9, 33.94, 51.5, 41.9, 33.94, 51.5, 41.9, 33.94, 51.5, 41.9, 33.94, 22.9, 25.78, 22.9, 25.78, 22.9, 25.78, &
         22.9, 25.78, 22.9, 25.78, 22.9, 25.78, 22.9, 25.78, 22.9, 25.78, 28.717, 3.133, 9.05, 28.717, 3.133, 9.05, &
         28.717, 3.133, 9.05, 28.717, 3.133, 9.05, 28.717, 3.133, 9.05, 28.717, 3.133, 9.05, 28.717, 3.133, 9.05, 28.717, &
         3.133, 9.05 /)
    real, dimension(64) :: lon = (/ -0.14, 12.49, 18.43, -0.14, 12.49, 18.43, -0.14, 12.49, 18.43, -0.14, 12.49, &
         18.43, -0.14, 12.49, 18.43, -0.14, 12.49, 18.43, -0.14, 12.49, 18.43, -0.14, 12.49, 18.43, -43.23, -80.22, &
         -43.23, -80.22, -43.23, -80.22, -43.23, -80.22, -43.23, -80.22, -43.23, -80.22, -43.23, -80.22, -43.23, &
         -80.22, 77.30, 101.70, 38.70, 77.30, 101.70, 38.70, 77.30, 101.70, 38.70, 77.30, 101.70, 38.70, 77.30, &
         101.70, 38.70, 77.30, 101.70, 38.70, 77.30, 101.70, 38.70, 77.30, 101.70, 38.70 /)
    real, dimension(64) :: p = (/ 1., 1., 1., 0.1, 0.1, 0.1, 0.01, 0.01, 0.01, 0.001, 0.001, 0.001, 1., 1., 1., 0.1, 0.1, &
         0.1, 0.01, 0.01, 0.01, 0.001, 0.001, 0.001, 1., 1., 0.1, 0.1, 0.01, 0.01, 0.001, 0.001, 1., 1., 0.1, 0.1, 0.01, 0.01, &
         0.001, 0.001, 1., 1., 1., 0.1, 0.1, 0.1, 0.01, 0.01, 0.01, 0.001, 0.001, 0.001, 1., 1., 1., 0.1, 0.1, 0.1, 0.01, 0.01, &
         0.01, 0.001, 0.001, 0.001 /)
    real, dimension(64) :: As = (/ 0.261932335215315, 0.224052264243819, 0.232799419729435, 0.422846099815923, 0.361695038577605, &
         0.375815863249885, 0.628288361403135, 0.537426697832840, 0.558408208124339, 0.910214864873219, 0.778581618247270, &
         0.808977983559787, 0.388493191478675, 0.331152693288537, 0.343398987665463, 0.627157508777074, 0.534590831198794, &
         0.554360432421279, 0.931865668626848, 0.794324926968103, 0.823699705080223, 1.350013840388400, 1.150755609177660, &
         1.193311482137480, 0.620097436204843, 0.266474898581355, 1.001043961180020, 0.430179311276501, 1.487407050310020, &
         0.639184456730478, 2.154838590811830, 0.926000272570210, 0.923410291353419, 0.392379988141146, 1.490692013674870, &
         0.633432094189190, 2.214953485526400, 0.941188798221228, 3.208850762453130, 1.363520458790480, 0.215641295105377, &
         0.221671292813958, 0.485336453602963, 0.348116930731781, 0.357851357032667, 0.783494814933050, 0.517251586526099, &
         0.531715541029748, 1.164160373375640, 0.749353500491186, 0.770307742528505, 1.686544176270300, 0.317912780109318, &
         0.324868808692722, 0.723516143303121, 0.513217198023143, 0.524446546832950, 1.167996227339010, 0.762566788555551, &
         0.779251982450942, 1.735474055736420, 1.104746910879880, 1.128919110207210, 2.514218597978350 /)
    real :: Nwet

    ne = 0
    write(*, *) achar(10), 'CG-3M3J-13-ValEx-Rev4_2.xlsx / P618-13 A_Scint'
    do i=1, size(freq, 1)
       Nwet = p453_Nwet(lat(i), lon(i), 50.)
       write(line, '(I2, A, F8.4, A, F8.4, A, F8.4, A, F8.4, A, F5.2, A, F8.4, A)') &
            20+i, ' freq ', freq(i), ' el ', el(i), ' lat ', lat(i), ' lon ', lon(i), ' p ', p(i), ' Nwet ', Nwet, ' As '
       ne = ne + num_test(trim(line), As(i), p618_scint(freq(i), el(i), sqrt(0.65), p(i), Nwet), rspec=5e-15)
    end do

  end function test_p618_scint

  ! ---------------------
  ! p618 Att_Tot, ppc dependence
  ! ---------------------

  integer function test_p618_att_tot_ppc() &
       result(ne)

    real, dimension(16) :: ppc = (/ 5., 3., 2., 1., 0.5, 0.3, 0.2, 0.1, 0.05, 0.03, 0.02, 0.01, &
         0.005, 0.003, 0.002, 0.001 /)
    real, dimension(16) :: Asc = (/ 0.141574488517842, 0.170742323439664, 0.193795402589708, &
         0.233479129826238, 0.274178424688772, 0.305276136650130, 0.330859051734132, &
         0.376913141916157, 0.426564034106947, 0.465910863575482, 0.499039477933432, &
         0.560038606076536, 0.627215770611167, 0.681130958054523, 0.726821135026408, 0.811339976146176 /)
    real, dimension(16) :: Ar = (/ 0.360661220915252, 0.536506775961356, 0.726360719406632, &
         1.188971244052750, 1.885466923135720, 2.595255915525150, 3.303669783964360, &
         4.867073651326550, 6.946527690196690, 8.847377369496150, 10.589374219866200, &
         14.040865181968700, 18.036240114082200, 21.255841242503600, 23.920670290858200, 28.546259160579700 /)
    real, dimension(16) :: Ac = (/ 0.281091319971039, 0.351733314918248, 0.402569159584718, &
         0.483383045624107, 0.483383045624107, 0.483383045624107, 0.483383045624107, &
         0.483383045624107, 0.483383045624107, 0.483383045624107, 0.483383045624107, &
         0.483383045624107, 0.483383045624107, 0.483383045624107, 0.483383045624107, 0.483383045624107 /)
    real, dimension(16) :: Ag = (/ 0.606737084635778, 0.634084339313449, 0.650927649679022, &
         0.673969685311903, 0.674043382903338, 0.674079725587193, 0.674113266072447, &
         0.674174047627131, 0.674174047627131, 0.674174047627131, 0.674174047627131, &
         0.674174047627131, 0.674174047627131, 0.674174047627131, 0.674174047627131, 0.674174047627131 /)
    real, dimension(16) :: At = (/ 1.263920209444300, 1.538586080638130, 1.796370502790570, &
         2.362543454581400, 3.058707716317630, 3.767817169636740, 4.475591510940940, &
         6.037890145864740, 8.116319599647650, 10.016559334970700, 11.758171244532400, &
         15.209215492942300, 19.204415319678400, 22.424066277970300, 25.089048370715400, 29.715151978425200 /)

    real, parameter :: lat = 46.2208
    real, parameter :: lon = 6.137
    real, parameter :: hs = 0.412
    real, parameter :: f = 19.5
    real, parameter :: el = 36.6141654045094
    real, parameter :: Deff = 1.2 * sqrt(0.65)
    real, parameter :: taudeg = 0

    real :: xasc, xar, xac, xag, xat
    integer :: i
    character(len=200) :: line

    write(*, *) achar(10), 'CG-3M3J-13-ValEx-Rev4_2.xlsx / P618-13 Att_Tot Sample total attenuation CDF'
    do i=1, size(At, 1)
       xasc = Asc(i)
       xar = Ar(i)
       xac = Ac(i)
       xag = Ag(i)
       xat = xag + sqrt((xar +xac)**2 + xasc**2) ! FIXME its own p618_function
       write(line, '(I2, A, ES12.6, A, ES12.6, A, ES12.6, A, ES12.6, A, ES12.6, A)') &
            99+i, ' ppc ', ppc(i), ' Asc ', xasc, ' Ar ', xar, ' Ac ', xac, ' Ag ', xag, ' At '
       ne = ne + num_test(trim(line), At(i), xat, rspec=5e-15)
    end do

  end function test_p618_att_tot_ppc


end program test0
