! (prop.f90) -*- coding: utf-8; mode: f90 -*-
! propagation functions following ITU models

! (c) lloda@sarc.name 2019
! This library is free software; you can redistribute it and/or modify it under
! the terms of the GNU Lesser General Public License as published by the Free
! Software Foundation; either version 3 of the License, or (at your option) any
! later version.

module prop

  use config
  use iso_c_binding

  real, allocatable, save :: p839h0(:, :)
  real, allocatable, save :: p837R001(:, :)
  real, allocatable, save :: p1510Ta(:, :)
  real, allocatable, save :: p1511topo(:, :)

  ! these are used both for p840ap and for p453Nwet.
  real, parameter, dimension(18) :: p840p = (/ 0.1, 0.2, 0.3, 0.5, 1., 2., 3., 5., &
       10., 20., 30., 50., 60., 70., 80., 90., 95., 99. /)

  real, allocatable, save :: p840Lred(:, :, :)  ! total columnar content of cloud liquid water reduced to 273.15 K
  real, allocatable, save :: p453Nwet(:, :, :)  ! wet term of surface refractivity
  real, allocatable, save :: p836rho(:, :, :)   ! surface water vapor density
  real, allocatable, save :: p836V(:, :, :)     ! total columnar water vapor content
  real, allocatable, save :: p836vsch(:, :, :)  ! water vapor scale height

contains

  integer function try_open(port, fname) &
       result(ierror)

    integer, intent(in) :: port
    character(len=*), intent(in) :: fname
    character(256) :: iomsg

    open(port, file=trim(datadir // fname), action='read', iostat=ierror, iomsg=iomsg)
    if (ierror/=0) then
       open(port, file=trim(test_datadir // fname), action='read', iostat=ierror, iomsg=iomsg)
       if (ierror/=0) then
          write(*, *) 'cannot open [' // trim(datadir // fname) // '] ... ', trim(iomsg)
          write(*, *) 'cannot open [' // trim(test_datadir // fname) // '] ... ', trim(iomsg)
          return
       end if
    end if

  end function try_open


  integer function load_lat_lon(fname, m, n, a) &
       result(ierror)

    character(len=*), intent(in) :: fname
    integer, intent(in) :: m, n
    real, allocatable, dimension(:, :), intent(out) :: a

    real, allocatable, dimension(:, :) :: x

    allocate(x(n, m), STAT=ierror)

    ierror = try_open(1, fname)
    if (ierror/=0) then
       return
    end if

    read(1, *) x
    allocate(a(m, n), STAT=ierror)
    a = transpose(x)
    deallocate(x)

  end function load_lat_lon

  integer function load_p_lat_lon(path1, path2, p, m, n, a) &
       result(ierror)

    character(len=*), intent(in) :: path1, path2
    real, intent(in), dimension(:) :: p
    integer, intent(in) :: m, n
    real, allocatable, dimension(:, :, :), intent(out) :: a

    real, allocatable, dimension(:, :) :: x

    block
      integer :: i
      character(64) :: fname

      allocate(x(n, m), STAT=ierror)
      allocate(a(size(p, 1), m, n), STAT=ierror)
      if (ierror/=0) then
         write(*, *) 'cannot allocate array for [' // path1 // '***' // path2 // ']'
         return
      end if
      do i=1, size(p, 1)
         if (p(i)>=1.) then
            write(fname, '(I2)') int(p(i))
         else
            write(fname, '(A, I1)') '0', int(p(i)*10)
         end if

         ierror = try_open(1, (path1 // trim(adjustl(fname)) // path2))
         if (ierror/=0) then
            return
         end if

         read(1, *) x
         a(i, :, :) = transpose(x)
      end do
    end block
    deallocate(x)

  end function load_p_lat_lon


  ! Bilinear interpolate in x, y table. x and y are indices in the sizes of the table
  ! [0 ... n], so the steps of x and y are accounted for in the caller.

  real function lookup_xy(x, y, table) &
       result(z)

    real, intent(in) :: x, y
    real, intent(in), dimension(:, :) :: table

    real :: dx, dy
    integer :: ix, iy

    ix = max(0, min(size(table, 1)-2, int(floor(x))))     ! x = end-of-table will use dx = 1.
    iy = max(0, min(size(table, 2)-2, int(floor(y))))     ! y = end-of-table will use dy = 1.
    dx = x-ix
    dy = y-iy

    z =  &
         + table(ix+1, iy+1)*(1-dx)*(1-dy) &
         + table(ix+2, iy+1)*dx*(1-dy) &
         + table(ix+1, iy+2)*(1-dx)*dy &
         + table(ix+2, iy+2)*dx*dy

  end function lookup_xy


  ! Helper function for lookup_pxy - find the indices/deltas, but don't interpolate.

  subroutine lookup_pxy_find(plist, m, n, p, x, y, ip, ix, iy, dp, dx, dy)

    real, intent(in) :: p, x, y
    real, intent(in), dimension(:) :: plist
    integer, intent(in) :: m, n
    integer, intent(out) :: ip, ix, iy
    real, intent(out) :: dp, dx, dy

    ix = max(0, min(m-2, int(floor(x))))     ! x = end-of-table will use dx = 1.
    iy = max(0, min(n-2, int(floor(y))))     ! y = end-of-table will use dy = 1.
    dx = x-ix
    dy = y-iy

    if (p<plist(lbound(plist, 1)) .or. p>plist(ubound(plist, 1))) then
       write(*, *) 'p', p, 'plist', plist
       stop 106
    end if
    do ip=2, size(plist, 1)
       if (p<=plist(ip)) then
          exit
       end if
    end do
    dp = (log(p) - log(plist(ip-1))) / (log(plist(ip)) - log(plist(ip-1)))

  end subroutine lookup_pxy_find


  ! Interpolate in p, x, y table, as described in P453-13 §2.2 and P.840-7 §3.1.
  ! Interpolation is bilinear in x, y then linear in the result and in the log of p.
  ! x and y are indices in the sizes of the table [0 ... n], so the steps of x and y are
  ! accounted for in the caller.

  real function lookup_pxy(p, x, y, plist, table) &
       result(z)

    real, intent(in) :: p, x, y
    real, intent(in), dimension(:) :: plist
    real, intent(in), dimension(:, :, :) :: table

    real :: dx, dy, dp
    integer :: ip, ix, iy, i
    real, dimension(2) :: zz

    call lookup_pxy_find(plist, size(table, 2), size(table, 3), p, x, y, ip, ix, iy, dp, dx, dy)
    do i=1, 2
       zz(i) = &
            + table(ip+i-2, ix+1, iy+1)*(1-dx)*(1-dy) &
            + table(ip+i-2, ix+2, iy+1)*dx*(1-dy) &
            + table(ip+i-2, ix+1, iy+2)*(1-dx)*dy &
            + table(ip+i-2, ix+2, iy+2)*dx*dy
    end do
    z = zz(1)*(1.-dp) + zz(2)*dp

  end function lookup_pxy


  integer(C_INT32_T) function prop_init() &
       bind(c, name='prop_init') &
       result(ierror)

    logical, save :: done = .false.
    ierror = 1

    if (.not. done) then

       ! according to ITU-R P.839-4, lat +90:-1.5:-90 (or th 0:1.5:180) and lon 0:1.5:360.
       ! used by p839_rain_height.

       ierror = load_lat_lon('P839/h0.txt', 121, 241, p839h0)
       if (ierror/=0) then
          return
       end if

       ! according to ITU-R P.837-7, lat -90:0.125:+90 and lon -180:0.125:+180.
       ! used by p837_rainfall_rate.

       ierror = load_lat_lon('P837/R001.txt', 1441, 2881, p837R001)
       if (ierror/=0) then
          return
       end if

       ! according to ITU-R P.840-7, lat +90:1.125:-90 (or th: -90:1.125:+90) and lon 0:1.125:+360.
       ! used by p840_Lred.

       ierror = load_p_lat_lon('P840/Lred Annual Maps/Lred_', '_v4.txt', p840p, 161, 321, p840Lred)
       if (ierror/=0) then
          return
       end if

       ! according to ITU-R P.453-13 §2.2, lat -90:0.75:+90 and lon -180:0.75:+180
       ! used by p453_Nwet.

       ierror = load_p_lat_lon('P453/P.453_NWET_Maps_Annual/NWET_Annual_', '.TXT', p840p, 241, 481, p453Nwet)
       if (ierror/=0) then
          return
       end if

       ! according to ITU-R P.1510-1 Annex 1, lat -90:0.75:+90 and lon -180:0.75:+180
       ! used by p1510_temp.

       ierror = load_lat_lon('P1510/T_Annual.TXT', 241, 481, p1510Ta)
       if (ierror/=0) then
          return
       end if

       ! according to ITU-R P.1511-1 Annex 1 and included in P.836-6 data package
       ! lat +90.5:-0.5:-90.5 and lon -0.5:0.5:360.5 for bicubic interpolation.

       ierror = load_lat_lon('P836/TOPO_0DOT5.txt', 363, 723, p1511topo)
       if (ierror/=0) then
          return
       end if

       ! according to ITU-R P.836-6 Anexes 1 and 2
       ! lat +90:-1.125:-90 and lon 0:+1.125:360

       ierror = load_p_lat_lon('P836/P_836_Maps_annual/Surface Water Vapor Density/RHO Annual Maps/RHO_', &
            '_v4.txt', p840p, 161, 321, p836rho)
       if (ierror/=0) then
          return
       end if

       ierror = load_p_lat_lon('P836/P_836_Maps_annual/Total Columnar Water Content/V Annual Maps/V_', &
            '_v4.txt', p840p, 161, 321, p836V)
       if (ierror/=0) then
          return
       end if

       ierror = load_p_lat_lon('P836/P_836_Maps_annual/Total Columnar Water Content/VSCH Annual Maps/VSCH_', &
            '_v4.txt', p840p, 161, 321, p836vsch)
       if (ierror/=0) then
          return
       end if

       done = .true.
    end if

  end function prop_init


  elemental real function deg2rad(xdeg) &
       result(x)
    real, intent(in) :: xdeg

    x = xdeg*(acos(-1.)/180.)

  end function deg2rad


  elemental real function rad2deg(x) &
       result(xdeg)
    real, intent(in) :: x

    xdeg = x*(180./acos(-1.))

  end function rad2deg


  ! According to ITU-R P.839-4, lat +90:-1.5:-90 lon 0:1.5:360

  real(C_DOUBLE) function p839_rain_height(lat, lon) &
       bind(c, name='p839_rain_height') &
       result(h)

    real(C_DOUBLE), intent(in) :: lat  ! latitude (°)
    real(C_DOUBLE), intent(in) :: lon  ! longitude (°)

    h = 0.36 + lookup_xy(max(0., min(180., (90.-lat)))/1.5, modulo(lon, 360.)/1.5, p839h0)

  end function p839_rain_height


  ! According to ITU-R P.837-7, lat -90:0.125:+90 and lon -180:0.125:+180.

  real(C_DOUBLE) function p837_rainfall_rate(lat, lon) &
       bind(c, name='p837_rainfall_rate') &
       result(R001)

    real(C_DOUBLE), intent(in) :: lat  ! latitude (°)
    real(C_DOUBLE), intent(in) :: lon  ! longitude (°)

    R001 = lookup_xy(max(0., min(180., lat+90.))/0.125, modulo(lon+180., 360.)/0.125, p837R001)

  end function p837_rainfall_rate


  ! According to ITU-R P.1510-1, lat -90:0.75:+90 and lon -180:0.75:+180

  real(C_DOUBLE) function p1510_temp(lat, lon) &
       bind(c, name='p1510_temp') &
       result(h)

    real(C_DOUBLE), intent(in) :: lat  ! latitude (°)
    real(C_DOUBLE), intent(in) :: lon  ! longitude (°)

    h = lookup_xy(max(0., min(180., lat+90.))/0.75, modulo(lon+180., 360.)/0.75, p1510Ta)

  end function p1510_temp


  real function p838_coeff(logf, a, b, c, m, x) &
       result(coeff)

    real, intent(in), dimension(:) :: a, b, c
    real, intent(in) :: logf, m, x

    coeff = sum(a * exp(-(((logf-b)/c)**2))) + m * logf + x

  end function p838_coeff


  subroutine p838_coeffs(freq, kh, ah, kv, av) &
       bind(c, name='p838_coeffs')

    real(C_DOUBLE), intent(in) :: freq
    real(C_DOUBLE), intent(out) :: kv, kh, av, ah

    real, dimension(4) :: &
         kha = (/ -5.33980, -0.35351, -0.23789, -0.94158 /), &
         khb = (/ -0.10008, 1.26970, 0.86036, 0.64552 /), &
         khc = (/ 1.13098, 0.45400, 0.15354, 0.16817 /), &
         kva = (/ -3.80595, -3.44965, -0.39902, 0.50167 /), &
         kvb = (/ 0.56934, -0.22911, 0.73042, 1.07319 /), &
         kvc = (/ 0.81061, 0.51059, 0.11899, 0.27195 /)

    real :: &
         khm = -0.18961, khx = 0.71147, &
         kvm = -0.16398, kvx = 0.63297

    real, dimension(5) :: &
         aha = (/ -0.14318, 0.29591, 0.32177, -5.37610, 16.1721 /), &
         ahb = (/ 1.82442, 0.77564, 0.63773, -0.96230, -3.29980  /), &
         ahc = (/ -0.55187, 0.19822, 0.13164, 1.47828, 3.43990 /), &
         ava = (/ -0.07771,  0.56727, -0.20238, -48.2991, 48.5833 /), &
         avb = (/ 2.33840, 0.95545, 1.14520, 0.791669, 0.791459 /), &
         avc = (/ -0.76284, 0.54039, 0.26809, 0.116226, 0.116479 /)

    real :: &
         ahm = 0.67849, ahx = -1.95537, &
         avm = -0.053739, avx = 0.83433

    real :: logf

    logf = log10(freq)
    kh = 10.**p838_coeff(logf, kha, khb, khc, khm, khx)
    kv = 10.**p838_coeff(logf, kva, kvb, kvc, kvm, kvx)
    ah = p838_coeff(logf, aha, ahb, ahc, ahm, ahx)
    av = p838_coeff(logf, ava, avb, avc, avm, avx)

  end subroutine p838_coeffs


  ! Specific attenuation exceeded p*100% on an average year, for rain and clouds in Earth to space links.
  ! Equation & table numbers from ITU-R P.618-13 except where indicated.

  real(C_DOUBLE) function p618_rain(lat, lon, hs, freq, eldeg, taudeg, ppc, r001) &
       bind(c, name='p618_rain') &
       result(att)

    real(C_DOUBLE), intent(in) :: freq     ! frequency (GHz)
    real(C_DOUBLE), intent(in) :: taudeg   ! polarization parameter (H 0, V 90, L/R 45)
    real(C_DOUBLE), intent(in) :: hs       ! height above mean sea for Earth station
    real(C_DOUBLE), intent(in) :: lat      ! latitude (°)
    real(C_DOUBLE), intent(in) :: lon      ! longitude (°)
    real(C_DOUBLE), intent(in) :: eldeg    ! elevation angle (°)
    real(C_DOUBLE), intent(in) :: ppc      ! probability (%)
    real(C_DOUBLE), intent(in) :: r001     ! point rainfall rate for 0.01% of average year (mm/h)

    real :: hr, ls, lg, lr, gr, el, horiz001, nu001, le, beta, att001
    real, parameter :: Re = 8500           ! effective Earth radius (8500 km)

    if (.not. ((.001 <= ppc) .and. (ppc <= 5))) then
       stop 100
    end if

    el = deg2rad(eldeg)

    ! step 1 - rain height (P839)

    hr = p839_rain_height(lat, lon)
    if (hr<=hs) then
       att = 0
       return
    end if

    ! step 2 - slant path below rain
    ! step 3 - projection of slant path

    block
      if (eldeg <= 0.) then
         stop 99
      else if (eldeg > 5.) then
         ls = (hr-hs)/sin(el)
      else
         ls = 2*(hr-hs)/(sqrt(sin(el)**2 + 2*(hr-hs)/Re) + sin(el))
      end if
      lg = ls*cos(el)
    end block

    ! step 4 - rainfall rate (P837)
    ! step 5 - specific attenuation (P838)

    block
      real :: tau, kh, ah, kv, av
      real :: k, a

      tau = deg2rad(taudeg)
      call p838_coeffs(freq, kh, ah, kv, av)
      k = (kh + kv + (kh-kv)*(cos(el)**2.)*cos(2.*tau)) / 2.
      a = (kh*ah + kv*av + (kh*ah - kv*av)*(cos(el)**2.)*cos(2.*tau)) / (2.*k)
      gr = k * (r001 ** a)
    end block

    ! step 6 - horizontal reduction factor

    horiz001 = 1./(1 + 0.78*sqrt(lg*gr/freq) - 0.38*(1-exp(-2*lg)))

    ! step 7 - vertical adjustment factor

    if (atan2(hr-hs, lg*horiz001) > el) then
       lr = lg*horiz001 / cos(el)
    else
       lr = (hr-hs) / sin(el)
    end if

    block
      real :: chi
      chi = max(0., 36.-abs(lat))
      nu001 = 1./(1. + sqrt(sin(el)) * (31. * (1. - exp(-eldeg/(1+chi))) * sqrt(lr*gr)/(freq**2) - 0.45))
    end block

    ! step 8 - effective path length

    le = lr * nu001

    ! step 9 - predicted attenuation for 0.01%

    att001 = gr * le

    ! step 10 - predicted attenuation for ppc (%) (0.001 ≤ ppc ≤ 5)

    block
      if ((ppc >= 1.) .or. (abs(lat) >= 36.)) then
         beta = 0.
      else if ((ppc < 1.) .and. (abs(lat) < 36.) .and. (eldeg >= 25.)) then
         beta = -.005*(abs(lat)-36.)
      else
         beta = -.005*(abs(lat)-36.) + 1.8 - 4.25*sin(el)
      end if
      att = att001 * ((ppc/.01) ** (-0.655 -0.033*log(ppc) +0.045*log(att001) +beta*(1-ppc)*sin(el)))
    end block

    ! ! debug intermediate values
    ! write(*, *) 'hᵣ', hr
    ! write(*, *) 'R₀₀₁', r001
    ! write(*, *) 'Lₛ', ls
    ! write(*, *) 'Lg', lg
    ! write(*, *) 'γᵣ', gr
    ! write(*, *) 'r₀₀₁', horiz001
    ! write(*, *) 'ν₀₀₁', nu001
    ! write(*, *) 'Lₑ', le
    ! write(*, *) 'A₀₀₁', att001
    ! write(*, *) 'Beta', beta

  end function p618_rain


  real(C_DOUBLE) function vapor_pressure(rho, temp) &
       bind(c, name='p676_vapor_pressure') &
       result(e)

    real(C_DOUBLE), intent(in) :: rho   ! water vapor density (g/m³)
    real(C_DOUBLE), intent(in) :: temp  ! temperature (K)

    e = rho * temp/216.7;               ! P676 (4)

  end function vapor_pressure


  ! Equation & table numbers from ITU-R P.676-11.

  subroutine p676_gas_specific(scut, f, P, e, temp, go, gw) &
       bind(c, name='p676_gas_specific')

    integer(C_INT32_T), intent(in) :: scut   ! use Annex 1 (20) if 0, else Annex 2 (22-23)
    real(C_DOUBLE), intent(in) :: f          ! frequency (GHz)
    real(C_DOUBLE), intent(in) :: P          ! dry air pressure (hPa)
    real(C_DOUBLE), intent(in) :: e          ! vapor part. pressure e(P) (hPa) (4)
    real(C_DOUBLE), intent(in) :: temp       ! temperature (K)
    real(C_DOUBLE), intent(out) :: go        ! Specific attenuation for dry air, γₒ (dB/km)
    real(C_DOUBLE), intent(out) :: gw        ! Specific attenuation for water vapor, γw (dB/km)

    real :: th
    th = 300.0/temp;

    block
      real, dimension(0:43, 0:6) :: dry = transpose(reshape( (/ &
                                ! spectroscopic data for oxigen (Table 1)
                                ! f0        a1    a2     a3   a4     a5     a6
           50.474214, 0.975, 9.651, 6.690, 0.0, 2.566, 6.850, &
           50.987745, 2.529, 8.653, 7.170, 0.0, 2.246, 6.800, &
           51.503360, 6.193, 7.709, 7.640, 0.0, 1.947, 6.729, &
           52.021429, 14.320, 6.819, 8.110, 0.0, 1.667, 6.640, &
           52.542418, 31.240, 5.983, 8.580, 0.0, 1.388, 6.526, &
           53.066934, 64.290, 5.201, 9.060, 0.0, 1.349, 6.206, &
           53.595775, 124.600, 4.474, 9.550, 0.0, 2.227, 5.085, &
           54.130025, 227.300, 3.800, 9.960, 0.0, 3.170, 3.750, &
           54.671180, 389.700, 3.182, 10.370, 0.0, 3.558, 2.654, &
           55.221384, 627.100, 2.618, 10.890, 0.0, 2.560, 2.952, &
           55.783815, 945.300, 2.109, 11.340, 0.0, -1.172, 6.135, &
           56.264774, 543.400, 0.014, 17.030, 0.0, 3.525, -0.978, &
           56.363399, 1331.800, 1.654, 11.890, 0.0, -2.378, 6.547, &
           56.968211, 1746.600, 1.255, 12.230, 0.0, -3.545, 6.451, &
           57.612486, 2120.100, 0.910, 12.620, 0.0, -5.416, 6.056, &
           58.323877, 2363.700, 0.621, 12.950, 0.0, -1.932, 0.436, &
           58.446588, 1442.100, 0.083, 14.910, 0.0, 6.768, -1.273, &
           59.164204, 2379.900, 0.387, 13.530, 0.0, -6.561, 2.309, &
           59.590983, 2090.700, 0.207, 14.080, 0.0, 6.957, -0.776, &
           60.306056, 2103.400, 0.207, 14.150, 0.0, -6.395, 0.699, &
           60.434778, 2438.000, 0.386, 13.390, 0.0, 6.342, -2.825, &
           61.150562, 2479.500, 0.621, 12.920, 0.0, 1.014, -0.584, &
           61.800158, 2275.900, 0.910, 12.630, 0.0, 5.014, -6.619, &
           62.411220, 1915.400, 1.255, 12.170, 0.0, 3.029, -6.759, &
           62.486253, 1503.000, 0.083, 15.130, 0.0, -4.499, 0.844, &
           62.997984, 1490.200, 1.654, 11.740, 0.0, 1.856, -6.675, &
           63.568526, 1078.000, 2.108, 11.340, 0.0, 0.658, -6.139, &
           64.127775, 728.700, 2.617, 10.880, 0.0, -3.036, -2.895, &
           64.678910, 461.300, 3.181, 10.380, 0.0, -3.968, -2.590, &
           65.224078, 274.000, 3.800, 9.960, 0.0, -3.528, -3.680, &
           65.764779, 153.000, 4.473, 9.550, 0.0, -2.548, -5.002, &
           66.302096, 80.400, 5.200, 9.060, 0.0, -1.660, -6.091, &
           66.836834, 39.800, 5.982, 8.580, 0.0, -1.680, -6.393, &
           67.369601, 18.560, 6.818, 8.110, 0.0, -1.956, -6.475, &
           67.900868, 8.172, 7.708, 7.640, 0.0, -2.216, -6.545, &
           68.431006, 3.397, 8.652, 7.170, 0.0, -2.492, -6.600, &
           68.960312, 1.334, 9.650, 6.690, 0.0, -2.773, -6.650, &
           118.750334, 940.300, 0.010, 16.640, 0.0, -0.439, 0.079, &
           368.498246, 67.400, 0.048, 16.400, 0.0, 0.000, 0.000, &
           424.763020, 637.700, 0.044, 16.400, 0.0, 0.000, 0.000, &
           487.249273, 237.400, 0.049, 16.000, 0.0, 0.000, 0.000, &
           715.392902, 98.100, 0.145, 16.000, 0.0, 0.000, 0.000, &
           773.839490, 572.300, 0.141, 16.200, 0.0, 0.000, 0.000, &
           834.145546, 183.100, 0.145, 14.700, 0.0, 0.000, 0.000 &
           /), (/7, 44/)))

      real :: d, ndf
      real, dimension(0:(size(dry, 1)-1)) :: fi, a1, a2, a3, a4, a5, a6
      real, dimension(0:(size(dry, 1)-1)) :: si, df, delta, ffi

      fi = dry(:, 0)
      a1 = dry(:, 1)
      a2 = dry(:, 2)
      a3 = dry(:, 3)
      a4 = dry(:, 4)
      a5 = dry(:, 5)
      a6 = dry(:, 6)

      si = a1 * 1e-7 * P * th**3 * exp(a2*(1.0 - th))               ! (3)
      df = a3 * 1e-4 * (P * th**(0.8-a4) + 1.1*e*th)                ! (6a)

      ! cf P676-11 Annex 2.1

      if (scut==0) then
         df = sqrt(df*df + 2.25e-6)                                 ! (6b)
      end if

      delta = (a5 + a6 * th) * 1e-4 * (P + e) * th**(0.8)           ! (7)

      ffi = f/fi * ((df - delta * (fi-f))/((fi-f)**2 + df**2) + &
           (df - delta * (fi+f))/((fi+f)**2 + df**2))               ! (5)

      d = 5.6e-4 * (P + e) * th**(0.8)                              ! (9)

      ndf = f * P * th**2 * (6.14e-5/(d * (1 + (f/d)**2)) + &
           1.4e-12 * P * th**(1.5) / (1 + 1.9e-5 * f**(1.5)))       ! (8)

      go = 0.182 * f * (sum(si*ffi) + ndf)                          ! (1-2) or (22)
    end block

    block
      real, dimension(0:34, 0:6) :: vapor = transpose(reshape((/ &
                                ! spectroscopic data for water vapor (Table 2)
                                !  f0        b1    b2     b3   b4     b5     b6
           22.235080, .1079, 2.144, 26.38, .76, 5.087, 1.00, &
           67.803960, .0011, 8.732, 28.58, .69, 4.930, .82, &
           119.995940, .0007, 8.353, 29.48, .70, 4.780, .79, &
           183.310087, 2.273, .668, 29.06, .77, 5.022, .85, &
           321.225630, .0470, 6.179, 24.04, .67, 4.398, .54, &
           325.152888, 1.514, 1.541, 28.23, .64, 4.893, .74, &
           336.227764, .0010, 9.825, 26.93, .69, 4.740, .61, &
           380.197353, 11.67, 1.048, 28.11, .54, 5.063, .89, &
           390.134508, .0045, 7.347, 21.52, .63, 4.810, .55, &
           437.346667, .0632, 5.048, 18.45, .60, 4.230, .48, &
           439.150807, .9098, 3.595, 20.07, .63, 4.483, .52, &
           443.018343, .1920, 5.048, 15.55, .60, 5.083, .50, &
           448.001085, 10.41, 1.405, 25.64, .66, 5.028, .67, &
           470.888999, .3254, 3.597, 21.34, .66, 4.506, .65, &
           474.689092, 1.260, 2.379, 23.20, .65, 4.804, .64, &
           488.490108, .2529, 2.852, 25.86, .69, 5.201, .72, &
           503.568532, .0372, 6.731, 16.12, .61, 3.980, .43, &
           504.482692, .0124, 6.731, 16.12, .61, 4.010, .45, &
           547.676440, .9785, .158, 26.00, .70, 4.500, 1.00, &
           552.020960, .1840, .158, 26.00, .70, 4.500, 1.00, &
           556.935985, 497.0, .159, 30.86, .69, 4.552, 1.00, &
           620.700807, 5.015, 2.391, 24.38, .71, 4.856, .68, &
           645.766085, .0067, 8.633, 18.00, .60, 4.000, .50, &
           658.005280, .2732, 7.816, 32.10, .69, 4.140, 1.00, &
           752.033113, 243.4, .396, 30.86, .68, 4.352, .84, &
           841.051732, .0134, 8.177, 15.90, .33, 5.760, .45, &
           859.965698, .1325, 8.055, 30.60, .68, 4.090, .84, &
           899.303175, .0547, 7.914, 29.85, .68, 4.530, .90, &
           902.611085, .0386, 8.429, 28.65, .70, 5.100, .95, &
           906.205957, .1836, 5.110, 24.08, .70, 4.700, .53, &
           916.171582, 8.400, 1.441, 26.73, .70, 5.150, .78, &
           923.112692, .0079, 10.293, 29.00, .70, 5.000, .80, &
           970.315022, 9.009, 1.919, 25.50, .64, 4.940, .67, &
           987.926764, 134.6, .257, 29.85, .68, 4.550, .90, &
           1780.000000, 17506., .952, 196.3, 2.00, 24.15, 5.00 &
           /), (/7, 35/)))

      real, dimension(0:(size(vapor, 1)-1)) :: fi, b1, b2, b3, b4, b5, b6
      real, dimension(0:(size(vapor, 1)-1)) :: si, df, delta, ffi

      fi = vapor(:, 0)
      b1 = vapor(:, 1)
      b2 = vapor(:, 2)
      b3 = vapor(:, 3)
      b4 = vapor(:, 4)
      b5 = vapor(:, 5)
      b6 = vapor(:, 6)

      si = b1 * 1e-1 * e * th**3.5 *exp(b2 * (1.0 - th))            ! (3)
      df = b3 * 1e-4 * (P * th**(b4) + b5 * e * th**b6)             ! (6a)

      ! cf P676-11 Annex 2.1

      if (scut==0) then
         df = 0.535 * df + sqrt(0.217* df*df + 2.1316e-12 * fi*fi/th) ! (6b)
      end if
      delta = 0                                                     ! (7)

      ffi = f/fi * ((df - delta * (fi-f))/((fi - f)**2 + df**2) + &
           (df - delta * (fi+f))/((fi+f)**2 + df**2))               ! (5)

      if (scut==0) then
         gw = 0.182 * f * (dot_product(si, ffi))                    ! (1-2)
      else
         block
           integer, dimension(9) :: i = (/ 1, 4, 5, 6, 8, 13, 21, 25, 35 /) - 1
           gw = 0.182 * f * (dot_product(si(i), ffi(i)))            ! (23)
         end block
      end if
    end block

  end subroutine p676_gas_specific


  subroutine p676_eq_height(f, e, P, ho, hw) &
       bind(c, name='p676_eq_height')

    real(C_DOUBLE), intent(in) :: f       ! frequency (GHz)
    real(C_DOUBLE), intent(in) :: e       ! vapor part. pressure e(P) (hPa) (4)
    real(C_DOUBLE), intent(in) :: P       ! pressure at height hₛ (hPa)
    real(C_DOUBLE), intent(out) :: ho     ! equivalent height for dry air
    real(C_DOUBLE), intent(out) :: hw     ! equivalent height for vapor

    real :: rp

    rp = (P+e) / 1013.25      ! after (26b)

    ! (25)

    block
      real :: t1, t2, t3
      t1 = 4.64/(1.+0.066*(rp**(-2.3))) * exp(-((f-59.7)/(2.87 + 12.4 * exp(-7.9*rp)))**2)
      t2 = (0.14*exp(2.12*rp)) / ((f-118.75)**2 + 0.031*exp(2.2*rp))
      t3 = 0.0114/(1+0.14*rp**(-2.6)) * f &
           * (-0.0247 + 0.0001*f + 1.61e-6*(f**2))/(1.-0.0169*f + 4.1e-5*(f**2) + 3.2e-7*(f**3))

      ho = (6.1)/(1+0.17*(rp**(-1.1))) * (1. + t1 + t2 + t3)
    end block

    ! (26)

    block
      real :: sw
      if (f>350.) then
         stop 101
      end if

      sw = 1.013 / (1. + exp(-8.6*(rp-0.57)))

      hw = 1.66 * (1. &
           + 1.38*sw/((f-22.235)**2 + 2.56*sw) &
           + 3.37*sw/((f-183.31)**2 + 4.69*sw) &
           + 1.58*sw/((f-325.1)**2 + 2.89*sw))
    end block

  end subroutine p676_eq_height


  ! FIXME have to pick p-dry from hs (P835-6). Otherwise att dry is independent of hs!
  ! one way is, make p optional, and in that case compute it from P835-6.

  real(C_DOUBLE) function p676_gas(eldeg, freq, P, e, temp, Vt, hs) &
       bind(c, name='p676_gas') &
       result(att)

    real(C_DOUBLE), intent(in) :: eldeg         ! elevation angle (°)
    real(C_DOUBLE), intent(in) :: freq          ! frequency (GHz)
    real(C_DOUBLE), intent(in) :: P             ! dry air pressure (hPa)
    real(C_DOUBLE), intent(in) :: e             ! vapor part. pressure e(P) (hPa)
    real(C_DOUBLE), intent(in) :: temp          ! temperature (K)
    real(C_DOUBLE), intent(in), optional :: Vt  ! total vaper vapor content (kg/m^2), cf p836_V()
    real(C_DOUBLE), intent(in), optional :: hs  ! station height above mean sea level (km)

    real :: go, gw, ho, hw, attw

    call p676_gas_specific(1, freq, P, e, temp, go, gw) ! (2a-2b)
    call p676_eq_height(freq, e, P, ho, hw)             ! (25-26)

    if (.not. present(Vt)) then
       attw = hw*gw
    else                                                ! (37)
       block
         real, parameter :: fref = 20.6, pref = 815.
         real :: Tref, eref, go1, gw1, go2, gw2

         if (.not. present(hs)) then
            stop 102
         end if

         Tref = 14.*log(0.22/3.67 * Vt) + 3 + 273.15
         eref = vapor_pressure(Vt/3.67, Tref)

         ! go1 go2 are wasted here. Maybe we should go back to split gas -> dry & wet.

         call p676_gas_specific(1, freq, pref, eref, Tref, go1, gw1)
         call p676_gas_specific(1, fref, pref, eref, Tref, go2, gw2)

         if (freq<1.) then
            stop 103
         else if (freq<20.) then
            attw = 0.0176 * Vt * (gw1 / gw2)

            ! ! debug intermediate values
            ! write(*, *) 'tref', tref - 273.15
            ! write(*, *) 'eref', eref
            ! write(*, *) 'g1', gw1
            ! write(*, *) 'g2', gw2

         else if (freq<350.) then
            block
              real :: a, b, h

              a = -0.113 &
                   + 0.2048*exp(-(((freq-22.43)/3.097)**2)) &
                   + 0.2326*exp(-(((freq-183.5)/4.096)**2)) &
                   + 0.2073*exp(-(((freq-325.)/3.651)**2))
              b = 8.741e4*exp(-0.587*freq) + 312.2*(freq**(-2.38)) + 0.723
              h = min(4., hs)

              ! ! debug intermediate values
              ! write(*, *) 'tref', tref - 273.15
              ! write(*, *) 'eref', eref
              ! write(*, *) 'g1', gw1
              ! write(*, *) 'g2', gw2
              ! write(*, *) 'a', a
              ! write(*, *) 'b', b
              ! write(*, *) 'h', h
              ! write(*, *) 'ahb1', (a*(h**b) + 1.)

              attw = 0.0176 * Vt * (gw1 / gw2) * (a*(h**b) + 1.)
            end block
         else
            stop 104
         end if
       end block
    end if

    if (eldeg>=10. .and. eldeg<=90.) then ! §2.2.1

       ! ! debug intermediate values
       ! write(*, *) 'ho', ho
       ! write(*, *) 'go', go
       ! write(*, *) 'Ao', ho*go
       ! write(*, *) 'Ao/sinθ', ho*go / sin(deg2rad(eldeg))
       ! write(*, *) 'hw', hw
       ! write(*, *) 'gw', gw
       ! write(*, *) 'Aw', hw*gw
       ! write(*, *) 'Aw/sinθ', hw*gw / sin(deg2rad(eldeg))

       att = (ho*go + attw) / sin(deg2rad(eldeg))  ! (29)
    else
       stop 105
    end if

  end function p676_gas


  ! Annual averages of total columnar content of liquid water reduced to a temperature
  ! of 273.15 K, Lred (kg/m²), from the dataset in R-REC-P.840-7-Maps.zip. As described
  ! in ITU-R P.840-7 §3.1, lat:+90..-90 and lon:0..360.

  real(C_DOUBLE) function p840_Lred(lat, lon, ppc) &
       bind(c, name='p840_Lred') &
       result(Lred)

    real(C_DOUBLE), intent(in) :: lat  ! latitude (°)
    real(C_DOUBLE), intent(in) :: lon  ! longitude (°)
    real(C_DOUBLE), intent(in) :: ppc  ! probability (%)

    Lred = lookup_pxy(ppc, max(0., min(180., (90.-lat)))/1.125, modulo(lon, 360.)/1.125, p840p, p840Lred)

  end function p840_Lred


  ! Annual value of the wet term of the surface refractivity Nwet (ppm) exceeded
  ! for ppc (%) of an average year, as described in ITU-R P.453-13 §2.2, from the
  ! dataset P.453_NWET_Maps_Annual.zip in R-REC-P.453-13-201712-I!!ZIP-E.zip.
  ! As described there, lat:-90...90 and lon:-180..+180.

  real(C_DOUBLE) function p453_Nwet(lat, lon, ppc) &
       bind(c, name='p453_Nwet') &
       result(Nwet)

    real(C_DOUBLE), intent(in) :: lat  ! latitude (°)
    real(C_DOUBLE), intent(in) :: lon  ! longitude (°)
    real(C_DOUBLE), intent(in) :: ppc  ! probability (%)

    Nwet = lookup_pxy(ppc, max(0., min(180., lat+90.))/0.75, modulo(lon+180., 360.)/0.75, p840p, p453Nwet)

  end function p453_Nwet


  ! Attenuation by clouds (dB), after ITU-R P.840-7.
  ! Equation numbers from ITU-R P.840-7 except where indicated.

  real(C_DOUBLE) function p840_clouds(freq, eldeg, Lred) &
       bind(c, name='p840_clouds') &
       result(Ac)

    real(C_DOUBLE), intent(in) :: freq      ! freq (GHz)
    real(C_DOUBLE), intent(in) :: eldeg     ! elevation (deg)
    real(C_DOUBLE), intent(in) :: Lred      ! reduced total columnar content of liquid water (kg/m²), cf ITU-R P.840-7 §3

    real, parameter :: temp = 273.15                    ! §3, (12)
    real :: fp, fs, dp, ds, th, Kl, e0, e1, e2, er, ei, eta

    if (eldeg<5. .or. eldeg>90.) then
       stop 108
    end if

    th = 300./temp                                     ! (9)
    fp = 20.20 - 146*(th-1.) + 316*(th-1.)**2          ! (GHz) (10)
    fs = 39.8 * fp                                     ! (11)
    dp = 1+(freq/fp)**2
    ds = 1+(freq/fs)**2

    e0 = 77.66 + 103.3*(th-1)                          ! (6)
    e1 = 0.0671*e0                                     ! (7)
    e2 = 3.52                                          ! (8)
    er = (e0-e1)/dp + (e1-e2)/ds + e2                  ! (5)
    ei = freq*( (e0-e1)/(fp*dp) + (e1-e2)/(fs*ds) )    ! (4)

    eta = (2 + er)/ei                                  ! (3)
    Kl = 0.819*freq / (ei*(1 + eta**2))                ! (dB/km)/(g/m³)   (2)
    Ac = (Lred * Kl) / sin(deg2rad(eldeg))             ! (12)

    ! ! debug intermediate values
    ! write(*, *) 'er', er
    ! write(*, *) 'ei', ei
    ! write(*, *) 'eta', eta
    ! write(*, *) 'Lred', Lred
    ! write(*, *) 'Kl', Kl

  end function p840_clouds


  ! Attenuation due to scintillation, after ITU-R P.618-13 §2.4.1
  ! Equation numbers from ITU-R P.618-13 except where indicated.

  real(C_DOUBLE) function p618_scint(freq, eldeg, Deff, ppc, Nwet) &
       bind(c, name='p618_scint') &
       result(As)

    real(C_DOUBLE), intent(in) :: freq      ! freq (GHz)
    real(C_DOUBLE), intent(in) :: eldeg     ! elevation (deg)
    real(C_DOUBLE), intent(in) :: Deff      ! effective antenna diameter (m) ~ D√η
    real(C_DOUBLE), intent(in) :: ppc       ! probability (%)
    real(C_DOUBLE), intent(in) :: Nwet      ! Median value of wet term of surface refractivity (ppm), cf ITU-R P.453-13 §2.2

    real, parameter :: hL = 1000            ! (m) (41)
    real :: sinel, sigmaref, L, x, g, sigma

    ! FIXME 0 or what for freq>20. ? P.618 says 20., but the validation has 29...

    if (freq<4. .or. freq>40.) then
       stop 109
    end if
    if (eldeg<5.) then
       stop 110
    end if

    sinel = sin(deg2rad(eldeg))
    sigmaref = 3.6e-3 + 1e-4 * Nwet                ! (40) (dB)
    L = 2.*hL / (sinel + sqrt(sinel**2 + 2.35e-4)) ! (41) (m)
    x = 1.22 * (Deff**2) * (freq/L)                ! (43a)
    g = sqrt(3.86 * ((x**2 + 1)**(11./12.)) * sin((11./6.) * atan(1./x)) - 7.08*(x**(5./6.))) ! (43)
    sigma = sigmaref * (freq**(7./12.)) * g / (sinel**1.2) ! (44)
    As = sigma * (-0.061 * (log10(ppc)**3) + 0.072 * (log10(ppc)**2) - 1.71*(log10(ppc)) + 3.0) ! (46)

    ! ! debug intermediate values
    ! write(*, *) 'σref', sigmaref
    ! write(*, *) 'L', L
    ! write(*, *) 'x', x
    ! write(*, *) 'g', g
    ! write(*, *) 'σ', sigma

  end function p618_scint


  ! Topographic altitude from ITU-R P.1511-1 Annex 1 and included in P.836-6 data package (km).
  ! Uses bicubic interpolation as described in ITU-R P.1144 Annex 1.2.

  real(C_DOUBLE) function p1511_topoh(lat, lon) &
       bind(c, name='p1511_topoh') &
       result(h)

    real(C_DOUBLE), intent(in) :: lat, lon

    real :: x, y, dx, dy, d, r
    integer :: ix, iy, i, k
    real, parameter :: a = -0.5

    x = (90.5 - max(-90., min(+90., lat)))/0.5
    y = (modulo(lon, 360.)+0.5)/0.5

    ! x=180 (y=360.). will use dx=1 (dy=1)
    ix = max(1, min(size(p1511topo, 1)-3, int(floor(x))))
    iy = max(1, min(size(p1511topo, 2)-3, int(floor(y))))

    ! dx, dy at interp points is 0 1 2 3
    dx = x-ix+1.
    dy = y-iy+1.

    h = 0
    do k=0, 3
       r = 0.
       do i=0, 3
          d = abs(dy-i)
          if (d<=1) then
             r = r + p1511topo(ix+k, iy+i) * ((a+2)*(d**3) - (a+3)*(d**2) + 1)
          else if (d<=2) then
             r = r + p1511topo(ix+k, iy+i) * (a*(d**3) -5*a*(d**2) + 8*a*d - 4*a)
          end if
       end do

       d = abs(dx-k)
       if (d<=1) then
          h = h + r * ((a+2)*(d**3) - (a+3)*(d**2) + 1)
       else if (d<=2) then
          h = h + r * (a*(d**3) -5*a*(d**2) + 8*a*d - 4*a)
       end if
    end do

  end function p1511_topoh


  ! Height scaling procedure for X = V or X = rho, after ITU-R P.836-6 Annex 2.

  real function p836_height_scale(X, lat, lon, ppc, h) &
       result(xx)

    real, intent(in), dimension(:, :, :) :: X
    real, intent(in) :: lat  ! latitude (°)
    real, intent(in) :: lon  ! longitude (°)
    real, intent(in) :: ppc  ! probability (%)
    real, intent(in) :: h    ! altitude (km)

    real :: dp, dx, dy
    integer :: ip, ix, iy

    real, dimension(2, 2, 2) :: vi
    real, dimension(2, 2, 2) :: vschi
    real, dimension(2, 2, 2) :: hi

    call lookup_pxy_find(p840p, size(X, 2), size(X, 3), &
         ppc, max(0., min(180., (90.-lat)))/1.125, modulo(lon, 360.)/1.125, &
         ip, ix, iy, &
         dp, dx, dy)

    vi = X(ip-1:ip, ix+1:ix+2, iy+1:iy+2)
    vschi = p836vsch(ip-1:ip, ix+1:ix+2, iy+1:iy+2)
    hi = reshape( spread( (/ &
         p1511_topoh(90.-(ix+0)*1.125, 0.+(iy+0)*1.125), &
         p1511_topoh(90.-(ix+1)*1.125, 0.+(iy+0)*1.125), &
         p1511_topoh(90.-(ix+0)*1.125, 0.+(iy+1)*1.125), &
         p1511_topoh(90.-(ix+1)*1.125, 0.+(iy+1)*1.125) /), &
         dim=2, ncopies=2), (/ 2, 2, 2 /), order=(/ 2, 3, 1 /))
    vi = vi * exp(-(h-hi)/vschi)

    xx = &
         + vi(1, 1, 1)*(1-dp)*(1-dx)*(1-dy) &
         + vi(1, 1, 2)*(1-dp)*(1-dx)*dy &
         + vi(1, 2, 1)*(1-dp)*dx*(1-dy) &
         + vi(1, 2, 2)*(1-dp)*dx*dy &
         + vi(2, 1, 1)*dp*(1-dx)*(1-dy) &
         + vi(2, 1, 2)*dp*(1-dx)*dy &
         + vi(2, 2, 1)*dp*dx*(1-dy) &
         + vi(2, 2, 2)*dp*dx*dy

  end function p836_height_scale


  ! Water vapor density, ρ in g/m³ exceeded for ppc of an average year.
  ! After ITU-R P.836-6 Annex 2.

  real(C_DOUBLE) function p836_rho(lat, lon, ppc, h) &
       bind(c, name='p836_rho') &
       result(rho)

    real(C_DOUBLE), intent(in) :: lat  ! latitude (°)
    real(C_DOUBLE), intent(in) :: lon  ! longitude (°)
    real(C_DOUBLE), intent(in) :: ppc  ! probability (%)
    real(C_DOUBLE), intent(in) :: h    ! altitude (km)

    rho = p836_height_scale(p836rho, lat, lon, ppc, h)

  end function p836_rho


  ! Total vaper vapor content, after ITU-R P.836-6 Annex 2.

  real(C_DOUBLE) function p836_V(lat, lon, ppc, h) &
       bind(c, name='p836_V') &
       result(Vt)

    real(C_DOUBLE), intent(in) :: lat  ! latitude (°)
    real(C_DOUBLE), intent(in) :: lon  ! longitude (°)
    real(C_DOUBLE), intent(in) :: ppc  ! probability (%)
    real(C_DOUBLE), intent(in) :: h    ! altitude (km)

    Vt = p836_height_scale(p836V, lat, lon, ppc, h)

  end function p836_V


end module prop
