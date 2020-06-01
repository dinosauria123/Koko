! Copyright (C) 2020 The Koko Project Developers
!
! See the file COPYRIGHT.md in the top-level directory of this
! distribution
!
! This file is part of Koko.
!
! Koko is free software: you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! Koko is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with Koko; see the file COPYING.  If not, see
! <https://www.gnu.org/licenses/>.

MODULE refind

  ! Formulae for refractive indices of some optical materials

  IMPLICIT NONE
  INTEGER, PARAMETER  :: dp = kind(1.0d0)

CONTAINS

  FUNCTION riair( lambda_um, t, p, h )
  
    ! Returns the refractive index of air as a function of
    ! wavelength, temperature, pressure, and humidity.
    !
    ! INPUT
    ! lambda_um    :  Wavelength in micrometer
    ! t (optional) :  Air temperature in deg Celsius (default 20 deg C)
    ! p (optional) :  Air pressure in Pascal (default 101325 Pa)
    ! h (optional) :  Relative humidity of air (default 0%)
    !
    ! OUTPUT
    ! riair :         Refractive index of air
    !
    ! Reference:
    ! K.P. Birch and M.J. Downs, "An updated Edlen equation for the
    ! refractive index of air," Metrologia 30, 155-162 (1993). 
    ! K.P. Birch and M.J. Downs, "Correction to the updated Edlen
    ! equation for the refractive index of air," Metrologia 31, 315-316
    ! (1994). 
    ! NIST modifications to account for the increased carbon dioxide content
    ! Engineering Metrology Toolbox, https://emtoolbox.nist.gov

    REAL(KIND=dp)          :: lambda_um
    REAL(KIND=dp),OPTIONAL :: t, p, h
    
    REAL(KIND=dp)          :: riair
    REAL(KIND=dp)          :: A,B,C,D,E,F,G,X,s2,ns,ntp,pv

    IF (.NOT. PRESENT(t)) THEN
       t = 20.0_dp
    END IF
    IF (.NOT. PRESENT(p)) THEN
       p = 101325.0_dp
    END IF
    IF (.NOT. PRESENT(h)) THEN
       h = 0.0_dp
    END IF

    ! set up constants
    A =    8342.54_dp
    B = 2406147.0_dp
    C =   15998.0_dp
    D =   96095.43_dp
    E =       0.601_dp
    F =       0.00972_dp
    G =       0.003661_dp
    
    ! convert wavelength to wavenumber in 1/um
    s2 = (1.0_dp / lambda_um)**2

    ! wavelength dependent part of refractive index
    ns = 1.0_dp + 1.0e-8_dp * (A + B/(130.0_dp - s2) + C/(38.9_dp - s2))

    ! calculate corrections for p, t
    X = (1.0_dp + 1.0e-8_dp * p * (E - F * t)) / (1.0_dp + G * t)
    ntp = 1.0_dp + p * (ns - 1.0_dp) * X / D

    ! correct for water vapor and CO2 using the NIST formula
    pv = 0.01_dp * h * water_svp(t)

    ! put it all together
    riair = ntp - ( 292.75e-10_dp/(t + 273.15_dp) )*(3.7345_dp - 0.0401_dp * s2)*pv

  END FUNCTION riair


  !-----------------------------------------------------------------------

  FUNCTION water_svp( tc )

    ! Calculates the saturation vapor pressure of water at a
    ! specified temperature t when the relative humidity
    ! is known. The IAWPS (International Association for the
    ! Properties of Water and Steam) formula is used to
    ! calculate the water pressure
    !
    ! INPUT
    ! tc :   Temperature in deg Celsius
    !
    ! OUTPUT
    ! water_svp : Saturation vapor pressure in Pascal
    !
    ! Reference
    ! NIST Engineering Metrology Toolbox ( https://emtoolbox.nist.gov )

    REAL(KIND=dp), INTENT(IN) :: tc

    REAL(KIND=dp) :: water_svp    
    REAL(KIND=dp) :: T, K1,K2,K3,K4,K5,K6,K7,K8,K9,K10, A1,A2,A3
    REAL(KIND=dp) :: A, B, C, X, Y, omega, theta

    ! set up constants
    K1  =   1.16705214528E+03_dp
    K2  =  -7.24213167032E+05_dp
    K3  =  -1.70738469401E+01_dp
    K4  =   1.20208247025E+04_dp
    K5  =  -3.23255503223E+06_dp
    K6  =   1.49151086135E+01_dp
    K7  =  -4.82326573616E+03_dp
    K8  =   4.05113405421E+05_dp
    K9  =  -2.38555575678E-01_dp
    K10 =   6.50175348448E+02_dp
    A1  = -13.928169_dp
    A2  =  34.7078238_dp
    A3  = 661.657_dp

    ! convert t to absolute temperature
    T = tc + 273.15_dp

    IF ( t > 0 ) THEN
       omega = T + K9 / (T - K10)
       A = omega * (omega + K1) + K2
       B = omega * (omega * K3 + K4) + K5
       C = omega * (omega * K6 + K7) + K8
       X = SQRT(B**2 - 4.0_dp * A * C) - B;
       water_svp = 1.0e6_dp * (2.0_dp * C / X)**4;
    ELSE
       theta = T / 273.16
       Y = A1 * (1.0_dp - theta**(-1.5_dp)) + A2 * (1.0_dp - theta**(-1.25_dp))
       water_svp = A3 * EXP(Y)
    END IF

  END FUNCTION water_svp
  
END MODULE refind
