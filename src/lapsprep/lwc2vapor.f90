  SUBROUTINE lwc2vapor(lwc,sh,t,p,thresh,lwc_m,sh_m,rh_m)

  ! Subroutine to convert cloud water to vapor.

    IMPLICIT NONE

    ! Inputs:

    REAL, INTENT(IN)    :: lwc    ! Cloud water mixing ratio (kg/kg)
    REAL, INTENT(IN)    :: sh     ! Specific humidity (kg/kg)   
    REAL, INTENT(IN)    :: t      ! Temperature (K)
    REAL, INTENT(IN)    :: p      ! Pressure (Pa)
    REAL, INTENT(IN)    :: thresh ! Saturation factor    
              ! Set thresh to 1.0 to convert cloud water up to
              ! vapor saturation.  1.1 will allow 110% RH, and so
              ! forth

    ! Outputs:
   
    REAL, INTENT(OUT)   :: lwc_m  ! Adjusted lwc
    REAL, INTENT(OUT)   :: sh_m   ! Adjusted specific humidity
    REAL, INTENT(OUT)   :: rh_m   ! Adjusted RH (%)

    ! Locals

    REAL :: shsat,mrmax,mr,mr_m,mrsat
    REAL, EXTERNAL :: ssh,make_rh

    
    ! Set saturation specific humidity for this point         

    shsat = ssh(p,t-273.15)*0.001
 
    ! Convert specific humidity to mixing ratio 
    mrsat = shsat/(1.-shsat)
    mr = sh/(1.-sh)
     
    mrmax = mrsat*thresh

    ! Create modified mixing ratio (mr_m) by adding cloud liquid

    mr_m = mr + lwc

    ! Zero out the modified cloud water

    lwc_m = 0.

    ! If mr_m exceeds mrmax, convert the excess amount 
    ! back to cloud water

    IF (mr_m .GT. mrmax) THEN
      lwc_m = mr_m - mrmax
      mr_m = mrmax
    ENDIF

    ! Compute RH from modified mixing ratio
    rh_m = (mr_m/mrsat)*100.

    ! Convert modified mixing ratio back to specific humidity
    sh_m = mr_m/(1.+mr_m)

    RETURN
  END SUBROUTINE lwc2vapor
