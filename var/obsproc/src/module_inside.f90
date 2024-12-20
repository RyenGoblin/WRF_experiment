
MODULE module_inside










USE module_type
USE module_func
USE module_mm5
USE map_utils
USE module_namelist

CONTAINS










SUBROUTINE inside_domain ( lat , lon , ins , jew, outside_domain, &
                           xjc_out,  yic_out,  xjd_out,  yid_out)




   USE module_map

   IMPLICIT NONE

   REAL ,    INTENT(IN)  :: lat , lon
   INTEGER , INTENT(IN)  :: ins , jew
   LOGICAL , INTENT(OUT) :: outside_domain
   REAL ,    OPTIONAL    :: xjc_out , yic_out
   REAL ,    OPTIONAL    :: xjd_out , yid_out

   

   REAL                  :: xjd , yid


   IF ( ABS(lat) .GT. 90. ) THEN

      outside_domain = .TRUE.

   ELSE
     if (fg_format == 'MM5') then
       CALL llxy (lat , lon , xjd , yid )
     else if (fg_format == 'WRF') then
       call latlon_to_ij(map_info, lat, lon, xjd, yid)
       xjd = xjd + .5
       yid = yid + .5
     endif
   
      IF ((yid .GE. 1.) .AND. (yid .LE. ins  )  .AND. & 
          (xjd .GE. 1.) .AND. (xjd .LE. jew  )) THEN

           outside_domain = .FALSE.

      ELSE
        
           outside_domain = .TRUE.








      END IF

   END IF

   IF (PRESENT (xjc_out)) THEN
       xjc_out = xjd - 0.5
       IF (PRESENT (yic_out)) THEN
           yic_out = yid - 0.5
           IF (PRESENT (xjd_out)) THEN
               xjd_out = xjd
               IF (PRESENT (yid_out)) THEN
                   yid_out = yid
               ENDIF
           ENDIF
       ENDIF
   ENDIF

        
END SUBROUTINE inside_domain
       



SUBROUTINE inside_window (time_obs, time_window_min, time_window_max, &
                          outside_window, iunit)




   USE module_date

   IMPLICIT NONE

   CHARACTER (LEN = 14) :: time_obs
   CHARACTER (LEN = 19) :: time_window_min
   CHARACTER (LEN = 19) :: time_window_max
   LOGICAL, INTENT(OUT) :: outside_window
   INTEGER, INTENT(IN), OPTIONAL :: iunit
   CHARACTER (LEN = 19) :: time_obs_long
   INTEGER :: itb, ita
   INTEGER :: iiunit
   LOGICAL :: date1_correct, date2_correct

   IF (PRESENT (iunit)) THEN
       iiunit = iunit
   ELSE
       iiunit = 0
   ENDIF

   WRITE (time_obs_long, FMT='(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)')    &
          time_obs ( 1: 4), time_obs ( 5: 6), time_obs ( 7: 8), &
          time_obs ( 9:10), time_obs (11:12), time_obs (13:14)



   CALL GETH_IDTS (time_obs_long, time_window_min, itb, date1_correct, iiunit)



   CALL GETH_IDTS (time_obs_long, time_window_max, ita, date2_correct, iiunit)

   IF (((itb .LT. 0) .OR. (ita .GT. 0)) .OR. &
       ((.NOT. date1_correct) .OR. (.NOT. date2_correct))) THEN
       outside_window = .TRUE.              
   ELSE
       outside_window = .FALSE.             
   ENDIF

   RETURN

END SUBROUTINE inside_window

END MODULE module_inside
