      subroutine readsoclup

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine reads the `soclup.dat` file containing
!!    file references to socioeconomic HRU fraction files (e.g., LupInput25.dat which is 25% water reduction).
      
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name       |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                                 |urban.dat
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      use parm
      character*20 socfnam
      integer :: eof
      
      
      write(soclup_log_unit,*) 'Reading soclup.dat entries:'
!!    read soclup.dat (file unit 123 already assigned)
      do
         read (123,100,iostat=eof) soc_id, socfnam
         if (eof < 0) exit
         if (soc_id == 0) exit
         
         fname_soc(soc_id) = socfnam
         write(soclup_log_unit,*) 'Scenario ID ', soc_id, ' -> ', trim(socfnam)
          
      end do

100   format (i5,1x,a)

      return
      end
