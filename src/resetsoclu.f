      subroutine resetsoclu

!! ~ PURPOSE ~
!! Dynamically reset HRU fractions based on the selected socio-economic scenario.

      use parm
      character(len=80) :: titldum
      character(len=80) :: lupfile
      integer :: hru, j, mon
      !logical, save :: baseline_saved = .false.
      
      !!!!!!!!!!!!!!!!!!!!
      write(soclup_log_unit,*) 'Before reset:'
      do j = 1, mhru
          write(soclup_log_unit,*) 'HRU ', j, ' hru_fr = ', hru_fr(j)
      end do
      flush(soclup_log_unit)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !if (.not. baseline_saved) then
        !do j = 1, mhru
            !hru_fr_base(j) = hru_fr(j)
        !end do
        !baseline_saved = .true.
        !write(soclup_log_unit,*) '>> Baseline hru_fr stored.'
      !end if
      
!! Select scolUP file based on current_lup_id (set earlier based on March volume)
      lupfile = fname_soc(current_lup_id)
      
      write(soclup_log_unit,*) '-----------------------------'
      write(soclup_log_unit,*) 'Applying LUP file: ', trim(lupfile)

      open (9124, file=lupfile, status='old')
      read (9124, '(A80)') titldum   ! skip header line

      do j = 1, mhru
         read (9124, *, end=99) hru, hru_fr(j)
      end do

!! Reset all HRU-dependent values
      do j = 1, mhru
         if (hru_fr(j) <= 0.0) hru_fr(j) = .0000001   ! avoid zero HRUs

         hru_km(j)    = sub_km(hru_sub(j)) * hru_fr(j)
         hru_ha(j)    = hru_km(j) * 100.0
         hru_dafr(j)  = hru_km(j) / da_km

         do mon = 1, 12
            wupnd(mon,j)   = wupnd(mon,j) * hru_fr(j)
            wushal(mon,j)  = wushal(mon,j) * hru_fr(j)
            wudeep(mon,j)  = wudeep(mon,j) * hru_fr(j)
         end do

         pnd_psa(j)   = pnd_psa(j)   * hru_fr(j)
         pnd_esa(j)   = pnd_esa(j)   * hru_fr(j)
         pnd_pvol(j)  = pnd_pvol(j)  * hru_fr(j)
         pnd_evol(j)  = pnd_evol(j)  * hru_fr(j)
         pnd_vol(j)   = pnd_vol(j)   * hru_fr(j)

         wet_nsa(j)   = wet_nsa(j)   * hru_fr(j)
         wet_mxsa(j)  = wet_mxsa(j)  * hru_fr(j)
         wet_nvol(j)  = wet_nvol(j)  * hru_fr(j)
         wet_mxvol(j) = wet_mxvol(j) * hru_fr(j)
         wet_vol(j)   = wet_vol(j)   * hru_fr(j)
         hru_ha(j)    = hru_km(j) * 100.
!	   pot_vol(j) = 10. * pot_volmm(j) * hru_ha(j)   !! mm => m^3     NUBZ     
         pot_volx(j)  = pot_volxmm(j)
         pot_tile(j)  = pot_tilemm(j)
      end do
      
      write(soclup_log_unit,*) 'After reset:'
      do j = 1, mhru
         write(soclup_log_unit,*) 'HRU ', j, ' new hru_fr = ', hru_fr(j)
      end do
      flush(soclup_log_unit)
      

99    close(9124)
      return
      end
