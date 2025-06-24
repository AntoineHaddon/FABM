!-----------------------------------------------------
!
! University of Victoria ice ecosystem model
!
! Contributors: Antoine Haddon, Nadja Steiner, Adam Mohanan
!
! Model description: representing ice algae growning in sea ice, with nutrient (NO3,NH4) limitation
!
!------------------------------------------------------

#include "fabm_driver.h"

module uvic_iceeco

   use fabm_types
   use fabm_expressions  
   use fabm_standard_variables

   implicit none

   private

   type, extends(type_base_model), public :: type_uvic_iceeco
        ! Declare horizontal prognostic variables      
        type (type_surface_state_variable_id) :: id_icedia_c, id_icedia_chl, id_icedia_n
        type (type_surface_state_variable_id) :: id_melosira_c, id_melosira_chl, id_melosira_n
        type (type_surface_state_variable_id) :: id_inh4, id_ino3
        type (type_surface_state_variable_id) :: id_avbotmelt

        ! Declare environmental variables
        type (type_horizontal_dependency_id) :: id_temp, id_meantemp, id_dmeantemp, id_par, id_ice_hi,id_botmelt,id_botgrowth,id_topmelt,id_termelt,id_Amelt
        type (type_dependency_id) :: id_ph2, id_no3SW, id_nh4SW, id_u, id_v

        ! Declare horizontal diagnostic variables
        type (type_horizontal_diagnostic_variable_id) :: id_meant, id_dmeant, id_bipar, id_uipar, id_limpar_icedia, id_limnc_icedia, id_phot_icedia, id_vn_icedia, id_synchl_icedia, id_igup_icedia_n, id_flush_icedia_n, id_flush_icedia_c, id_mortlin_icedia_n, id_mortquad_icedia_n, id_meltoff_icedia_n, id_meltoff_icedia_c, id_sloughing_icedia_c, id_sloughing_icedia_n, id_moldiff_no3, id_moldiff_nh4, id_no3uptake_melosira, id_nh4uptake_melosira, id_mort_melosira
        

        type (type_global_dependency_id) :: id_dt 

        ! Declare model parameters
        real(rk) :: zia, ear, tempref, drag, md_n, nu, knit, fmin, r_pond
        real(rk) :: pcref_icedia, alpha_icedia, qnmin_icedia, qnmax_icedia, vnref_icedia,chltonmax_icedia,zeta_icedia,mlin_icedia,t_sens,mquad_icedia,min_icedia, kno3_icedia, knh4_icedia, crit_melt, dmo, tmo, dtmo,flshia,slghia, ac_ia
        real(rk) :: pcref_melosira, alpha_melosira, chltonmax_mel
        real(rk) :: spd = 86400.0_rk ! Seconds Per Day (spd)
        integer :: melosira

    contains

        procedure :: initialize
        procedure :: do_surface
      
   end type

contains


    subroutine initialize(self,configunit)
        class (type_uvic_iceeco), intent(inout), target :: self
        integer, intent(in)                          :: configunit

        ! register state variables
        call self%register_state_variable(self%id_icedia_c,   'icedia_c',   'mg C m-3',   'Ice diatoms C biomass',   minimum=0.0_rk) 
        call self%register_state_variable(self%id_icedia_chl, 'icedia_chl', 'mg Chl m-3', 'Ice diatoms Chl biomass', minimum=0.0_rk) 
        call self%register_state_variable(self%id_icedia_n,   'icedia_n',   'mg N m-3',   'Ice diatoms N biomass',   minimum=0.0_rk) 
        call self%register_state_variable(self%id_melosira_c,   'melosira_c',   'mg C m-3',   'Melosira Arctica C biomass',   minimum=0.0_rk) 
        call self%register_state_variable(self%id_melosira_chl, 'melosira_chl', 'mg Chl m-3', 'Melosira Arctica Chl biomass', minimum=0.0_rk) 
        call self%register_state_variable(self%id_melosira_n,   'melosira_n',   'mg N m-3',   'Melosira Arctica N biomass',   minimum=0.0_rk) 
        call self%register_state_variable(self%id_inh4,       'inh4',       'mmol N m-3', 'Bottom ice NH4')!,          minimum=0.0_rk) 
        call self%register_state_variable(self%id_ino3,       'ino3',       'mmol N m-3', 'Bottom ice NO3')!,          minimum=0.0_rk) 
        call self%register_state_variable(self%id_avbotmelt,       'avbotmelt',       'm s-1', 'Average bottom melt') 
        
        
        ! register parameters: read in vals from fabm.yaml 
        call self%get_parameter(self%zia, 'zia', 'm','Ice skeletal layer thickness',default=0.03_rk)
        call self%get_parameter(self%ear, 'ear', '-','Ratio of Ea (activation energy) to R (gas constant)',default=4498.0_rk)
        call self%get_parameter(self%tempref, 'tempref', 'K','Reference temperature',default=298.15_rk)
        call self%get_parameter(self%drag, 'drag', '-','drag coefficient at the ice-water interface',default=0.0054_rk)
        call self%get_parameter(self%md_n, 'md_n', 'm2 s-1','molecular diffusion coefficient',default=0.47e-9_rk)
        call self%get_parameter(self%nu, 'nu', 'm2 s-1','kinematic viscosity',default=1.86e-6_rk)
        call self%get_parameter(self%knit, 'knit', 'd-1','nitrification rate',default=0.05_rk,scale_factor=1.0_rk/self%spd)
        call self%get_parameter(self%fmin, 'fmin', '-','NH4 remineralization fraction',default=0.3_rk)
        call self%get_parameter(self%r_pond, 'r_pond', '','melt pond drainage rate',default=0.0175_rk,scale_factor=1.0_rk/self%spd)
        
        ! ice diatoms params
        call self%get_parameter(self%pcref_icedia, 'pcref_icedia', 'gC gC-1 d-1','Reference rate of photosynthesis for ice diatoms',default=3.0_rk,scale_factor=1.0_rk/self%spd)
        call self%get_parameter(self%alpha_icedia, 'alpha_icedia', 'gC gChl-1 (W m-2)-1 d-1','Initial slope of P-I curve for ice diatoms',default=27.04_rk,scale_factor=1.0_rk/self%spd)
        call self%get_parameter(self%qnmin_icedia, 'qnmin_icedia', 'gN gC-1','Min N/C ice diatoms',default=0.04_rk)
        call self%get_parameter(self%qnmax_icedia, 'qnmax_icedia', 'gN gC-1','Max N/C ice diatoms',default=0.172_rk)
        call self%get_parameter(self%vnref_icedia, 'vnref_icedia', 'gN gC-1 d-1','reference N uptake rate ice diatoms',default=0.6_rk,scale_factor=1.0_rk/self%spd)
        call self%get_parameter(self%chltonmax_icedia, 'chltonmax_icedia', 'gChl gN-1','Maximum Chl to N ice diatoms',default=0.18_rk)
        call self%get_parameter(self%zeta_icedia, 'zeta_icedia', 'gC gN-1','Respitory cost of biosynthesis ice diatoms',default=2._rk)
        call self%get_parameter(self%mlin_icedia, 'mlin_icedia', 'd-1','Linear mortality rate ice diatoms',default=0.05_rk,scale_factor=1.0_rk/self%spd)
        call self%get_parameter(self%t_sens, 't_sens', 'C-1','Temperature sensitivity of linear mortality',default=0.0633_rk)
        call self%get_parameter(self%mquad_icedia, 'mquad_icedia', '(mg C m-3)-1 d-1','Quadratic mortality rate ice diatoms',default=0.06_rk,scale_factor=1.0_rk/self%spd)
        call self%get_parameter(self%min_icedia, 'min_icedia', 'mg C m-3','Mortality threshold ice diatoms',default=1.2_rk)
        call self%get_parameter(self%kno3_icedia, 'kno3_icedia', 'mmol m-3','NO3 half saturation constant for ice diatoms',default=1.0_rk)
        call self%get_parameter(self%knh4_icedia, 'knh4_icedia', 'mmol m-3','NH4 half saturation constant for ice diatoms',default=1.0_rk)
        call self%get_parameter(self%crit_melt, 'crit_melt','m d-1', 'critical melt rate [m d-1]', default=0.015_rk)
        call self%get_parameter(self%dmo, 'dmo','(mgC m-3)-1 d-1', 'melt off coefficent', default=0.005_rk,scale_factor=1.0_rk/self%spd)
        call self%get_parameter(self%tmo, 'tmo','C', 'melt off temperature', default=-5.0_rk)
        call self%get_parameter(self%dtmo, 'dtmo','C', 'melt off temperature change threshold', default=.25_rk)
        call self%get_parameter(self%flshia, 'flshia','-', 'fraction of flushed ice algae', default=.3_rk)
        call self%get_parameter(self%slghia, 'slghia','-', 'fraction of sloughed ice algae', default=.3_rk)
        call self%get_parameter(self%ac_ia, 'ac_ia','', 'specific light attenuation coefficient for ice algae (mg Chl m-2)-1', default=0.06_rk) 

        ! melosira params
        call self%get_parameter(self%melosira, 'melosira', '-','switch to activate melosira (0: without, 1 with)',default=0)
        call self%get_parameter(self%pcref_melosira, 'pcref_melosira', 'gC gC-1 d-1','Reference rate of photosynthesis for melosira',default=6.19_rk,scale_factor=1.0_rk/self%spd)
        call self%get_parameter(self%alpha_melosira, 'alpha_melosira', 'gC gChl-1 (W m-2)-1 d-1','Initial slope of P-I curve for melosira',default=1.88_rk,scale_factor=1.0_rk/self%spd)
        call self%get_parameter(self%chltonmax_mel, 'chltonmax_mel', 'gChl gN-1','Maximum Chl to N melosira',default=0.18_rk)


        ! Register environmental variables
        call self%register_horizontal_dependency(self%id_temp,standard_variables%sea_ice_temperature) ! bottom ice temp
        call self%register_horizontal_dependency(self%id_meantemp,standard_variables%sea_ice_mean_temp) 
        call self%register_horizontal_dependency(self%id_dmeantemp,standard_variables%sea_ice_temp_change) 
        call self%register_horizontal_dependency(self%id_par,standard_variables%lowest_ice_layer_PAR)      
        call self%register_horizontal_dependency(self%id_ice_hi,standard_variables%sea_ice_thickness)
        call self%register_horizontal_dependency(self%id_topmelt,standard_variables%topmelt) ! surface (snow+ice) melt rate
        call self%register_horizontal_dependency(self%id_termelt,standard_variables%termelt) ! interior melt rate
        call self%register_horizontal_dependency(self%id_botmelt,standard_variables%tendency_of_sea_ice_thickness_due_to_thermodynamics_melt)
        call self%register_horizontal_dependency(self%id_Amelt,standard_variables%f_melt) ! meltpond fraction
        call self%register_horizontal_dependency(self%id_botgrowth,standard_variables%tendency_of_sea_ice_thickness_due_to_thermodynamics_grow)

        ! Register diagnostic variables
        call self%register_horizontal_diagnostic_variable(self%id_meant,'meantemp','degC','Mean sea ice temperature',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_dmeant,'dmeantemp','degC s-1','Mean sea ice temperature change',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_bipar,'bipar','W m-2','Bottom ice PAR',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_uipar,'uipar','W m-2','Under ice PAR',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_limpar_icedia,'limpar_icedia','-','PAR Limitation factor ice diatoms',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_limnc_icedia,'limnc_icedia','-','N/C Limitation factor ice diatoms',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_phot_icedia,'phot_icedia','mg C m-3 d-1','Photosynthesis rate ice diatoms (=GPP)',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_vn_icedia,'vn_icedia','gN d-1','N uptake rate ice diatoms',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_synchl_icedia,'synchl_icedia','mg Chl m-3 d-1','Chl synthesis rate ice diatoms',source=source_do_horizontal)
        
        call self%register_horizontal_diagnostic_variable(self%id_igup_icedia_n,'igup_icedia_n','mmolN m-3 d-1','Uptake of phytoplankton 2 from ocean surface with ice growth ',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_flush_icedia_n,'flush_icedia_n','mmolN m-3 d-1','Flushing of ice algal N biomass',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_flush_icedia_c,'flush_icedia_c','mgC m-3 d-1','Flushing of ice algal C biomass',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_mortlin_icedia_n,'mortlin_icedia_n','mmolN m-3 d-1','Loss of ice algal N biomass from linear mortality',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_mortquad_icedia_n,'mortquad_icedia_n','mmolN m-3 d-1','Loss of ice algal N biomass from quadratic mortality',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_meltoff_icedia_n,'meltoff_icedia_n','mmolN m-3 d-1','Loss of ice algal N biomass from melt-off',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_meltoff_icedia_c,'meltoff_icedia_c','mgC m-3 d-1','Loss of ice algal C biomass from melt-off',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_sloughing_icedia_n,'sloughing_icedia_n','mmolN m-3 d-1','Loss of ice algal N biomass from sloughing',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_sloughing_icedia_c,'sloughing_icedia_c','mgC m-3 d-1','Loss of ice algal C biomass from sloughing',source=source_do_horizontal)
        
        call self%register_horizontal_diagnostic_variable(self%id_moldiff_no3,'moldiff_no3','mmolN m-3 d-1','Molecular diffusion of NO3 at sea ice ocean interface (positive is flow to sea ice)',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_moldiff_nh4,'moldiff_nh4','mmolN m-3 d-1','Molecular diffusion of nh4 at sea ice ocean interface (positive is flow to sea ice)',source=source_do_horizontal)
        
        call self%register_horizontal_diagnostic_variable(self%id_nh4uptake_melosira,'nh4uptake_melosira','mmolN m-3 d-1','NH4 uptake by melosira from ocean surface',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_no3uptake_melosira,'no3uptake_melosira','mmolN m-3 d-1','NO3 uptake by melosira from ocean surface',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_mort_melosira,'mort_melosira','mmolN m-3 d-1','Melosira N biomass loss from mortlaity',source=source_do_horizontal)

        ! Register couplings: ocean surface variables
        call self%register_dependency(self%id_ph2,'uvic_eco_ph2','','')
        call self%register_dependency(self%id_no3SW,'uvic_eco_no3','','') 
        call self%register_dependency(self%id_nh4SW,'uvic_eco_nh4','','')
        call self%register_dependency(self%id_u, standard_variables%zonal_current)
        call self%register_dependency(self%id_v, standard_variables%meridional_current) 
        call self%request_coupling(self%id_ph2,'uvic_eco_ph2')
        call self%request_coupling(self%id_no3SW,'uvic_eco_no3')
        call self%request_coupling(self%id_nh4SW,'uvic_eco_nh4')

        call self%register_global_dependency(self%id_dt,standard_variables%timestep) 

    end subroutine initialize





    subroutine do_surface(self,_ARGUMENTS_DO_SURFACE_)
        class (type_uvic_iceeco),intent(in) :: self

        _DECLARE_ARGUMENTS_DO_SURFACE_
        real(rk) :: icedia_c, icedia_chl, icedia_n, qn_icedia, qchl_icedia
        real(rk) :: melosira_c, melosira_chl, melosira_n, qn_melosira, qchl_melosira
        real(rk) :: ino3, inh4
        real(rk) :: no3SW, nh4SW,u,v, ph2
        real(rk) :: limtemp
        real(rk) :: pcmax_icedia,limnc_icedia, limice_icedia, limpar_icedia, phot_icedia, vn_icedia,limnh4_icedia,limno3_icedia,no3uptake_icedia,nh4uptake_icedia,rhochl_icedia,synchl_icedia,resp_icedia,mort_icedia,mortlin_icedia,mortquad_icedia, flush_icedia_c, igup_icedia_n, meltoff_icedia_c, sloughing_icedia_c
        real(rk) :: pcmax_melosira,limnc_melosira, limpar_melosira, phot_melosira, vn_melosira, limnh4_melosira,limno3_melosira, no3uptake_melosira, nh4uptake_melosira, rhochl_melosira, synchl_melosira, resp_melosira, mort_melosira
        real(rk) :: fric_vel, moldiff_no3, moldiff_nh4, nitrif, remin
        real(rk) :: temp,meantemp,dmeantemp,bipar,uipar,dt, ice_hi,topmelt,termelt,botmelt,botgrowth, Amelt, avbotmelt

        _HORIZONTAL_LOOP_BEGIN_
        _GET_HORIZONTAL_(self%id_icedia_c,icedia_c)
        _GET_HORIZONTAL_(self%id_icedia_chl,icedia_chl)
        _GET_HORIZONTAL_(self%id_icedia_n,icedia_n)
        _GET_HORIZONTAL_(self%id_melosira_c,melosira_c)
        _GET_HORIZONTAL_(self%id_melosira_chl,melosira_chl)
        _GET_HORIZONTAL_(self%id_melosira_n,melosira_n)
        _GET_HORIZONTAL_(self%id_ino3,ino3)
        _GET_HORIZONTAL_(self%id_inh4,inh4)
        
        _GET_(self%id_ph2,ph2)
        _GET_(self%id_no3SW,no3SW)
        _GET_(self%id_nh4SW,nh4SW)
        _GET_(self%id_u,u)
        _GET_(self%id_v,v)
        _GET_HORIZONTAL_(self%id_temp,temp)
        _GET_HORIZONTAL_(self%id_meantemp,meantemp)
        _GET_HORIZONTAL_(self%id_dmeantemp,dmeantemp)
        _GET_HORIZONTAL_(self%id_par,bipar)
        _GET_HORIZONTAL_(self%id_ice_hi,ice_hi)
        _GET_HORIZONTAL_(self%id_topmelt,topmelt)
        _GET_HORIZONTAL_(self%id_termelt,termelt)
        _GET_HORIZONTAL_(self%id_Amelt,Amelt)
        _GET_HORIZONTAL_(self%id_botmelt,botmelt)
        _GET_HORIZONTAL_(self%id_botgrowth,botgrowth)
        _GET_GLOBAL_(self%id_dt,dt)


        ! BOTTOM ICE DIATOMS

        ! Elemental ratios
        qn_icedia = icedia_n / icedia_c
        qchl_icedia = icedia_chl / icedia_c
        
        ! N limitation
        limnc_icedia = (qn_icedia - self%qnmin_icedia) / (self%qnmax_icedia - self%qnmin_icedia)
        
        ! Temp limitation
        limtemp = exp(-self%ear * (1.0_rk/(temp) - 1.0_rk/self%tempref) )
        
        ! Photosynthesis ice diatoms
        pcmax_icedia= self%pcref_icedia * limtemp * limnc_icedia 
        limpar_icedia = 1.0_rk - exp( - self%alpha_icedia/max(1e-15,pcmax_icedia) * qchl_icedia * bipar)
        phot_icedia = pcmax_icedia * limpar_icedia * icedia_c
        
        ! N uptake ice diatoms
        limno3_icedia= max(0.0_rk, ino3 / (self%kno3_icedia + ino3) )
        limnh4_icedia= max(0.0_rk, inh4 / (self%knh4_icedia + inh4) )
        limnc_icedia = (self%qnmax_icedia - qn_icedia) / (self%qnmax_icedia - self%qnmin_icedia)
        vn_icedia = self%vnref_icedia * limtemp * max(0.0_rk,limnc_icedia**0.05) * (limnh4_icedia + (1.0_rk - limnh4_icedia)*limno3_icedia) * icedia_c
        no3uptake_icedia = self%vnref_icedia * limtemp * max(0.0_rk,limnc_icedia**0.05) * (1.0_rk - limnh4_icedia)*limno3_icedia * icedia_c /14.0_rk ! last term is conversion from gN to mmol N
        nh4uptake_icedia = self%vnref_icedia * limtemp * max(0.0_rk,limnc_icedia**0.05) * limnh4_icedia  * icedia_c /14.0_rk
        
        ! Chl synthesis ice diatoms
        rhochl_icedia = pcmax_icedia * limpar_icedia / max(1e-15,(self%alpha_icedia * qchl_icedia * bipar))
        synchl_icedia = rhochl_icedia * self%chltonmax_icedia * vn_icedia
        
        ! Respiration
        resp_icedia = self%zeta_icedia * vn_icedia

        ! Mortality and remineralization
        if (icedia_c.lt.self%min_icedia) then ! low biomass
            mortlin_icedia = 0.0_rk
            mortquad_icedia = 0.0_rk
            remin= 0.0_rk
        else
            mortlin_icedia = self%mlin_icedia * exp(self%t_sens * (temp-273.15_rk)) * icedia_c 
            mortquad_icedia = self%mquad_icedia * icedia_c * icedia_c
            remin = self%fmin * self%mlin_icedia * exp(self%t_sens * (temp-273.15_rk)) * icedia_n /14.0_rk ! last term is conversion from gN to mmol N
        endif
        mort_icedia = mortlin_icedia + mortquad_icedia

        
        ! Melt-off
        meltoff_icedia_c=0.0_rk
        ! if (temp-273.15_rk.gt.self%tmo) then
        !     meltoff_icedia_c = self%dmo * min(( temp-273.15_rk-self%tmo )/(-1.8_rk-self%tmo), 1.0_rk) * icedia_c*icedia_c 
        ! endif
        ! if ((meantemp-273.15_rk.gt.self%tmo)) then
        if ((meantemp-273.15_rk.gt.self%tmo).and.(dmeantemp*self%spd.gt.self%dtmo)) then
            meltoff_icedia_c = self%dmo * max(dmeantemp*self%spd,0.0_rk) * max(0.0_rk, min(( meantemp-273.15_rk-self%tmo )/max(1e-15,(-1.8_rk-self%tmo)), 1.0_rk))**0.2 * icedia_c*icedia_c 
        endif

        ! Sloughing
        sloughing_icedia_c = self%slghia * icedia_c * botmelt/self%zia 

        ! Flushing
        flush_icedia_c = self%flshia * icedia_c * ( topmelt + termelt + Amelt*self%r_pond )/self%zia 
        
        ! Uptake from ocean surface with ice growth (ph2 is is mmol N m-3, need to convert to gN m-3)
        igup_icedia_n = ph2*14.0_rk/self%zia * max(0.0_rk, botgrowth )


        uipar = bipar * exp(-self%ac_ia* icedia_chl*self%zia  ) ! Under ice PAR: bottom ice PAR with shading by bottom ice aglae

        ! MELOSIRA ARCTICA
        if (self%melosira.eq.1) then
            ! Elemental ratios
            qn_melosira = melosira_n / melosira_c
            qchl_melosira = melosira_chl / melosira_c

            ! N limitation
            limnc_melosira = (qn_melosira - self%qnmin_icedia) / (self%qnmax_icedia - self%qnmin_icedia)

            ! Photosynthesis melosira
            pcmax_melosira= self%pcref_melosira * limtemp * limnc_melosira
            limpar_melosira = 1.0_rk - exp( - self%alpha_melosira/pcmax_melosira * qchl_melosira * uipar)
            phot_melosira = pcmax_melosira * limpar_melosira * melosira_c
            
            ! N uptake melosira
            limno3_melosira= max(0.0_rk, no3SW / (self%kno3_icedia + no3SW) )
            limnh4_melosira= max(0.0_rk, nh4SW / (self%knh4_icedia + nh4SW) )
            limnc_melosira = (self%qnmax_icedia - qn_melosira) / (self%qnmax_icedia - self%qnmin_icedia)
            vn_melosira = self%vnref_icedia * limtemp * max(0.0_rk,limnc_melosira**0.05) * (limnh4_melosira + (1.0_rk - limnh4_melosira)*limno3_melosira) * melosira_c
            no3uptake_melosira = self%vnref_icedia * limtemp * max(0.0_rk,limnc_melosira**0.05) * (1.0_rk - limnh4_melosira)*limno3_melosira * melosira_c /14.0_rk ! last term is conversion from gN to mmol N
            nh4uptake_melosira = self%vnref_icedia * limtemp * max(0.0_rk,limnc_melosira**0.05) * limnh4_melosira  * melosira_c /14.0_rk
            
            ! Chl synthesis melosira
            rhochl_melosira = pcmax_melosira * limpar_melosira / max(1e-15,(self%alpha_melosira * qchl_melosira * uipar))
            synchl_melosira = rhochl_melosira * self%chltonmax_mel * vn_melosira

            ! Respiration
            resp_melosira = self%zeta_icedia * vn_melosira

            ! Mortality and remineralization
            if (melosira_c.lt.self%min_icedia) then ! low biomass
                mort_melosira = 0.0_rk
            else
                mort_melosira = self%mlin_icedia * exp(self%t_sens * (temp-273.15_rk)) * melosira_c 
            endif
        
            ! average bottom melt for flushing (weights adjusted to get close to daily average)
            _GET_HORIZONTAL_(self%id_avbotmelt,avbotmelt)
            _ADD_SURFACE_SOURCE_(self%id_avbotmelt, max(-avbotmelt/dt, (botmelt - 1.e-6_rk*avbotmelt ))  )
            avbotmelt = avbotmelt * self%spd * 0.2e-5_rk ! conversion to m/d with extra wieght to get close to daily average
            ! Flushing with threshold on average bottom melt
            if (avbotmelt.gt.self%crit_melt) then 
                _ADD_SURFACE_SOURCE_(self%id_melosira_c, (-melosira_c+1e-10)/dt )
                _ADD_SURFACE_SOURCE_(self%id_melosira_n, (-melosira_n+1e-10)/dt )
                _ADD_SURFACE_SOURCE_(self%id_melosira_chl, (-melosira_chl+1e-10)/dt )
            endif


        endif

        ! NUTRIENTS

        ! N molecular diffusion
        fric_vel = sqrt(self%drag)*sqrt(u**2+v**2)
        moldiff_no3 = self%md_n /self%nu * fric_vel * (no3SW-ino3) /self%zia
        moldiff_nh4 = self%md_n /self%nu * fric_vel * (nh4SW-inh4) /self%zia

        ! Nitrification
        ! nitrif = inh4 * self%knit / (1.0_rk + bipar)
        nitrif = 0.0_rk
        

        ! DYNAMICS

        if (ice_hi.lt.0.1_rk) then ! Ice is absent: oldversion would set everything to follow sea surface values, new maintain everything at 10^-10
            ! _ADD_SURFACE_SOURCE_(self%id_icedia_c, (ph2*14.0_rk-icedia_n)/dt /max(1.e-15,qn_icedia) )
            ! _ADD_SURFACE_SOURCE_(self%id_icedia_n, (ph2*14.0_rk-icedia_n)/dt )
            ! _ADD_SURFACE_SOURCE_(self%id_icedia_chl, (ph2*14.0_rk-icedia_n)/dt *qchl_icedia/max(1.e-15,qn_icedia))
            _ADD_SURFACE_SOURCE_(self%id_icedia_c  , (-icedia_c  +1e-10_rk)/dt )
            _ADD_SURFACE_SOURCE_(self%id_icedia_n  , (-icedia_n  +1e-10_rk)/dt )
            _ADD_SURFACE_SOURCE_(self%id_icedia_chl, (-icedia_chl+1e-10_rk)/dt )
            if (self%melosira.eq.1) then
                ! _ADD_SURFACE_SOURCE_(self%id_melosira_c, (ph2*14.0_rk-melosira_n)/dt /max(1.e-15,qn_melosira) )
                ! _ADD_SURFACE_SOURCE_(self%id_melosira_n, (ph2*14.0_rk-melosira_n)/dt )
                ! _ADD_SURFACE_SOURCE_(self%id_melosira_chl, (ph2*14.0_rk-melosira_n)/dt *qchl_melosira/max(1.e-15,qn_melosira))
                _ADD_SURFACE_SOURCE_(self%id_melosira_c  , (-melosira_c  +1e-10)/dt )
                _ADD_SURFACE_SOURCE_(self%id_melosira_n  , (-melosira_n  +1e-10)/dt )
                _ADD_SURFACE_SOURCE_(self%id_melosira_chl, (-melosira_chl+1e-10)/dt )
            endif
            _ADD_SURFACE_SOURCE_(self%id_ino3, (-ino3+1e-10)/dt )
            _ADD_SURFACE_SOURCE_(self%id_inh4, (-inh4+1e-10)/dt )        
        else

            ! Ice diatoms dynamics
            _ADD_SURFACE_SOURCE_(self%id_icedia_c, phot_icedia - resp_icedia - mort_icedia  + igup_icedia_n / max(1.e-15,qn_icedia) -meltoff_icedia_c -sloughing_icedia_c -flush_icedia_c)
            _ADD_SURFACE_SOURCE_(self%id_icedia_n, vn_icedia - mort_icedia*qn_icedia + igup_icedia_n -meltoff_icedia_c*qn_icedia -sloughing_icedia_c*qn_icedia -flush_icedia_c*qn_icedia)
            _ADD_SURFACE_SOURCE_(self%id_icedia_chl, synchl_icedia - mort_icedia*qchl_icedia + igup_icedia_n*qchl_icedia/max(1.e-15,qn_icedia) -meltoff_icedia_c*qchl_icedia -sloughing_icedia_c*qchl_icedia -flush_icedia_c*qchl_icedia  )

            ! Melosira dynamics
            if (self%melosira.eq.1) then
                _ADD_SURFACE_SOURCE_(self%id_melosira_c, phot_melosira - resp_melosira - mort_melosira )
                _ADD_SURFACE_SOURCE_(self%id_melosira_n, vn_melosira - mort_melosira*qn_melosira )
                _ADD_SURFACE_SOURCE_(self%id_melosira_chl, synchl_melosira - mort_melosira*qchl_melosira )
            endif


            ! Bottom ice nitrogen dynamics
            _ADD_SURFACE_SOURCE_(self%id_ino3, -no3uptake_icedia + moldiff_no3 + nitrif)
            _ADD_SURFACE_SOURCE_(self%id_inh4, -nh4uptake_icedia + moldiff_nh4 - nitrif + remin)

        endif

        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_uipar,uipar) 
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_bipar,bipar) 
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_meant,meantemp) 
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_dmeant,dmeantemp) 

        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_limpar_icedia,limpar_icedia) 
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_limnc_icedia,(qn_icedia - self%qnmin_icedia) / (self%qnmax_icedia - self%qnmin_icedia)) 
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_phot_icedia,phot_icedia *self%spd) 
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_synchl_icedia, synchl_icedia *self%spd) 
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_vn_icedia,vn_icedia *self%spd) 
        
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_igup_icedia_n, igup_icedia_n/14.0_rk *self%spd)  ! mgN m-3 s-1 -> mmolN m-3 d-1
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_flush_icedia_n, flush_icedia_c*qn_icedia/14.0_rk *self%spd)  ! mgC m-3 s-1 -> mmolN m-3 d-1
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_flush_icedia_c, flush_icedia_c *self%spd)  ! mgC m-3 s-1 -> mgC m-3 d-1
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_meltoff_icedia_n, meltoff_icedia_c*qn_icedia/14.0_rk *self%spd)  ! mgC m-3 s-1 -> mmolN m-3 d-1
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_meltoff_icedia_c, meltoff_icedia_c *self%spd)  ! mgC m-3 s-1 -> mgC m-3 d-1
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_sloughing_icedia_n, sloughing_icedia_c*qn_icedia/14.0_rk *self%spd)  ! mgC m-3 s-1 -> mmolN m-3 d-1
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_sloughing_icedia_c, sloughing_icedia_c *self%spd)  ! mgC m-3 s-1 -> mgC m-3 d-1
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_mortlin_icedia_n, mortlin_icedia*qn_icedia/14.0_rk *self%spd)  ! mgC m-3 s-1 -> mmolN m-3 d-1
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_mortquad_icedia_n, mortquad_icedia*qn_icedia/14.0_rk *self%spd)  ! mgC m-3 s-1 -> mmolN m-3 d-1
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_moldiff_no3, moldiff_no3 *self%spd)  ! mmolN m-3 s-1 -> mmolN m-3 d-1
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_moldiff_nh4, moldiff_nh4 *self%spd)  ! mmolN m-3 s-1 -> mmolN m-3 d-1
        if (self%melosira.eq.1) then
            _SET_HORIZONTAL_DIAGNOSTIC_(self%id_no3uptake_melosira, no3uptake_melosira *self%spd)  ! mmolN m-3 s-1 -> mmolN m-3 d-1
            _SET_HORIZONTAL_DIAGNOSTIC_(self%id_nh4uptake_melosira, nh4uptake_melosira *self%spd)  ! mmolN m-3 s-1 -> mmolN m-3 d-1
            _SET_HORIZONTAL_DIAGNOSTIC_(self%id_mort_melosira, mort_melosira*qn_icedia/14.0_rk *self%spd)  ! mgC m-3 s-1 -> mmolN m-3 d-1
        endif


        _HORIZONTAL_LOOP_END_
    end subroutine do_surface


end module uvic_iceeco