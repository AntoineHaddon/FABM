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
!    use fabm_standard_variables

   implicit none

   private

   type, extends(type_base_model), public :: type_uvic_iceeco
        ! Declare horizontal prognostic variables      
        type (type_surface_state_variable_id) :: id_icedia_c,id_icedia_chl,id_icedia_n

        ! Declare environmental variables
        type (type_horizontal_dependency_id) :: id_temp,id_par
    
        ! Declare horizontal diagnostic variables
        type (type_horizontal_diagnostic_variable_id) :: id_bipar, id_limpar_icedia, id_vn_icedia,id_rhochl_icedia
    
        ! Declare model parameters
        real(rk) :: ear, tempref, pcref_icedia, alpha_icedia, qnmin_icedia, qnmax_icedia, vnref_icedia,chltonmax_icedia,zeta_icedia,mlin_icedia,mquad_icedia,min_icedia
        real(rk) :: spd = 86400.0_rk ! Seconds Per Day (spd)

    contains

        procedure :: initialize
        procedure :: do_surface
      
   end type

contains


    subroutine initialize(self,configunit)
        class (type_uvic_iceeco), intent(inout), target :: self
        integer, intent(in)                          :: configunit

        ! register state variables
        call self%register_state_variable(self%id_icedia_c,'icedia_c','mg C m-3','Ice diatoms C biomass',minimum=0.0_rk) 
        call self%register_state_variable(self%id_icedia_chl,'icedia_chl','mg Chl m-3','Ice diatoms Chl biomass',minimum=0.0_rk) 
        call self%register_state_variable(self%id_icedia_n,'icedia_n','mg N m-3','Ice diatoms N biomass',minimum=0.0_rk) 

        ! register parameters: read in vals from fabm.yaml 
        call self%get_parameter(self%ear, 'ear', '-','Ratio of Ea (activation energy) to R (gas constant)',default=4498.0_rk)
        call self%get_parameter(self%tempref, 'tempref', 'K','Reference temperature',default=298.15_rk)
        call self%get_parameter(self%pcref_icedia, 'pcref_icedia', 'gC gC-1 d-1','Reference rate of photosynthesis for ice diatoms',default=3.0_rk,scale_factor=1.0_rk/self%spd)
        call self%get_parameter(self%alpha_icedia, 'alpha_icedia', 'gC gChl-1 (W m-2)-1 d-1','Initial slope of P-I curve for ice diatoms',default=27.04_rk,scale_factor=1.0_rk/self%spd)
        call self%get_parameter(self%qnmin_icedia, 'qnmin_icedia', 'gN gC-1','Min N/C ice diatoms',default=0.04_rk)
        call self%get_parameter(self%qnmax_icedia, 'qnmax_icedia', 'gN gC-1','Max N/C ice diatoms',default=0.172_rk)
        call self%get_parameter(self%vnref_icedia, 'vnref_icedia', 'gN gC-1 d-1','reference N uptake rate ice diatoms',default=0.6_rk,scale_factor=1.0_rk/self%spd)
        call self%get_parameter(self%chltonmax_icedia, 'chltonmax_icedia', 'gChl gN-1','Maximum Chl to N ice diatoms',default=0.18_rk)
        call self%get_parameter(self%zeta_icedia, 'zeta_icedia', 'gC gN-1','Respitory cost of biosynthesis ice diatoms',default=2._rk)
        call self%get_parameter(self%mlin_icedia, 'mlin_icedia', 'd-1','Linear mortality rate ice diatoms',default=0.05_rk,scale_factor=1.0_rk/self%spd)
        call self%get_parameter(self%mquad_icedia, 'mquad_icedia', '(mg C m-3)-1 d-1','Quadratic mortality rate ice diatoms',default=0.06_rk,scale_factor=1.0_rk/self%spd)
        call self%get_parameter(self%min_icedia, 'min_icedia', 'mg C m-3','Mortality threshold ice diatoms',default=1.2_rk)

        ! Register environmental variables
        call self%register_horizontal_dependency(self%id_temp,standard_variables%sea_ice_temperature) 
        call self%register_horizontal_dependency(self%id_par,standard_variables%lowest_ice_layer_PAR)      

        ! Register diagnostic variables
        call self%register_horizontal_diagnostic_variable(self%id_bipar,'bipar','W m-2','Bottom ice PAR',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_limpar_icedia,'limpar_icedia','-','Limitation factor ice diatoms',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_vn_icedia,'vn_icedia','gN d-1','N uptake rate ice diatoms',source=source_do_horizontal)
        call self%register_horizontal_diagnostic_variable(self%id_rhochl_icedia,'rhochl_icedia','-','Chl synthesis ratio ice diatoms',source=source_do_horizontal)

    end subroutine initialize





    subroutine do_surface(self,_ARGUMENTS_DO_SURFACE_)
        class (type_uvic_iceeco),intent(in) :: self

        _DECLARE_ARGUMENTS_DO_SURFACE_
        real(rk) :: icedia_c, icedia_chl, icedia_n, qn_icedia, qchl_icedia
        real(rk) :: pcmax,limtemp
        real(rk) :: limnc_icedia,limpar_icedia,phot_icedia,vn_icedia,limnh4_icedia,limno3_icedia,rhochl_icedia,synchl_icedia,resp_icedia,mort_icedia
        real(rk) :: temp,par

        _HORIZONTAL_LOOP_BEGIN_
        _GET_HORIZONTAL_(self%id_icedia_c,icedia_c)
        _GET_HORIZONTAL_(self%id_icedia_chl,icedia_chl)
        _GET_HORIZONTAL_(self%id_icedia_n,icedia_n)

        _GET_HORIZONTAL_(self%id_temp,temp)
        _GET_HORIZONTAL_(self%id_par,par)


        ! Elemental ratios
        qn_icedia = icedia_n / icedia_c
        qchl_icedia = icedia_chl / icedia_c

        ! N limitation
        limnc_icedia = (qn_icedia - self%qnmin_icedia) / (self%qnmax_icedia - self%qnmin_icedia)
        limnh4_icedia=1.0_rk
        limno3_icedia=1.0_rk
        
        ! Temp limitation
        limtemp = exp(-self%ear * (1.0_rk/(temp) - 1.0_rk/self%tempref) )

        ! Photosynthesis ice diatoms
        pcmax= self%pcref_icedia * limtemp * limnc_icedia
        limpar_icedia = 1.0_rk - exp( - self%alpha_icedia/pcmax * qchl_icedia * par)
        phot_icedia = pcmax * limpar_icedia * icedia_c

        ! N uptake
        limnc_icedia = (self%qnmax_icedia - qn_icedia) / (self%qnmax_icedia - self%qnmin_icedia)
        vn_icedia = self%vnref_icedia * limtemp * max(0.0_rk,limnc_icedia**0.05) * (limnh4_icedia + (1.0_rk - limnh4_icedia)*limno3_icedia) * icedia_c

        ! Chl synthesis
        rhochl_icedia = pcmax * limpar_icedia / max(1e-15,(self%alpha_icedia * qchl_icedia * par))
        synchl_icedia = rhochl_icedia * self%chltonmax_icedia * vn_icedia

        ! Respiration
        resp_icedia = self%zeta_icedia * vn_icedia

        ! Mortality
        if (icedia_c.lt.self%min_icedia) then ! low biomass
            mort_icedia = 0.0_rk
        else
            mort_icedia = self%mlin_icedia * icedia_c + self%mquad_icedia * icedia_c * icedia_c
        endif

        ! Ice diatoms dynamics
        if (icedia_c.lt.40000.0_rk) then ! max biomass
            _ADD_SURFACE_SOURCE_(self%id_icedia_c, phot_icedia - resp_icedia - mort_icedia )
        endif
        _ADD_SURFACE_SOURCE_(self%id_icedia_n, vn_icedia - mort_icedia*qn_icedia )
        _ADD_SURFACE_SOURCE_(self%id_icedia_chl, synchl_icedia - mort_icedia*qchl_icedia )



        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_bipar,par) 
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_limpar_icedia,limpar_icedia) 
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_vn_icedia,vn_icedia *86400.0_rk) 
        _SET_HORIZONTAL_DIAGNOSTIC_(self%id_rhochl_icedia, rhochl_icedia) 

        _HORIZONTAL_LOOP_END_
    end subroutine do_surface


end module uvic_iceeco