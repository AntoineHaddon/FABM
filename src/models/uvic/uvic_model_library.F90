
!#include "fabm_driver.h"

!jpnote :: ??? based on other model_libraries that exist in other models 
!

! !MODULE: FABM model library
!
! !INTERFACE:
module uvic_model_library

    use fabm_types, only: type_base_model_factory, type_base_model
    
    use uvic_eco

    implicit none

    private

    type,extends(type_base_model_factory) :: type_factory
        contains
        procedure :: create
       ! procedure :: initialize
    end type


    type (type_factory),save,target,public :: uvic_model_factory

contains

    subroutine create(self,name,model)

        !
        ! !INPUT PARAMETERS:
        class (type_factory),intent(in) :: self
        character(*),        intent(in) :: name
        class (type_base_model),pointer :: model

        select case (name)
            case ('uvic_eco'); allocate(type_uvic_eco::model)  
           ! case ('uvic_dms');                  allocate(type_uvic_dms::model)
            !case ('uvic_icealgae');                  allocate(type_uvic_icealgae::model)
            !case ('uvic_dic');                  allocate(type_uvic_dic::model)
            !case ('uvic_icedms');                  allocate(type_uvic_icedms::model)
            !case ('uvic_npzd_papa');                  allocate(type_uvic_npzd_papa::model)
            case default
                call self%type_base_model_factory%create(name,model)
        end select 
    end subroutine create

end module uvic_model_library