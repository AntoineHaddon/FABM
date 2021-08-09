add_library(fabm_models_uvic OBJECT
            uvic_model_library.F90
            eco.F90
           )

add_dependencies(fabm_models_uvic fabm_base)

#-----------------------------------------------------------------------
# Copyright (C) 2016 - Karsten Bolding and Jorn Bruggeman (BB)         !
#-----------------------------------------------------------------------
#icealgae.F90
            #dms.F90
            #npzd_papa.F90
            #icedms.F90