#
# CONFIGURATION IDENTIFICATION:
#      $HeadURL$
#      @(#)$Id$
#
# NCODA ncar diagnostics subroutine library
#
#....... Library and Include Directories ..........
#

include ../../config.os

include ../../config.user 

LIB = ../../lib/libdiagn.a

INCLUDE_DIR = ../../include

FFLAGS = $(FFLAGS_GRAPH) -I $(INCLUDE_DIR)

.PRECIOUS: $(LIB)

#
#.......... Source List ...........................
#

OBJS = \
       $(LIB)(adj_diagn_glb.o) \
       $(LIB)(adj_diagn_ice.o) \
       $(LIB)(adj_diagn_prf.o) \
       $(LIB)(adj_diagn_sfc.o) \
       $(LIB)(adj_sens_ice.o) \
       $(LIB)(adj_sens_prf.o) \
       $(LIB)(adj_sfc_bkg.o) \
       $(LIB)(argo_mdb_diagn.o) \
       $(LIB)(contour_count.o) \
       $(LIB)(contour_point.o) \
       $(LIB)(impact_hist_glb.o) \
       $(LIB)(impact_hist_lat.o) \
       $(LIB)(impact_hist_prf.o) \
       $(LIB)(impact_hist_sfc.o) \
       $(LIB)(map_adj_sens.o) \
       $(LIB)(map_bias.o) \
       $(LIB)(map_impact_glb.o) \
       $(LIB)(map_impact_ice.o) \
       $(LIB)(map_impact_prf.o) \
       $(LIB)(map_impact_sfc.o) \
       $(LIB)(model_bias.o) \
       $(LIB)(model_point.o) \
       $(LIB)(plt_sss_bias.o) \
       $(LIB)(plt_sss_sctr.o) \
       $(LIB)(rd_hycom_clim.o) \
       $(LIB)(time_impact_prf.o) \
       $(LIB)(vert_hist_prf.o) \
       $(LIB)(vrfy_map.o) \
       $(LIB)(vrfy_obs.o) \
       $(LIB)(vrfy_obs_fcst.o) \
       $(LIB)(vrfy_rgn.o) \
       $(LIB)(vrfy_rgn_fcst.o) \
       $(LIB)(vrfy_sctr.o) \
       $(LIB)(vrfy_ssh.o) \
       $(LIB)(vrfy_ssh_map.o) \
       $(LIB)(vrfy_ssh_rgn.o) \
       $(LIB)(vrfy_time.o) \
       $(LIB)(vrfy_vert.o)

INCLUDES = \
       $(INCLUDE_DIR)/coda_types.h \
       $(INCLUDE_DIR)/color_table.h

target : $(OBJS)

########################################################################
# Dependencies
########################################################################

${LIB}(contour_count.o)    : $(INCLUDE_DIR)/color_table.h
${LIB}(impact_hist_buoy.o) : $(INCLUDE_DIR)/coda_types.h \
                             $(INCLUDE_DIR)/color_table.h
${LIB}(impact_hist_lat.o)  : $(INCLUDE_DIR)/coda_types.h \
                             $(INCLUDE_DIR)/color_table.h
${LIB}(impact_hist_prf.o)  : $(INCLUDE_DIR)/coda_types.h \
                             $(INCLUDE_DIR)/color_table.h
${LIB}(impact_hist_sfc.o)  : $(INCLUDE_DIR)/coda_types.h \
                             $(INCLUDE_DIR)/color_table.h
${LIB}(map_adj_sens.o)     : $(INCLUDE_DIR)/color_table.h
${LIB}(map_bias)           : $(INCLUDE_DIR)/color_table.h
${LIB}(map_impact_ice.o)   : $(INCLUDE_DIR)/coda_types.h \
                             $(INCLUDE_DIR)/color_table.h
${LIB}(map_impact_prf.o)   : $(INCLUDE_DIR)/coda_types.h \
                             $(INCLUDE_DIR)/color_table.h
${LIB}(map_impact_sfc.o)   : $(INCLUDE_DIR)/coda_types.h \
                             $(INCLUDE_DIR)/color_table.h
${LIB}(plt_sss_bias..o)    : $(INCLUDE_DIR)/color_table.h
${LIB}(plt_sss_sctr..o)    : $(INCLUDE_DIR)/color_table.h
${LIB}(time_impact_prf.o)  : $(INCLUDE_DIR)/color_table.h
${LIB}(vert_hist_prf.o)    : $(INCLUDE_DIR)/coda_types.h \
                             $(INCLUDE_DIR)/color_table.h
${LIB}(vrfy_map.o)         : $(INCLUDE_DIR)/color_table.h
${LIB}(vrfy_sctr.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(vrfy_time.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(vrfy_vert.o)        : $(INCLUDE_DIR)/coda_types.h \
                             $(INCLUDE_DIR)/color_table.h

########################################################################
# common rules for all Makefiles - do not edit
########################################################################

emptyrule:

clean:
	(if [ -d rii_files ] ; then rm -rf rii_files ; fi) ; 
	$(RM_CMD) "#"*
