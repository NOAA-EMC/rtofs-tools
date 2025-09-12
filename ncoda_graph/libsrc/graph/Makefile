#
# CONFIGURATION IDENTIFICATION:
#      $HeadURL$
#      @(#)$Id$
#
# NCODA ncar graphics subroutine library
#
#....... Library and Include Directories ..........
#

include ../../config.os

include ../../config.user 

LIB = ../../lib/libgraph.a

INCLUDE_DIR = ../../include

FFLAGS = $(FFLAGS_GRAPH) -I $(INCLUDE_DIR)

.PRECIOUS: $(LIB)

#
#.......... Source List ...........................
#

OBJS = \
       $(LIB)(contour_fld.o) \
       $(LIB)(contour_xsect.o) \
       $(LIB)(err_ser.o) \
       $(LIB)(glb_map.o) \
       $(LIB)(glb_merc.o) \
       $(LIB)(glb_polar.o) \
       $(LIB)(jmin_ser.o) \
       $(LIB)(map_obs.o) \
       $(LIB)(map_raw_ice.o) \
       $(LIB)(map_raw_prf.o) \
       $(LIB)(map_raw_ssh.o) \
       $(LIB)(map_raw_sss.o) \
       $(LIB)(map_raw_sst.o) \
       $(LIB)(map_raw_syn.o) \
       $(LIB)(map_ssh_uv.o) \
       $(LIB)(map_volumes.o) \
       $(LIB)(plot_antarctic.o) \
       $(LIB)(plot_arch.o) \
       $(LIB)(plot_arctic.o) \
       $(LIB)(plot_botprs.o) \
       $(LIB)(plot_btm.o) \
       $(LIB)(plot_chobs.o) \
       $(LIB)(plot_data.o) \
       $(LIB)(plot_diff.o) \
       $(LIB)(plot_dist.o) \
       $(LIB)(plot_diurnal.o) \
       $(LIB)(plot_ensm.o) \
       $(LIB)(plot_ensm_std.o) \
       $(LIB)(plot_err.o) \
       $(LIB)(plot_flds.o) \
       $(LIB)(plot_flow.o) \
       $(LIB)(plot_gpt.o) \
       $(LIB)(plot_grdnt.o) \
       $(LIB)(plot_ice.o) \
       $(LIB)(plot_jmin.o) \
       $(LIB)(plot_lyp.o) \
       $(LIB)(plot_mdl_bias.o) \
       $(LIB)(plot_mld.o) \
       $(LIB)(plot_movie.o) \
       $(LIB)(plot_obs.o) \
       $(LIB)(plot_obs_raw.o) \
       $(LIB)(plot_ohc.o) \
       $(LIB)(plot_polar_data.o) \
       $(LIB)(plot_prof.o) \
       $(LIB)(plot_prof_fcst.o) \
       $(LIB)(plot_prof_stats.o) \
       $(LIB)(plot_prof_vrfy.o) \
       $(LIB)(plot_sal.o) \
       $(LIB)(plot_scl.o) \
       $(LIB)(plot_sctr.o) \
       $(LIB)(plot_sens.o) \
       $(LIB)(plot_slatrk.o) \
       $(LIB)(plot_ssh.o) \
       $(LIB)(plot_sss.o) \
       $(LIB)(plot_sst.o) \
       $(LIB)(plot_stats.o) \
       $(LIB)(plot_stats_fcst.o) \
       $(LIB)(plot_stats_lvl.o) \
       $(LIB)(plot_tmp.o) \
       $(LIB)(plot_uv.o) \
       $(LIB)(plot_uv_inc.o) \
       $(LIB)(plot_uv_sfc.o) \
       $(LIB)(plot_vol.o) \
       $(LIB)(plot_vrfy.o) \
       $(LIB)(plot_xsect.o) \
       $(LIB)(plot_xsect_ensm.o) \
       $(LIB)(prof_fcst.o) \
       $(LIB)(prof_trp.o) \
       $(LIB)(prof_vrfy.o) \
       $(LIB)(ssh_corr.o) \
       $(LIB)(stats_fcst.o) \
       $(LIB)(stats_ser.o) \
       $(LIB)(stats_ser_lvl.o) \
       $(LIB)(xsect_fld.o)

INCLUDES = \
       $(INCLUDE_DIR)/coda_types.h \
       $(INCLUDE_DIR)/color_table.h

target: $(OBJS)

########################################################################
# Dependencies
########################################################################

${LIB}(jmin_ser.o)        : $(INCLUDE_DIR)/coda_types.h \
                            $(INCLUDE_DIR)/color_table.h
${LIB}(map_raw_ice.o)     : $(INCLUDE_DIR)/color_table.h
${LIB}(map_raw_prf.o)     : $(INCLUDE_DIR)/color_table.h
${LIB}(map_raw_ssh.o)     : $(INCLUDE_DIR)/color_table.h
${LIB}(map_raw_sss.o)     : $(INCLUDE_DIR)/color_table.h
${LIB}(map_raw_sst.o)     : $(INCLUDE_DIR)/color_table.h
${LIB}(map_raw_syn.o)     : $(INCLUDE_DIR)/color_table.h
${LIB}(map_ssh_uv.o)      : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_arctic.o)     : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_btm.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_chobs.o)      : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_data.o)       : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_dist.o)       : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_diurnal.o)    : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_ensm.o)       : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_flds.o)       : $(INCLUDE_DIR)/omapnl.h
${LIB}(plot_flow.o)       : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_gpt.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_grdnt.o)      : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_ice.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_lyp.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_mld.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_movie.o)      : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_obs.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_ohc.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_sal.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_scl.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_sctr.o)       : $(INCLUDE_DIR)/coda_types.h
${LIB}(plot_sens.o)       : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_slatrk.o)     : $(INCLUDE_DIR)/coda_types.h \
                            $(INCLUDE_DIR)/color_table.h
${LIB}(plot_ssh.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_sss.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_sst.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_stats.o)      : $(INCLUDE_DIR)/coda_types.h
${LIB}(plot_stats_lvl.o)  : $(INCLUDE_DIR)/coda_types.h 
${LIB}(plot_tmp.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_uv.o)         : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_uv_sfc.o)     : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_vol.o)        : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_vol_a.o)      : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_vrfy.o)       : $(INCLUDE_DIR)/color_table.h
${LIB}(plot_xsect_ensm.o) : $(INCLUDE_DIR)/color_table.h
${LIB}(stats_ser_lvl.o)   : $(INCLUDE_DIR)/color_table.h
${LIB}(xsect_fld.o)       : $(INCLUDE_DIR)/color_table.h

########################################################################
# common rules for all Makefiles - do not edit
########################################################################

emptyrule:

clean:
	(if [ -d rii_files ] ; then rm -rf rii_files ; fi) ; 
	$(RM_CMD) "#"*
