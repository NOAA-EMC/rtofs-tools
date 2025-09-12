#
# CONFIGURATION IDENTIFICATION:
#      $HeadURL$
#      @(#)$Id$
#
# NCODA ncar utility subroutine library
#
#....... Library and Include Directories ..........
#

include ../../config.os

include ../../config.user 

LIB = ../../lib/libutil.a

INCLUDE_DIR = ../../include

FFLAGS = $(FFLAGS_GRAPH) -I $(INCLUDE_DIR)

.PRECIOUS: $(LIB)

#
#.......... Source List ...........................
#

OBJS = \
       $(LIB)(clin.o) \
       $(LIB)(cmap.o) \
       $(LIB)(coamps_grid.o) \
       $(LIB)(coda_xtnd.o) \
       $(LIB)(colram.o) \
       $(LIB)(colrcl.o) \
       $(LIB)(cr_fname.o) \
       $(LIB)(crsram.o) \
       $(LIB)(crsrcl.o) \
       $(LIB)(dtg_time.o) \
       $(LIB)(error_exit.o) \
       $(LIB)(fld2_trp.o) \
       $(LIB)(fld3_trp.o) \
       $(LIB)(gks_color.o) \
       $(LIB)(great_circle.o) \
       $(LIB)(init_gks.o) \
       $(LIB)(irreg_ll2ij.o) \
       $(LIB)(ij2ll.o) \
       $(LIB)(lbl_axis.o) \
       $(LIB)(lbl_xsect.o) \
       $(LIB)(ll2ij.o) \
       $(LIB)(map_bkg.o) \
       $(LIB)(rd_coda_file.o) \
       $(LIB)(rd_dpth_msk.o) \
       $(LIB)(rd_ll_lvl.o) \
       $(LIB)(rd_ssh_mean.o) \
       $(LIB)(rd_topog.o) \
       $(LIB)(set_uplow.o) \
       $(LIB)(set_zoom.o) \
       $(LIB)(smth_2d.o) \
       $(LIB)(svan.o) \
       $(LIB)(time_dtg.o) \
       $(LIB)(title_plot.o)

target: $(OBJS)

########################################################################
# Dependencies
########################################################################

########################################################################
# common rules for all Makefiles - do not edit
########################################################################

emptyrule:

clean:
	(if [ -d rii_files ] ; then rm -rf rii_files ; fi) ; 
	$(RM_CMD) "#"*
