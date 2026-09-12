#
# CONFIGURATION IDENTIFICATION:
#      $HeadURL$
#      @(#)$Id$
#
# Coupled Ocean Data Assimilation - fnoc subroutine library

include ../../config.os

include ../../config.user

LIB  = ../../lib/libfnoc.a

FFLAGS = $(FFLAGS_FNOC)

.PRECIOUS: $(LIB)

OBJS = \
       $(LIB)(dtgchk.o)  \
       $(LIB)(dtgdif.o)  \
       $(LIB)(dtgmod.o)  \
       $(LIB)(dtgnum.o)  \
       $(LIB)(dtgops.o)

target: $(OBJS)

########################################################################
# common rules for all Makefiles - do not edit
########################################################################

emptyrule:

clean:
	(if [ -d rii_files ] ; then rm -rf rii_files ; fi) ; 
	$(RM_CMD) "#"*
