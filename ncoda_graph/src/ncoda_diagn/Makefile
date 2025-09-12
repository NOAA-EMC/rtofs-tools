#
# CONFIGURATION IDENTIFICATION:
#      $HeadURL$
#      @(#)$Id$
#
#   makefile to build program ncoda diagnostic graphics
#
#   other dependencies

include ../../config.os

include ../../config.user

INCLUDE_DIR = ../../include

LIBDIR   = ../../lib

LIBS   = \
	$(LIBDIR)/libdiagn.a \
	$(LIBDIR)/libgraph.a \
	$(LIBDIR)/libnag.a \
	$(LIBDIR)/libutil.a \
	$(LIBDIR)/libfnoc.a

OBJS     = ncoda_diagn.o            

PROGRAM  = ../../bin/rtofs_ncoda_diagn

target : $(PROGRAM) 

#   loader directive

$(PROGRAM) : $(OBJS) $(LIBS)
	$(FC) $(LDFLAGS_GRAPH) -o $@ $(OBJS) $(LIBS) $(GKSLIBS)

.f.o:
	$(RM) $@
	$(FC) -c $(FFLAGS_GRAPH) -I$(INCLUDE_DIR) $*.f

########################################################################
# common rules for all Makefiles - do not edit
########################################################################

emptyrule:

clean:
	(if [ -d rii_files ] ; then rm -rf rii_files ; fi) ; 
	$(RM_CMD) "#"*

