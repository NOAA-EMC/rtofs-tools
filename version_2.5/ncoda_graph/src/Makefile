#
# CONFIGURATION IDENTIFICATION:
#      $HeadURL$
#      @(#)$Id$
#
# -------------------------------------------------------------------
#

include ../config.os

#--------------------------------------------------------------
# 
#  For each os type, cd to subdirectory and invoke the makefile
#  with the given target.
#  
#--------------------------------------------------------------

gnu:
	(cd ncoda_crs; $(MAKE) gnu );
	(cd ncoda_diagn; $(MAKE) gnu );
	(cd ncoda_map; $(MAKE) gnu );
	(cd ncoda_movie; $(MAKE) gnu );

cray_intel:
	(cd ncoda_crs; $(MAKE) cray_intel );
	(cd ncoda_diagn; $(MAKE) cray_intel );
	(cd ncoda_map; $(MAKE) cray_intel );
	(cd ncoda_movie; $(MAKE) cray_intel );

cray_pgi:
	(cd ncoda_crs; $(MAKE) cray_pgi );
	(cd ncoda_diagn; $(MAKE) cray_pgi );
	(cd ncoda_map; $(MAKE) cray_pgi );
	(cd ncoda_movie; $(MAKE) cray_pgi );

dell_intel:
	(cd ncoda_crs; $(MAKE) dell_intel );
	(cd ncoda_diagn; $(MAKE) dell_intel );
	(cd ncoda_map; $(MAKE) dell_intel );
	(cd ncoda_movie; $(MAKE) dell_intel );

ibm_intel:
	(cd ncoda_crs; $(MAKE) ibm_intel );
	(cd ncoda_diagn; $(MAKE) ibm_intel );
	(cd ncoda_map; $(MAKE) ibm_intel );
	(cd ncoda_movie; $(MAKE) ibm_intel );

linux_intel:
	(cd ncoda_crs; $(MAKE) linux_intel );
	(cd ncoda_diagn; $(MAKE) linux_intel );
	(cd ncoda_map; $(MAKE) linux_intel );
	(cd ncoda_movie; $(MAKE) linux_intel );

linux_pgi: 
	(cd ncoda_crs; $(MAKE) linux_pgi );
	(cd ncoda_diagn; $(MAKE) linux_pgi );
	(cd ncoda_map; $(MAKE) linux_pgi );
	(cd ncoda_movie; $(MAKE) linux_pgi );

clean:
	$(RM_CMD) "#"*; \
	(cd ncoda_crs; $(MAKE) clean );
	(cd ncoda_diagn; $(MAKE) clean );
	(cd ncoda_map; $(MAKE) clean );
	(cd ncoda_movie; $(MAKE) clean );

#--------------------------------------------------------------
