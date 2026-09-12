#
# CONFIGURATION IDENTIFICATION:
#      $HeadURL$
#      @(#)$Id$
#
# Make file to build NCODA graphics libraries
# 
# -------------------------------------------------------------------
#
# Include the common definitions for os types

include ../config.os

#--------------------------------------------------------------
# 
#  For each os type, cd to subdirectory and invoke the makefile
#  with the given target.
#  
#--------------------------------------------------------------

gnu:
	(cd diagn; $(MAKE) gnu );
	(cd fnoc; $(MAKE) gnu );
	(cd graph; $(MAKE) gnu );
	(cd nag; $(MAKE) gnu );
	(cd util ; $(MAKE) gnu );

cray_intel:
	(cd diagn; $(MAKE) cray_intel );
	(cd fnoc; $(MAKE) cray_intel );
	(cd graph; $(MAKE) cray_intel );
	(cd nag; $(MAKE) cray_intel );
	(cd util ; $(MAKE) cray_intel );

cray_pgi:
	(cd diagn; $(MAKE) cray_pgi );
	(cd fnoc; $(MAKE) cray_pgi );
	(cd graph; $(MAKE) cray_pgi );
	(cd nag; $(MAKE) cray_pgi );
	(cd util ; $(MAKE) cray_pgi );

dell_intel:
	(cd diagn; $(MAKE) dell_intel );
	(cd fnoc; $(MAKE) dell_intel );
	(cd graph; $(MAKE) dell_intel );
	(cd nag; $(MAKE) dell_intel );
	(cd util; $(MAKE) dell_intel );

ibm_intel:
	(cd diagn; $(MAKE) ibm_intel );
	(cd fnoc; $(MAKE) ibm_intel );
	(cd graph; $(MAKE) ibm_intel );
	(cd nag; $(MAKE) ibm_intel );
	(cd util; $(MAKE) ibm_intel );

linux_intel:
	(cd diagn; $(MAKE) linux_intel );
	(cd fnoc; $(MAKE) linux_intel );
	(cd graph; $(MAKE) linux_intel );
	(cd nag; $(MAKE) linux_intel );
	(cd util; $(MAKE) linux_intel );

linux_pgi:
	(cd diagn; $(MAKE) linux_pgi ); 
	(cd fnoc; $(MAKE) linux_pgi ); 
	(cd graph; $(MAKE) linux_pgi );
	(cd nag; $(MAKE) linux_pgi );
	(cd util; $(MAKE) linux_pgi );

clean:
	$(RM_CMD) "#"*; 
	(cd diagn; $(MAKE) clean );
	(cd fnoc; $(MAKE) clean );
	(cd graph; $(MAKE) clean );
	(cd nag; $(MAKE) clean );
	(cd util; $(MAKE) clean );

