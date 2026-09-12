#
# CONFIGURATION IDENTIFICATION:
#      $HeadURL$
#      @(#)$Id$
#
# -------------------------------------------------------------------
#

include ./config.os

#------------------------------------------------------------------
# 
#  cd to subdirectory and invoke the makefile with the given target
#  
#-------------------------------------------------------------------

gnu:
	if [ ! -d lib ] ; then mkdir lib ; fi ;
	if [ ! -d bin ] ; then mkdir bin ; fi ;
	(cd libsrc; $(MAKE) gnu );
	(cd src; $(MAKE) gnu );

cray_intel:
	if [ ! -d lib ] ; then mkdir lib ; fi ;
	if [ ! -d bin ] ; then mkdir bin ; fi ;
	(cd libsrc; $(MAKE) cray_intel );
	(cd src; $(MAKE) cray_intel );

cray_pgi:
	if [ ! -d lib ] ; then mkdir lib ; fi ;
	if [ ! -d bin ] ; then mkdir bin ; fi ;
	(cd libsrc; $(MAKE) cray_pgi );
	(cd src; $(MAKE) cray_pgi );

dell_intel:
	if [ ! -d lib ] ; then mkdir lib ; fi ;
	if [ ! -d bin ] ; then mkdir bin ; fi ;
	(cd libsrc; $(MAKE) dell_intel );
	(cd src; $(MAKE) dell_intel );

ibm_intel:
	if [ ! -d lib ] ; then mkdir lib ; fi ;
	if [ ! -d bin ] ; then mkdir bin ; fi ;
	(cd libsrc; $(MAKE) ibm_intel );
	(cd src; $(MAKE) ibm_intel );

linux_intel:
	if [ ! -d lib ] ; then mkdir lib ; fi ;
	if [ ! -d bin ] ; then mkdir bin ; fi ;
	(cd libsrc; $(MAKE) linux_intel );
	(cd src; $(MAKE) linux_intel );

linux_pgi:
	if [ ! -d lib ] ; then mkdir lib ; fi ; 
	if [ ! -d bin ] ; then mkdir bin ; fi ; 
	(cd libsrc; $(MAKE) linux_pgi ); 
	(cd src; $(MAKE) linux_pgi );

clean:
	$(RM_CMD) "#"*; 
	(cd libsrc; $(MAKE) clean); 
	(cd src; $(MAKE) clean);
	(if [ -d lib ] ; then cd lib; rm -f *.a ; fi) ; 
	(if [ -d bin ] ; then cd bin; rm -f dtg rtofs_ncoda* ; fi) ; 
