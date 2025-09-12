#
# CONFIGURATION IDENTIFICATION:
#      $HeadURL$
#      @(#)$Id$
#
# This config.os file is included in the toplevel and the intermediate 
# Makefiles.
#
# -------------------------------------------------------------------
#
# General Macros

SHELL=/bin/sh

RM = rm -f

MAKE = gmake

RM_CMD = $(RM) *.l *.BAK *.bak *.o *.i core errs ,* *~ *.a .emacs_* \
*.mod *.s tags TAGS make*.log MakeOut 

# ------------------------------------------------------------------
# Prompt the user for os type
#

default:
	@echo "To make ncoda utility executables and libraries "
	@echo "type one of the following:"
	@echo "   make gnu"
	@echo "   make cray_intel"
	@echo "   make cray_pgi"
	@echo "   make dell_intel"
	@echo "   make ibm_intel"
	@echo "   make linux_intel"
	@echo "   make linux_pgi"
	@echo "   make clean"

#-------------------------------------------------------------------

