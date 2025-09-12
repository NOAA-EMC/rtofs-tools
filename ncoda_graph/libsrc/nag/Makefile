# NAG subroutine library for ncoda quality control and
#     ncoda diagnostics programs
#
# SCCS IDENTIFICATION:  %W% %G%
#
#....... Library and Include Directories ..........
#

include ../../config.os

include ../../config.user

LIB = ../../lib/libnag.a

FFLAGS = $(FFLAGS_GRAPH)

.PRECIOUS: $(LIB)

#
#.......... Source List ...........................
#

OBJS = \
   $(LIB)(aazf06t.o) \
   $(LIB)(aazg03t.o) \
   $(LIB)(abwp01t.o) \
   $(LIB)(abyp01t.o) \
   $(LIB)(abzp01t.o) \
   $(LIB)(aczg03t.o) \
   $(LIB)(baxs14t.o) \
   $(LIB)(bays14t.o) \
   $(LIB)(bazs14t.o) \
   $(LIB)(cgzg02t.o) \
   $(LIB)(cgyg02t.o) \
   $(LIB)(f01qcet.o) \
   $(LIB)(f01qdet.o) \
   $(LIB)(f01qeet.o) \
   $(LIB)(f01qjet.o) \
   $(LIB)(f01qket.o) \
   $(LIB)(f02swet.o) \
   $(LIB)(f02sxet.o) \
   $(LIB)(f02syet.o) \
   $(LIB)(f02weet.o) \
   $(LIB)(f02wuet.o) \
   $(LIB)(f04abet.o) \
   $(LIB)(f04afet.o) \
   $(LIB)(f04aget.o) \
   $(LIB)(f06baet.o) \
   $(LIB)(f06bcet.o) \
   $(LIB)(f06blet.o) \
   $(LIB)(f06dbet.o) \
   $(LIB)(f06eaet.o) \
   $(LIB)(f06edet.o) \
   $(LIB)(f06efet.o) \
   $(LIB)(f06fbet.o) \
   $(LIB)(f06fcet.o) \
   $(LIB)(f06fget.o) \
   $(LIB)(f06fjet.o) \
   $(LIB)(f06fqet.o) \
   $(LIB)(f06fret.o) \
   $(LIB)(f06klet.o) \
   $(LIB)(f06paet.o) \
   $(LIB)(f06pjet.o) \
   $(LIB)(f06pmet.o) \
   $(LIB)(f06qfet.o) \
   $(LIB)(f06qhet.o) \
   $(LIB)(f06qket.o) \
   $(LIB)(f06qtet.o) \
   $(LIB)(f06qxet.o) \
   $(LIB)(f06yaet.o) \
   $(LIB)(f06yjet.o) \
   $(LIB)(f06ypet.o) \
   $(LIB)(f07fdet.o) \
   $(LIB)(fdzf07t.o) \
   $(LIB)(g01ecet.o) \
   $(LIB)(g02bdet.o) \
   $(LIB)(g02caet.o) \
   $(LIB)(g02cget.o) \
   $(LIB)(g02chet.o) \
   $(LIB)(g03acet.o) \
   $(LIB)(jgrf04t.o) \
   $(LIB)(jgsf04t.o) \
   $(LIB)(jgtf04t.o) \
   $(LIB)(jguf04t.o) \
   $(LIB)(jgxf04t.o) \
   $(LIB)(jgyf04t.o) \
   $(LIB)(p01abet.o) \
   $(LIB)(p01acet.o) \
   $(LIB)(qawf01t.o) \
   $(LIB)(s14aaet.o) \
   $(LIB)(s14abet.o) \
   $(LIB)(s14baet.o) \
   $(LIB)(s15adet.o) \
   $(LIB)(wdzf02t.o) \
   $(LIB)(wexf02t.o) \
   $(LIB)(x02ajet.o) \
   $(LIB)(x02aket.o) \
   $(LIB)(x02alet.o) \
   $(LIB)(x02amet.o) \
   $(LIB)(x02daet.o) \
   $(LIB)(x03aaet.o) \
   $(LIB)(x04aaet.o) \
   $(LIB)(x04baet.o) \
   $(LIB)(xusf02t.o) \
   $(LIB)(xutf02t.o) \
   $(LIB)(xuuf02t.o) \
   $(LIB)(xuvf02t.o) \
   $(LIB)(xuwf02t.o) \
   $(LIB)(zayf07t.o) \
   $(LIB)(zazf07t.o)
 
target: $(OBJS)

########################################################################
# common rules $(LIB)(for $(LIB)(all Makefiles - do not edit
########################################################################

emptyrule:

clean:
	(if [ -d rii_files ] ; then rm -rf rii_files ; fi) ;
	$(RM_CMD) "#"*
