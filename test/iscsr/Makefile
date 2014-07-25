#
# Libraries used
#
BASEDIR=../..
INCDIR=$(BASEDIR)/include/
include $(INCDIR)/Make.inc.psblas
LIBDIR=$(BASEDIR)/lib/
PSBLAS_LIB= -L$(LIBDIR) -lpsb_util -lpsb_krylov -lpsb_prec -lpsb_base
LDLIBS=$(PSBLDLIBS)

FINCLUDES=$(FMFLAG)$(INCDIR) $(FMFLAG).


DPGOBJS=pdgenspmv.o psb_d_iscsr_mat_mod.o psb_d_iscsr_impl.o
EXEDIR=./runs

all: pdgenspmv

pdgenspmv.o  psb_d_iscsr_impl.o: psb_d_iscsr_mat_mod.o

pdgenspmv: $(DPGOBJS)
	$(F90LINK) $(LOPT) $(DPGOBJS) -o pdgenspmv $(PSBLAS_LIB) $(LDLIBS) 
	/bin/mv pdgenspmv $(EXEDIR) 



clean: 
	/bin/rm -f $(DPGOBJS)

lib:
	(cd ../../; make library)
verycleanlib:
	(cd ../../; make veryclean)

