# Unix makefile for tigermain example

HOME=/usr
#MOSMLHOME=${HOME}
MOSMLHOME=/usr/local/
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac -v

GCC=gcc
CFLAGS= -g
MOSMLC=${MOSMLHOME}/bin/mosmlc -c -liberal
MOSMLL=${MOSMLHOME}/bin/mosmlc

# Unix
REMOVE=rm -f
MOVE=mv
EXEFILE=

# DOS
#REMOVE=del
#MOVE=move
#EXEFILE=.exe

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

GRALOBJS= tigerabs.uo tigergrm.uo tigerlex.uo tigermain.uo \
	tigernlin.uo tigerpp.uo tigerescap.uo tigertab.uo tigerseman.uo tigertemp.uo topsort.uo tigertree.uo \
	tigerframe.uo tigertrans.uo tigerit.uo tigerpila.uo tigerinterp.uo tigerutils.uo \
	tigerassem.uo tigercodegen.uo tigergraph.uo tigerflow.uo tigerliveness.uo tigercoloring.uo

all: tiger

tiger: $(GRALOBJS) $(OBJSGEN)
	$(MOSMLL) -o tiger $(EXEFILE) tigermain.uo

tigergrm.sml tigergrm.sig: tigergrm.y 
	$(MOSMLYACC) tigergrm.y

tigerlex.sml: tigerlex.lex
	$(MOSMLLEX) tigerlex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) tigergrm.output
	$(REMOVE) tigergrm.sig
	$(REMOVE) tigergrm.sml
	$(REMOVE) tigerlex.sml
	$(REMOVE) tigermain
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) errlist
	$(REMOVE) *.o

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: tigerabs.sml tigergrm.sml tigerlex.sml tigermain.sml \
	tigernlin.sml tigerpp.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tigerpp.uo: tigerabs.uo tigersres.uo tigertrans.uo
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigerescap.ui: tigerabs.uo 
tigerinterp.ui: tigertree.uo tigerframe.ui tigertemp.ui 
tigerinterp.uo: tigerinterp.ui tigertree.uo tigertab.ui tigerframe.ui \
    tigerit.uo tigertemp.ui 
tigerframe.ui: tigertree.uo tigertemp.ui tigerassem.uo 
tigerframe.uo: tigerframe.ui tigertree.uo tigertemp.ui tigerassem.uo 
tigergrm.ui: tigerabs.uo 
tigerseman.ui: tigerabs.uo 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigerit.uo: tigertree.uo tigertab.ui 
tigertree.uo: tigertemp.ui 
tigerutils.ui: tigertree.uo
tigerutils.uo: tigerutils.ui tigertree.uo
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigerabs.uo tigertrans.ui topsort.uo
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigerpila.uo: tigerpila.ui 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigertab.uo: tigertab.ui 
tigerassem.uo: tigertemp.ui tigertemp.uo tigertab.uo
tigercodegen.ui: tigertree.uo tigerframe.ui tigerassem.uo tigerframe.ui tigerframe.uo 
tigercodegen.uo: tigerframe.ui tigerframe.uo tigercodegen.ui tigertree.uo tigerit.uo \
		    tigerassem.uo tigertemp.ui
tigertemp.uo: tigertemp.ui 
tigergraph.ui: tigerutils.ui 
tigergraph.uo: tigerutils.uo tigergraph.ui
tigerflow.ui : tigergraph.ui
tigerflow.uo : tigergraph.uo
tigerliveness.ui : tigerflow.ui
tigerliveness.uo : tigerflow.uo
tigercoloring.ui: tigerutils.ui tigergraph.ui tigerflow.ui tigerliveness.ui tigertab.ui
tigercoloring.uo: tigercoloring.ui tigerutils.uo tigergraph.uo tigerflow.uo tigerliveness.uo tigertab.uo tigerassem.uo tigerutils.uo
tigermain.uo: tigerseman.ui tigerescap.ui tigerinterp.ui tigergrm.ui tigerlex.uo \
    tigerpp.uo tigercanon.ui tigercanon.uo tigerinterp.uo tigerinterp.ui tigercodegen.ui tigerflow.ui \
	tigerliveness.ui tigerliveness.uo tigercoloring.ui tigercoloring.uo tigerframe.ui tigerframe.uo
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigerframe.ui \
    tigertemp.ui 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigercanon.ui: tigertree.uo tigerframe.ui tigertemp.ui
tigersimpleregalloc.uo: tigersimpleregalloc.ui
tigersimpleregalloc.ui: tigerframe.ui tigerassem.uo

