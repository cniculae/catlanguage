########################## User's variables #####################
#
## The Caml sources (including camlyacc and camllex source files)

SOURCES = Parser.mly Lexer.ml ParseTree.ml Interpreter.ml

# The executable file to generate
#
EXEC = mysplinterpreter

########################## Advanced user's variables #####################
#
## The Caml compilers.
# You may fix here the path to access the Caml compiler on your machine
# # You may also have to add various -I options.
CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

# The list of Caml libraries needed by the program
# For instance:
LIBS= str.cma

CUSTOM=-custom

# # Default setting of the WITH* variables. Should be changed if your
# # local libraries are not found by the compiler

################ End of user's variables #####################


##############################################################
################ This part should be generic
################ Nothing to set up or fix here
##############################################################
all:: .depend.input .depend $(EXEC)

opt : $(EXEC).opt

#ocamlc -custom other options graphics.cma other files -cclib -lgraphics -cclib -lX11
#ocamlc -thread -custom other options threads.cma other files -cclib -lthreads
#ocamlc -custom other options str.cma other files -cclib -lstr
#ocamlc -custom other options nums.cma other files -cclib -lnums
#ocamlc -custom other options unix.cma other files -cclib -lunix
#ocamlc -custom other options dbm.cma other files -cclib -lmldbm -cclib -lndbm

SMLIY = $(SOURCES:.mly=.ml)
SMLIYL = $(SMLIY:.mll=.ml)
SMLYL = $(filter %.ml,$(SMLIYL))
OBJS = $(SMLYL:.ml=.cmo)
OPTOBJS = $(OBJS:.cmx=.cmx)


$(EXEC): $(OBJS)
	$(CAMLC) $(CUSTOM) -o $(EXEC) $(LIBS) $(OBJS)

# $(EXEC)-opt: $(OPTOBJS)
# #	$(CAMLOPT) -o $(EXEC) $(LIBS:.cma=.cmxa) $(OPTOBJS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

.mll.cmo:
	$(CAMLLEX) $<
	$(CAMLC) -c $*.ml

.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) -c $*.ml

.mly.cmo:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli
	$(CAMLC) -c $*.ml

.mly.cmx:
	$(CAMLYACC) $<
	$(CAMLOPT) -c $*.mli
	$(CAMLOPT) -c $*.ml

.mly.cmi:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

clean::
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f $(EXEC)
	rm -f $(EXEC).opt

.depend.input: Makefile
	@echo -n '--Checking Ocaml input files: '
	@(ls $(SMLIY) $(SMLIY:.ml=.mli) 2>/dev/null || true) \
	     >  .depend.new
	@diff .depend.new .depend.input 2>/dev/null 1>/dev/null && \
	    (echo 'unchanged'; rm -f .depend.new) || \
	    (echo 'changed'; mv .depend.new .depend.input)

depend: .depend

.depend:: $(SMLIY) .depend.input
	@echo '--Re-building dependencies'
	$(CAMLDEP) $(SMLIY) $(SMLIY:.ml=.mli) > .depend

include .depend
