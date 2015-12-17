############################################################
## Makefile for the dOvs CamlJoos 0 compiler
## ---------------------------------------------------------
##
## Usage:
##   make [target]
##
## Targets:
##   joos0.byte     -- build an ocaml byte code joos compiler (default target)
##   joos0.native   -- build an native code joos compiler
##   joos0.top      -- build an interactive joos compiler toplevel
##   joos0.debug    -- build a joos compiler with debugging symbols
##   joos0.profile  -- build a joos compiler with profiling instrumentation
##   htdoc          -- generate documentation (in doc/index.html)
##   libs           -- build and install ocaml library dependencies
##   clean          -- clean up all build related files
##
############################################################

# (just ignore this thing...)
.PHONY: default all \
        joos0.byte joos0.native joos0.top joos0.debug joos0.profile \
        htdoc clean prepare findlib javalib libs

# Default build target (defined below)
# should probably be either 'joos0.native' or 'joos0.byte'
joos0: joos0.byte

# Default target for Eclipse
all: joos0


test:
	./joos0 tests/*.java
	cd tests ; jasc *.j
#ok	cd tests ; java A
	cd tests ; java B
#ok	cd tests ; java Binop
	cd tests ; java C
#	cd tests ; java Cons
	cd tests ; java ConsMain
	cd tests ; java D
	cd tests ; java FacIter
	cd tests ; java FacRec
#ok	cd tests ; java Field3
	cd tests ; java Field4
	cd tests ; echo 1 | java IntegerToString 
	cd tests ; java Main2
#ok	cd tests ; java New
#ok	cd tests ; java New2
	cd tests ; java New3
#ok	cd tests ; java While
	cd tests ; java Xor

test0:
	cd test_joos0; ../joos0 *.java
	cd test_joos0; jasc *.j
	cd test_joos0; java test_arithmeticoperations
	cd test_joos0; java test_boolean
	cd test_joos0; java test_booleanliterals
	cd test_joos0; java test_comparisonoperations
	cd test_joos0; java test_eagerbooleanoperations
	cd test_joos0; java test_fieldinitializers
	cd test_joos0; java test_if
	cd test_joos0; java test_implicitstringconcatenation
	cd test_joos0; java test_implicitsupercall
	cd test_joos0; java test_int
	cd test_joos0; java test_intliterals
	cd test_joos0; java test_nullliteral
	cd test_joos0; java test_protectedfields
	cd test_joos0; java test_publicclasses
	cd test_joos0; java test_publicconstructors
	cd test_joos0; java test_publicmethods
	cd test_joos0; java test_stringliterals
	cd test_joos0; java test_while

test1:
	cd test_joos1; ../joos0 *.java


test2:
	cd test_joos2; ../joos0 *.java
test3:
	cd test_joos3; ../joos0 *.java

# Non-source directories (comma separated, no spaces)
# Add directories that do not contain ml source files.
# Fx directories with test cases (will speed up compilation)
EXCL_DIRS := doc

# dOvs modules for generating a toplevel and documentation
# (ie, with source code in src/<module>.ml)
MODS := error ast types \
        parser lexer \
        env \
        link \
        typing \
        constfold \
        res \
        inst \
        codegen \
        limits \
        emit \
        main

############################################################
## YOU SHOULD NOT NEED TO EDIT ANYTHING BELOW THIS LINE
############################################################

# Directories
SRC_DIR := src
DOC_DIR := doc
BLD_DIR := _build

# Base invokation of ocamlbuild
OCAMLBUILD := ocamlbuild -no-links -use-menhir -menhir "menhir -v" -Xs $(EXCL_DIRS)

# Include, linking and library flags
LIBS := str,unix

joos0.byte: prepare
	@echo "*** Building joos0.byte"
	@$(OCAMLBUILD) -libs $(LIBS) $(SRC_DIR)/main.byte
	@ln -sf $(BLD_DIR)/$(SRC_DIR)/main.byte joos0.byte
	@ln -sf joos0.byte joos0

joos0.native: prepare
	@echo "*** Building joos0.native"
	@$(OCAMLBUILD) -libs $(LIBS) $(SRC_DIR)/main.native
	@ln -sf $(BLD_DIR)/$(SRC_DIR)/main.native joos0.native
	@ln -sf joos0.native joos0

joos0.debug: prepare
	@echo "*** Building joos0.debug"
	@$(OCAMLBUILD) -libs $(LIBS) $(SRC_DIR)/main.d.byte
	@ln -sf _build/$(SRC_DIR)/main.d.byte joos0.debug

joos0.profile: prepare
	@echo "*** Building joos0.profile"
	@$(OCAMLBUILD) -libs $(LIBS) $(SRC_DIR)/main.p.native
	@ln -sf _build/$(SRC_DIR)/main.p.native joos0.profile

joos0.top: prepare
	@echo "*** Building joos0.top"
	@echo $(MODS:%=$(SRC_DIR)/%) > $(SRC_DIR)/main.mltop
	@$(OCAMLBUILD) -libs $(LIBS) $(SRC_DIR)/main.top
	@rm -f $(SRC_DIR)/main.mltop
	@ln -sf _build/$(SRC_DIR)/main.top joos0.top

htdoc: joos0.byte
	@echo "*** Building $(DOC_DIR)"
	@mkdir -p $(DOC_DIR)
	@ocamldoc -d $(DOC_DIR) -I $(BLD_DIR)/$(SRC_DIR) \
	  -html $(MODS:%=$(BLD_DIR)/$(SRC_DIR)/%.ml)
	@echo "Open $(DOC_DIR)/index.html to browse the documentation."

clean:
	$(OCAMLBUILD) -clean
	rm -rf $(DOC_DIR)
	rm -f joos0 joos0.byte joos0.native joos0.top joos0.debug joos0.profile
	rm -f $(SRC_DIR)/main.mltop
	rm -f .ocamlinit
	rm -f parser.automaton
	rm -f tests/*.class
## Some specially generated files
.ocamlinit:
	@echo "#directory \"$(BLD_DIR)/$(SRC_DIR)\";;" > .ocamlinit

parser.automaton:
	@ln -sf $(BLD_DIR)/$(SRC_DIR)/parser.automaton parser.automaton

prepare: .ocamlinit parser.automaton
