makefile_common=~/.emacs.d/site-lisp/Makefile.common
mymake=make -f $(makefile_common)
subdirs=. dashboard generic-apt

all: #byte-compile
	@echo "byte-compile disabled now."

byte-compile:
	for i in $(subdirs); do \
	(echo "=== make in $${i} ==="; cd $${i}; $(mymake); cd -); \
	done

clean:
	for i in $(subdirs); do \
	(echo "=== make clean in $${i} ==="; cd $${i}; $(mymake) clean; cd -); \
	done
