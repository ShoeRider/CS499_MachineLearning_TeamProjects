all: Double  Test


Install_R:
	sudo apt-get install r-base

#C and C++ compilation:
Double:
	R CMD SHLIB Double.c

Double_CPP:
	R CMD SHLIB Double_cpp.cpp


#Commands to run R code
Test:
	Rscript Double.R

Save_Test:
	R CMD BATCH Double.R
	# save the output in the file Double.Rout
	cat Double.Rout


#removes all objects and executables
.PHONY: clean $(SUBDIRS)
clean :
	for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir clean; \
	done
	rm -f *.o *.gch *.so .RData *.RData *.Rout
