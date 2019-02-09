all: KNN_Test




Install_R:
	sudo apt-get install r-base



#Commands to run R code
KNN_Test:
	Rscript KNN.R

Save_Test:
	R CMD BATCH KNN.R
	# save the output in the file Double.Rout
	cat KNN.Rout


#removes all objects and executables
.PHONY: clean $(SUBDIRS)
clean :
	for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir clean; \
	done
	rm -f *.o *.gch *.so .RData *.RData *.Rout
