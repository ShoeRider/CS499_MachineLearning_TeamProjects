#TODO Create
ALL: QuickSort

#TODO include/Create/Verify these recipes work:

#Compile QuickSort with no optimization
QuickSort : QuickSort.o $(ALLOBJS)
	$(CC) $(LFLAGS) QuickSort.o $(ALLOBJS) -o QuickSort -lm -lgomp -fopenmp -o QuickSort

doubler:
	R CMD SHLIB doubler.c

RunDoubler_Test:
	Rscript
#removes all objects and executables
.PHONY: clean $(SUBDIRS)
clean :
	for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir clean; \
	done
	rm -f *.o *.gch *.so
