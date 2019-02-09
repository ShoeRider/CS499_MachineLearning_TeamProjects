#TODO Create
ALL: QuickSort

#TODO include a recursive call to compile, and Run nested Programs

#removes all objects and executables
.PHONY: clean $(SUBDIRS)
clean :
	for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir clean; \
	done
	rm -f *.o *.gch *.so
