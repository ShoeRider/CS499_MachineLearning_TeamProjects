#TODO Create
ALL: QuickSort

#TODO include/Create/Verify these recipes work:  

#Compile QuickSort with no optimization 
QuickSort : QuickSort.o $(ALLOBJS)
	$(CC) $(LFLAGS) QuickSort.o $(ALLOBJS) -o QuickSort -lm -lgomp -fopenmp -o QuickSort
  
  
#removes all objects and executables
.PHONY: clean $(SUBDIRS)
clean :
	for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir clean; \
	done
	rm -f *.o *.gch                \
	question1/question1_Anthony    \
	question2/question2_Anthony    \
	question3/question3_Anthony    \
	question4/question4_Anthony    \
