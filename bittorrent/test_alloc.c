#include <stdio.h>
#include <stdlib.h>


int main(int argc,char **argv) {

	int *array,count=1,i;

	array = (int *)malloc(count*sizeof(int));

	*array = 1;

	for(i=0;i<10;i++,count++) {
		array = (int *)realloc(array,count*sizeof(int));
		*(array+i) = count;
	}

	for(i=0;i<count-1;i++) {
		printf("%d\n",*(array+i));
	}

	free(array);

	return 0;
}
