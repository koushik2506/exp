#include <stdio.h>

void func(void *a)
{
	int b = *(int *)a;

	fprintf(stderr, "b is %d\n" ,b);

}

int main(void)
{
	int a = 3;

	func((void *)&a);

	return 0;
}
