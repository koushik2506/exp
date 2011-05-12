#include <execinfo.h>
#include <stdio.h>
#include <malloc.h>

void e1()
{
	void *buffer;
	int size;

	buffer = (void *)malloc(10 * sizeof(int));

	backtrace(&buffer, 10);

	backtrace_symbols_fd(buffer, size, 1);

}

void d()
{
	e1();

}

void b()
{
	d();
}

void a()
{
	b();
}


int main(int argc, char **argv)
{
	a();

	return 0;
}
