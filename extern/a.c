#include <stdio.h>

int i[2];

void changei()
{
	i[0] = 42;

}

int main(void)
{
	changei();
	dob();

	printf("%d\n", i[0]);

	return 0;
}
