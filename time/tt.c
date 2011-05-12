#include <time.h>
#include <stdio.h>

int main(int argc, char **argv)
{
	time_t curr_time;
	struct tm *tm_struct;
	char formatted_time[32];

	time(&curr_time);

	tm_struct = localtime(&curr_time);

	strftime(formatted_time, 32, "%H:%M, %d %b %Y", tm_struct);

	printf("%s", formatted_time);


	return 0;
}
