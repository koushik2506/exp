#include <stdio.h>
#include <string.h>
#include <malloc.h>


int main(void)
{

	char *str = "AT+COPS=?\r\n+COPS: (2,\"Bharat Karnataka\",\"CBW\",\"40471\"),(1,\"803\",\"803\",\"405803\"),(1,\"034\",\"034\",\"405034\"),(1,\"Hutch-Kamataka\",\"HUTCH\",\"40486\"),(1,\"010\",\"010\",\"405010\"),(1,\"Airtel\",\"AIRTEL\",\"40445\"),(1,\"820\",\"820\",\"405820\"),(1,\"Spice Telecom\",\"SPICE\",\"40444\"),,(0-4),(0-2)";

	char *start = NULL, *end = NULL;
	char *list = NULL;
	int len;
	int i;
	char field_num;
	char **op_list = NULL;
	char op_count = 0;


	start = strstr(str, "+COPS: ") + 7;

	end   = strstr(start, ",,");

	len = end - start;

	list = (char *)malloc((len + 1)*sizeof(char));

	strncpy(list, start, len);

	*(list + len) = 0;

	i = 0;
	field_num = 0;

	while ( i++ < len ) {

		char c ;
		c = *(list + i);

		switch(c) {

			case '(':

				field_num = 0;	
				break;

			case ',': 

				field_num++;
				break;

			case  '"':
				if ( field_num == 3 ) { // MNC 

					char *mnc_end;
					int  mnc_len;
					char *mnc;

					mnc_end = strstr(list + i + 1, "\"");

					mnc_len = mnc_end - (list + i + 1 );  

					mnc = (char *)malloc((mnc_len + 1)*sizeof(char));

					strncpy(mnc, (list + i + 1), mnc_len);

					*(mnc + mnc_len) = 0;

					op_list = realloc(op_list, (op_count + 1) * sizeof(char *)); 

					*(op_list + op_count) = mnc;

					op_count++;

					i += mnc_len + 2;

				}
				break;
		}
	}

	for ( i = 0; i < op_count; i++){

		fprintf(stdout, "%s\n", *(op_list + i));

		free(*(op_list+i));
	}

	free(op_list);
	free(list);


	return 0;
}
