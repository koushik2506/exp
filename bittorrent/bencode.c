#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef union {
	long long int integer;
	char *string;
	} bencode_basevalue;

// Copy and deallocate source
void copy_bencode_basevalue(bencode_basevalue *dest,bencode_basevalue source) {

	int source_string_length = strlen(source.string);
	(*dest).string = (char *)malloc(source_string_length*sizeof(char));

	strcpy((*dest).string,source.string);

	(*dest).integer = source.integer;

	free(source.string);

}

bencode_basevalue parse_string(char **input) {

	long int string_length; 
	long int input_length;
	
	char *return_string; 
	char *original_string;
	char *empty;

	bencode_basevalue return_value;


	input_length = strlen(*input);
	original_string = (char *)malloc(input_length*sizeof(char));
	strncpy(original_string,*input,(size_t)input_length);


	string_length = strtol(strtok(original_string,":"),NULL,10);
	return_string = (char *)malloc(string_length*sizeof(char));

	empty = strchr(*input,':') + 1;
	
	strncpy(return_string,empty,(size_t)string_length);

	*input = empty+string_length;

	free(original_string);

	return_value.string = return_string;

	return return_value;

}

bencode_basevalue parse_int(char **input) {

	int size;
	bencode_basevalue return_value;
	char *emptybuf;

	// From i to e, since i is the first char,
	size  = (strchr(*input,'e') - (*input+1));
	
	emptybuf = (char *)malloc(size*sizeof(char));
	strncpy(emptybuf,(*input+1),(size_t)size);

	return_value.integer  = strtoull(emptybuf,NULL,10);

	free(emptybuf);

	*input = *input+size+2;

	return return_value;


}

bencode_basevalue* parse_list(char **input,int *list_length) {

	int local_list_length = 0;
	char *process_input = (*input + 1);
	bencode_basevalue *return_value = NULL;

	do {
		switch (*process_input) {
			case 'i': 
				// Allocate memory for 1 bencode_basevalue
				return_value = (bencode_basevalue *)realloc(return_value,local_list_length+1*sizeof(bencode_basevalue));

			//	copy_bencode_basevalue(*(return_value+local_list_length),parse_int(&process_input));

				local_list_length++;

				break;
			default:
				// Allocate memory for 1 bencode_basevalue
				return_value = (bencode_basevalue *)realloc(return_value,local_list_length+1*sizeof(bencode_basevalue));

			//	copy_bencode_basevalue(*(return_value+local_list_length),parse_string(&process_input));

				local_list_length++;
				break;
		}
	}while(*process_input != 'e');

	*input = process_input + 1;

	return return_value;

}

#define INPUT "4:spam"

int main(int argc,char **argv) {

	char *input;
	int input_length = strlen(INPUT);
	bencode_basevalue result;

	input = (char *)malloc(input_length*sizeof(char));

	strncpy(input,INPUT,input_length);

	printf("%s\n",input);

	copy_bencode_basevalue(&result,parse_string(&input));

	printf("%s\n",result.string);

	free(input);

	return 0;

}
