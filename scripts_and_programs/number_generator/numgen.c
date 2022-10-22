//build command (gcc 12.1.0):
//gcc numgen.c -O2 -o numgen.exe

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

int main()
{
	//set the max size in characters of the Start Number, End Number, Number Prefix, Number Suffix
	printf("Set Input Length (if unsure put 1000): ");
	char length[1000];
	fgets(length, sizeof(length), stdin);
	int length_int;
	sscanf(length, "%d", &length_int);

	printf("Start Number: ");
	char start_number[length_int];
	fgets(start_number, sizeof(start_number), stdin);
	int start_number_int;
	sscanf(start_number, "%d", &start_number_int);

	printf("End Number: ");
	char end_number[length_int];
	fgets(end_number, sizeof(end_number), stdin);
	int end_number_int;
	sscanf(end_number, "%d", &end_number_int);

	printf("Number Prefix: ");
	char number_prefix[length_int];
	fgets(number_prefix, sizeof(number_prefix), stdin);

	printf("Number Suffix: ");
	char number_suffix[length_int];
	fgets(number_suffix, sizeof(number_suffix), stdin);

	//benchmarking code
	float start_time = (float)clock()/CLOCKS_PER_SEC;

	//remove newlines  at end
	//suffix newline is not removed
	size_t ln0 = strlen(number_prefix)-1;
	if (number_prefix[ln0] == '\n')
    	number_prefix[ln0] = '\0';
	
	int current_number = start_number_int;
	char current_number_char[strlen(end_number)];
	char* string_combined;
	string_combined = malloc(strlen(number_prefix) + strlen(end_number) + strlen(number_suffix) + 1);

	FILE *output_file;

	output_file = fopen("output.txt", "w+");

	while (current_number <= end_number_int)
	{
		sprintf(current_number_char, "%d", current_number);
		strcpy(string_combined, number_prefix);
		strcat(string_combined, current_number_char);
		strcat(string_combined, number_suffix);

		fputs(string_combined, output_file);

		current_number++;
	}
	free(string_combined);

	fclose(output_file);

	//benchmarking code
	float end_time = (float)clock()/CLOCKS_PER_SEC;
	float time_elapsed = end_time - start_time;
	printf("Generated in: ");
	printf("%f", time_elapsed);
	printf(" Seconds");

	return 0;
}