//build command (gcc 12.1.0):
//gcc -O2 numgen_block_dumping_test.c -o numgen_block_dumping_test.exe

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

	printf("Max memory usage (kb): ");
	char max_memory[1000];
	fgets(max_memory, sizeof(max_memory), stdin);
	int max_memory_int;
	sscanf(max_memory, "%d", &max_memory_int);

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
	char number_prefix_optimized[strlen(number_prefix)];
	strcpy(number_prefix_optimized, number_prefix);

	printf("Number Suffix: ");
	char number_suffix[length_int];
	fgets(number_suffix, sizeof(number_suffix), stdin);
	char number_suffix_optimized[strlen(number_suffix)];
	strcpy(number_suffix_optimized, number_suffix);

	typedef int bool;
	enum { false, true };

	//benchmarking code
	float start_time = (float)clock()/CLOCKS_PER_SEC;

	//remove newlines  at end
	//suffix newline is not removed
	size_t ln0 = strlen(number_prefix_optimized)-1;
	if (number_prefix_optimized[ln0] == '\n')
		number_prefix_optimized[ln0] = '\0';
	
	int current_number = start_number_int;
	char current_number_char[strlen(end_number)];
	char* string_combined;
	string_combined = malloc(strlen(number_prefix_optimized) + strlen(end_number) + strlen(number_suffix_optimized) + 1);

	FILE *output_file;

	output_file = fopen("output.txt", "w+");

	int base_length = strlen(number_prefix_optimized) + strlen(end_number) + strlen(number_suffix_optimized) + 1;

	if (max_memory_int == 0)
	{
		char* entire_string;
		entire_string = malloc(base_length);
		char* empty_string = "";
		strcpy(entire_string, empty_string);
		while (current_number <= end_number_int)
		{
			entire_string = realloc(entire_string, (strlen(number_prefix_optimized) + strlen(end_number) + strlen(number_suffix_optimized)) * (current_number - start_number_int + 1) + 1);
			sprintf(current_number_char, "%d", current_number);
			strcpy(string_combined, number_prefix_optimized);
			strcat(string_combined, current_number_char);
			strcat(string_combined, number_suffix_optimized);
			strcat(entire_string, string_combined);
			current_number++;
		}
		fputs(entire_string, output_file);
		free(entire_string);
	}
	else if (max_memory_int == 1)
	{
		while (current_number <= end_number_int)
		{
			sprintf(current_number_char, "%d", current_number);
			strcpy(string_combined, number_prefix_optimized);
			strcat(string_combined, current_number_char);
			strcat(string_combined, number_suffix_optimized);

			fputs(string_combined, output_file);

			current_number++;
		}
	}
	else
	{
		char* entire_string;
		char* empty_string = "";
		int counter = 0;
		bool written = false;
		bool malloc_now = true;
		while (current_number <= end_number_int)
		{
			written = false;
			if (malloc_now)
			{
				entire_string = malloc(base_length);
				strcpy(entire_string, empty_string);
				malloc_now = false;
			}
			else
			{
				entire_string = realloc(entire_string, (strlen(number_prefix_optimized) + strlen(end_number) + strlen(number_suffix_optimized)) * (current_number - start_number_int + 1) + 1);
			}
			sprintf(current_number_char, "%d", current_number);
			strcpy(string_combined, number_prefix_optimized);
			strcat(string_combined, current_number_char);
			strcat(string_combined, number_suffix_optimized);
			strcat(entire_string, string_combined);
			current_number++;
			if (max_memory_int < (strlen(number_prefix_optimized) + strlen(end_number) + strlen(number_suffix_optimized)) * (current_number - start_number_int + 1) + 1 + base_length && max_memory_int >= base_length * 2 + 1)
			{
				fputs(entire_string, output_file);
				written = true;
				malloc_now = true;
			}
		}
		if (!written)
		{
			fputs(entire_string, output_file);
		}
		free(entire_string);
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