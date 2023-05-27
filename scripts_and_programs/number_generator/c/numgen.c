//build command (gcc 12.1.0):
//gcc numgen.c -O2 -o numgen.exe

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>

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

	printf("Padding Char (optional, one character only): ");
	char padding_char = getchar();
	bool padding;
	if (padding_char == '\n') {
		padding = false;
	} else {
		padding = true;
	}

	//remove newlines  at end
	if (number_prefix[strlen(number_prefix) - 1] == '\n') {
		number_prefix[strlen(number_prefix) - 1] = '\0';
	}

	if (number_suffix[strlen(number_suffix) - 1] == '\n') {
		number_suffix[strlen(number_suffix) - 1] = '\0';
	}

	//get lengths after removing newlines
	int start_number_len = strlen(start_number);
	int end_number_len = strlen(end_number);
	int number_prefix_len = strlen(number_prefix);
	int number_suffix_len = strlen(number_suffix);
	
	int current_number = start_number_int;
	char current_number_char[strlen(end_number)];
	char* string_combined;
	string_combined = malloc(number_prefix_len + end_number_len + number_suffix_len + 1);
    int memory_size = number_prefix_len + end_number_len + number_suffix_len - 1;

	//fill string with padding char
	int i = 0;
	if (padding) {
		while (i < memory_size) {
			string_combined[i] = padding_char;
			i++;
		}
	}

	//benchmarking code
	float start_time = (float)clock()/CLOCKS_PER_SEC;

	FILE *output_file;

	output_file = fopen("output.txt", "w+");

	while (current_number <= end_number_int)
	{
		sprintf(current_number_char, "%d", current_number);
		int current_number_char_len = strlen(current_number_char);

		memcpy(string_combined, number_prefix, number_prefix_len);

		if (padding) {
			memcpy(string_combined + number_prefix_len + end_number_len - current_number_char_len - 1, current_number_char, current_number_char_len);
			memcpy(string_combined + number_prefix_len + end_number_len - 1, number_suffix, number_suffix_len);
			fwrite(string_combined, memory_size, 1, output_file);
		} else {
			memcpy(string_combined + number_prefix_len, current_number_char, current_number_char_len);
			memcpy(string_combined + number_prefix_len + current_number_char_len, number_suffix, number_suffix_len);
			fwrite(string_combined, number_prefix_len + current_number_char_len + number_suffix_len, 1, output_file);
		}

		fputc('\n', output_file);

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