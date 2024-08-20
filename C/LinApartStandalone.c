#include <stdio.h>
#include "LinApart.h"


static void usage(void)
{
    fputs("Usage: LinApart <exponents> <roots> [-q] [-o OutputFileName]\n", stderr);
    fputs("  <exponents>: Comma-separated list of exponents (e.g., \"1,2,3,4\")\n", stderr);
    fputs("  <roots>: Comma-separated list of roots (e.g., \"a,b,c,d\")\n", stderr);
    fputs("  exponents should contain 1 element more than roots (the exponent of the monomial in the numerator)\n", stderr);
    fputs("  -q suppresses the standard output\n", stderr);
    fputs("  -o alters the file name of the output file\n", stderr);
    fputs("Example: LinApart \"1,2,3\" \"a1,a2\"\n", stderr);
}

int main(int argc, char *argv[]) {
	int quiet = 0;
	if (argc < 2) {
		usage();
		return 1;
	}
	char *fname = "LA_result.out";
	for (int i = 1; i < argc; i++){
		if (!strcmp(argv[i], "-v")) { /* prints version information */
			puts("LinApart-"VERSION);
			return 0;
		}
		else if (!strcmp(argv[i], "-q")) {
			//N = atoi(argv[++i]);
			quiet = 1;
		}
		else if (!strcmp(argv[i], "-o")) {
			fname = argv[++i];
		}
		if (argc < 3) {
			usage();
			return 1;
		}
	}


	char *multiplicities_str = argv[1];
	char *constants_str = argv[2];

	 // Count the number of multiplicities
    int N = 2;
    for (int i = 0; multiplicities_str[i] != '\0'; i++) {
        if (multiplicities_str[i] == ',') {
            N++;
        }
    }

    // Preallocate memory for multiplicities array
    int m[N];

    // Parse multiplicities
    int index = 0;
    char *token = strtok(multiplicities_str, ",");
    while (token != NULL) {
	    if(index==1)
		    m[index++] = 1; //x-dependent part
        m[index++] = atoi(token);
        token = strtok(NULL, ",");
    }

	// Parse constants
	char *a[N+1];
	a[0] = NULL;
	token = strtok(constants_str, ",");
	int a_count = 1;
	while (token != NULL && a_count < N+1) {
		a[a_count] = token;
		a_count++;

		// Get the next token
		token = strtok(NULL, ",");
	}

	if( N-a_count!= 1){
		usage();
		return 1;
	}

	// Adjust N to match the original code
	N = N - 2;

	// Open the output file
	remove(fname);
	FILE *fptr = fopen(fname, "a");

	//Run
	run(fptr, N, a, m);

	if(quiet)
		return 0;

	fptr = fopen(fname, "rb");

	long length;
	char* buffer = 0;
	if (fptr) {
		fseek(fptr, 0, SEEK_END);
		length = ftell(fptr);
		fseek(fptr, 0, SEEK_SET);
		buffer = (char*)calloc(length, sizeof(char));
		if (buffer == NULL) {
			fprintf(stderr, "Error: calloc failed to allocate memory for reading buffer.\n");
			return 1;
		}
		if (buffer) {
			fread(buffer, 1, length, fptr);
			buffer[length] = '\0';
		}
		fclose(fptr);
	}

	printf("%s\n", buffer);

	free(buffer);

	return 0;
}
