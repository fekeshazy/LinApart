#include "LinApart.h"
#include <string.h>

#define BUF_SIZE 10485760/500


// Helper functions for the C code
/*
   The problem with the buf function is that it doesn't check whether there's enough
   space in the str buffer to hold the formatted data. Instead, it simply checks
   whether there's less than 50 bytes of space left in the buffer, and if so, it writes
   the current contents of the buffer to disk and resets the length of the buffer to
   zero. This means that if the formatted data is longer than the remaining space in the
   buffer, the function will write outside the bounds of the buffer, causing a buffer overflow.
*/
size_t buf(int disk, FILE *fptr, char* str, char const* fmt, ...) {
    size_t len = strlen(str);
    size_t remaining = BUF_SIZE - len;

    va_list args;
    va_start(args, fmt);
    size_t result = vsnprintf(str + len, remaining, fmt, args);
    va_end(args);

    if (disk || result >= remaining) {
        fputs(str, fptr);
        len = 0;
        result = snprintf(str, BUF_SIZE, fmt, args);
    }

    return result + len;
}

// Mathematical helper functions
long double a(int i)
{
	return 1./i/10;
}


// A simple factorial iterated function. There is no safeguard for negative numbers!
void fact(mpz_t *result, long long n) {

	if(n<0) {
		fprintf(stderr, "Error: negative number (%lld) in factorial. \n", n);
		return;
	}

	mpz_set_ui(*result, 1);
	mpz_fac_ui(*result, n);
}

char* convert_to_string(mpz_t value) {
	// Get the length of the result string
	size_t result_len;
	mp_exp_t exp;

	char *result_str = mpz_get_str(NULL, 10, value);
	if (result_str == NULL) {
		fprintf(stderr, "Error: mpz_get_str failed to allocate memory.\n");
		return NULL;
	}

	result_len = strlen(result_str);

	// Allocate a buffer for the result
	char *buffer = malloc(result_len + 1);

	if (buffer == NULL) {
		fprintf(stderr, "Error: malloc failed to allocate memory for buffer.\n");
		return NULL;
	}

	// Copy the result string to the buffer
	strcpy(buffer, result_str);

	return buffer;
}

void binomial(mpz_t *result, int n, int m){
	mpz_t fact1, fact2, fact3;
	mpz_inits(fact1, fact2, fact3, NULL);

	fact(&fact1,n);
	fact(&fact2,m);
	fact(&fact3,n-m);

	mpz_set_ui(*result, 1);
	mpz_div(*result, fact1, fact2);
	mpz_div(*result, *result, fact3);
	mpz_clears(fact1, fact2, fact3, NULL);
}

// Helper functions for expression construction

// This function basically implements Lemma 2 from the article. It is equivalent to the
// Mathematica function with the same name in the Test_of_Theorems.m file.
void ReplaceRemainedStructure(int n, int m, int i, char **a, FILE *fptr, char *prod){

	// Opening of Plus[sum1, sum2]

	// Definition of temporary variables used in the sums.
	int tmpMod;
	int tmpBinomial;

	buf(0,fptr,prod,"(");
	// First sum
	for(int j=-m-n; j<=-m; j++){

		int xFactor = (j>=1) ? 1:0;
		tmpMod=pow(-1,(j-(-m-n))%2);
		mpz_t tmpBinomial;
		mpz_init(tmpBinomial);
		binomial(&tmpBinomial,n,j-(-m-n));
		char *tmpBinomial_str=convert_to_string(tmpBinomial);

		buf(0, fptr, prod,
				"(%d)*(%d)*%s*(-(%s))^(%d)*(x-(%s))^(%d)+",
				xFactor, tmpMod, tmpBinomial_str, a[i], j-(-m-n), a[i], -j);

		free(tmpBinomial_str);

	}

	// Second sum
	for(int j=0; j<=n+m; j++){

		tmpMod=pow(-1,j+((n+m)%2));
		mpz_t tmpBinomial;
		mpz_init(tmpBinomial);
		binomial(&tmpBinomial,n-1-j,-m-1);
		char *tmpBinomial_str=convert_to_string(tmpBinomial);

		if((n+m-j) == 0 ){
			buf(0, fptr, prod,
					"%d*%s*x^%d+",
					tmpMod, tmpBinomial_str, j);
		}else{
			buf(0, fptr, prod,
					"%d*%s*(-(%s))^%d*x^%d+",
					tmpMod, tmpBinomial_str, a[i], (n+m-j), j);
		}

		free(tmpBinomial_str);

	}
	buf(0,fptr,prod,"0)*");

}

// This is the main funciton used for generating the individual terms.
// It takes the following arguments: ...
void genTerms(int i, int *partition, int *m, int n, int mi, char **a, int remainedNumPower, FILE *fptr, char *prod){
	if (m[0]-partition[0] < 0){
		return;
	}

	// Definition of temporary terms.
	//int factNum=1;
	//int factDen=1;
	mpz_t factNum, factDen,fact1;
	mpz_inits(factNum, factDen, fact1, NULL);
	mpz_set_ui(factNum,1);
	mpz_set_ui(factDen,1);
	int one = 1;

	// We "calculate" the product from the final formula of the article.
	// The test of that formula with Mathematica and C indexing can be found in the
	// test_of_Theorem.m file.
	for (int j = 2;  j < n; j++) {

		// Condition to avoid singularities arising from the case 1/(a[i]-a[i])^m.
		if(partition[i]!=0) {
			return;

		}
		else if (i==j) continue;

		// Calculating the constans from the fromula
		one *= pow(-1,partition[j]);

		//factNum *= fact(m[j]+(partition[j]-1));
		fact(&fact1,m[j]+(partition[j]-1));
		mpz_mul(factNum,factNum,fact1);
		//factDen *= fact(m[j]-1)*fact(partition[j]);
		fact(&fact1,m[j]-1);
		mpz_mul(factDen,factDen,fact1);
		fact(&fact1,partition[j]);
		mpz_mul(factDen,factDen,fact1);

		// Creating the denominator.
		// The i-1 and j-1 indexing is due to the fact, that we wanted to start from a[1]
		// and not from a[2].
		buf(0,fptr,prod,"((%s)-(%s))^(-%d)*", a[i-1], a[j-1],partition[j]+m[j]);
	}

	mpz_t factCoeff;
	mpz_init(factCoeff);
	mpz_div(factCoeff,factNum,factDen);

	char *factCoeff_str = convert_to_string(factCoeff);

	buf(0,fptr,prod,"(%d)*%s*",one,factCoeff_str);
	//mpz_clears(factNum,factDen,factCoeff,NULL);


	//if(m[0]>=sumOfMultiplicities){
	if(remainedNumPower){

		fact(&factNum,m[0]);
		fact(&factDen,(m[0])-partition[0]);
		fact(&fact1,partition[0]);
		mpz_mul(factDen,factDen,fact1);
		mpz_div(factCoeff,factNum,factDen);
		factCoeff_str = convert_to_string(factCoeff);

		buf(0,fptr,prod,"%s*",factCoeff_str);

		if(((m[0])-partition[0]) == 0 ){
		}else{
			buf(0, fptr, prod,
					"(%s)^%d*",
					a[i-1],((m[0])-partition[0]));
		}

		ReplaceRemainedStructure(remainedNumPower,
				-(partition[1]+m[1]),i-1, a, fptr,prod);

		free(factCoeff_str);

	}
	else{

		fact(&factNum,m[0]);
		fact(&factDen,m[0]-partition[0]);
		fact(&fact1,partition[0]);
		mpz_mul(factDen,factDen,fact1);
		mpz_div(factCoeff,factNum,factDen);
		factCoeff_str = convert_to_string(factCoeff);

		buf(0,fptr,prod,"%s*",factCoeff_str);

		if((m[0]-partition[0]) == 0 ){
		}else{
			buf(0, fptr, prod, "(%s)^%d*",
					a[i-1],(m[0]-partition[0]));
		}

		buf(0,fptr,prod,"(x-(%s))^(-%d)*", a[i-1], partition[1]+m[1]);

		free(factCoeff_str);
	}

	buf(0,fptr,prod,"1+");

}


void generatePartitions(int ii, int nOrig, int n, int mi, char **a, int *partition, int *m, int index, int remainedNumPower, FILE *fptr, char *prod) {
	if (mi == 0) {
		if (n == 0) {
			genTerms(ii,partition, m, index, nOrig, a, remainedNumPower, fptr,prod);
		}
		return;
	}

	for (int i = 0; i <= n; i++) {
		partition[index] = i;
		if(m[0]<partition[0]) continue;
		generatePartitions(ii,nOrig, n - i, mi - 1, a, partition, m, index + 1, remainedNumPower, fptr, prod);
	}
}

void run(FILE *fptr, int N, char **a, int *m){
	char *prod = (char*)calloc(BUF_SIZE, sizeof(char));
	if (prod == NULL) {
		fprintf(stderr, "Error: calloc failed to allocate memory for prod.\n");
		return;
	}
	int sumOfMultiplicities=0;

	for(int j=2; j < N+2 ; j++)
		sumOfMultiplicities+=m[j];

	int remainedNumPower = 0;
	if(sumOfMultiplicities<=m[0]){
		int tmp = sumOfMultiplicities - 1;
		remainedNumPower = m[0] - tmp;
		m[0] = tmp;
	}

	for (int i = 2; i < N+2; i++) {
		int *partition = (int*)calloc(N+2, sizeof(int));
		if (partition == NULL) {
			fprintf(stderr, "Error: calloc failed to allocate memory for partition.\n");
			return;
		}

		generatePartitions(i, m[i]-1, m[i]-1, N+2, a, partition, m, 0, remainedNumPower, fptr, prod);
		free(partition);
	}

	buf(0, fptr, prod, "0");
	buf(1, fptr, prod, "");
	free(prod);
	fclose(fptr);
}
