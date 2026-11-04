#include <R.h>

/* The example of "Writing R Extensions", section 5.2. */
void convolve(double *a, int *na, double *b, int *nb, double *ab)
{
    int nab = *na + *nb - 1;

    for (int i = 0; i < nab; i++)
        ab[i] = 0.0;
    for (int i = 0; i < *na; i++)
        for (int j = 0; j < *nb; j++)
            ab[i + j] += a[i] * b[j];
}

/* A logical vector goes to an int*. */
void all_positive(double *x, int *n, int *ok)
{
    *ok = 1;
    for (int i = 0; i < *n; i++)
        if (x[i] <= 0) *ok = 0;
}

void upper_first(char **s, int *n)
{
    for (int i = 0; i < *n; i++)
        if (s[i][0] >= 'a' && s[i][0] <= 'z') s[i][0] += 'A' - 'a';
}

void xor_raw(unsigned char *x, int *n, unsigned char *key)
{
    for (int i = 0; i < *n; i++)
        x[i] ^= *key;
}
