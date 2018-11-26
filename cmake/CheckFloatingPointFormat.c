#include <math.h>
#include <stdio.h>
#include <stdint.h>

int main(int argc, char* argv[]) {
	const double numbers[8] = {
		/* the IEEE-754 representation of the number below consists of the
		 * first eight letters of the uppercase alphabet. Their order will
		 * reveal the endianness we are dealing with. */
		2.39373654120722785592079162598E6, 0, 0, 0,
		/* these are just dummies */
		1234567, 2345678, 3456789, 4567890 };
	double result;
	const double *ptr = numbers, *end = numbers + 8;
	int i;

	result = 0.0;
	while (ptr < end) {
		result += *ptr;
		ptr++;
	}

	return (result == 12345.0);
}
