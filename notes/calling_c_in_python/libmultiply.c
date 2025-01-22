#include <stdint.h>

void multiply(float *source, float *destination, unsigned int length, int multiplier) {
    for (unsigned int i = 0; i < length; i++) {
        destination[i] = source[i] * multiplier;
    }
}
