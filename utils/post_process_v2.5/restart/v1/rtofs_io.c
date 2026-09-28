#define _FILE_OFFSET_BITS 64
#include <stdio.h>
#include <stdint.h>

// Callable directly from Fortran via iso_c_binding
void read_hycom_record_c(const char* filename, int irec, int n2drec, float* array) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        fprintf(stderr, "FATAL: C I/O wrapper failed to open: %s\n", filename);
        return;
    }

    // Explicit 64-bit math for the byte offset to shatter the 2GB limit
    off_t offset = (off_t)(irec - 1) * (off_t)n2drec * 4;
    fseeko(file, offset, SEEK_SET);
    
    // Read the binary data
    fread(array, 4, n2drec, file);
    fclose(file);

    // Swap Big-Endian (RTOFS) to Little-Endian (Intel x86_64)
    uint32_t *raw = (uint32_t *)array;
    for (int i = 0; i < n2drec; ++i) {
        uint32_t x = raw[i];
        raw[i] = ((x >> 24) & 0x000000ff) |
                 ((x >>  8) & 0x0000ff00) |
                 ((x <<  8) & 0x00ff0000) |
                 ((x << 24) & 0xff000000);
    }
}
