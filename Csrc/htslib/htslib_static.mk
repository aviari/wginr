HTSLIB_static_LDFLAGS = -L../libdeflate -L../zlib -Wl,-L../libdeflate -L../zlib
HTSLIB_static_LIBS = -lpthread -lz -lm -ldeflate
