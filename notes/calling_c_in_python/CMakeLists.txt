cmake_minimum_required (VERSION 3.24.1)
project(libmultiply)

set(SOURCES
    libmultiply.c
)

add_library(multiply SHARED ${SOURCES})
