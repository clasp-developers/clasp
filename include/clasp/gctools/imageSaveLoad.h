/*
    File: imageSaveLoad.h
*/


#ifndef imageSaveLoad_H //[
#define imageSaveLoad_H

#include <clasp/core/common.h>
#include <clasp/llvmo/llvmoExpose.h>



namespace imageSaveLoad {

void image_save(const std::string& filename);
int image_load(const std::string& filename);


void clearLibraries();
void* encodeLibrarySaveAddress(void* address);
void* decodeLibrarySaveAddress(void* savedAddress);

void* encodeEntryPointSaveAddress(void* address, llvmo::CodeBase_sp code);
void* decodeEntryPointSaveAddress(void* savedAddress, llvmo::CodeBase_sp code);

};


#endif // imageSaveLoad_H
