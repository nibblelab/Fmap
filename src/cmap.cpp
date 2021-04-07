// #############################################################################
// This file contains the functions needed by fmap.f90 to call
// the std::map in C++
//
// Compilation command:
// g++ -c -g -Wall -Wextra -Werror cmap.cpp
//
// #############################################################################
#include <unordered_map>
#include <string>
#include <cstring>

using namespace std;

#ifdef __cplusplus
extern "C" {
#endif

// Type to map the Fortran array
// Must match the `type, bind(c):: c_vec` in fmap.f90
typedef struct vec {
    double* data;
    int size;
} vec;

typedef struct c_string {
    char* str;
    int size;
} c_string;

// Initialize pointer by calling map contructor
void mapInit(unordered_map<string,vec>*& mapping){
    mapping = new unordered_map<string,vec>();
}

// Insert a value in the map using a key
void mapAdd(unordered_map<string,vec>* mapping, char* fkey, int n, double* data){
    // Convert char* to string
    string key(fkey);
    // Fill the vec type
    vec value;
    value.data = data;
    value.size = n;
    // Map it
    mapping->insert( {key,value} );
    return;
}

// Get a value from map using a key
vec mapGet(unordered_map<string,vec>* mapping, const char* fkey){
    // Convert char* to string
    string key(fkey);
    return((*mapping)[key]);
}

// Get a key from map using a fake index
c_string mapGetVarNameByIndex(unordered_map<string,vec>* mapping, int i) {
    int indx = 0;
    string key;
    for ( auto it = mapping->begin(); it != mapping->end(); ++it ) {
        if(indx == i) {
            key = it->first;
            break;
        }
        indx++;
    }
    c_string value;
    value.size = key.size() + 1;
    value.str = new char[value.size];
    strncpy(value.str, key.c_str(), value.size);
    return value;
}

// Get a value from map using a fake index
vec mapGetByIndex(unordered_map<string,vec>* mapping, int i) {
    int indx = 0;
    string key;
    vec r;
    for ( auto it = mapping->begin(); it != mapping->end(); ++it ) {
        if(indx == i) {
            key = it->first;
            r = it->second;
            break;
        }
        indx++;
    }
    return r;
}

// Check id map is empty
bool mapIsEmpty(unordered_map<string,vec>* mapping){
    return(mapping->empty());
}

// Clear mapping
void mapClear(unordered_map<string,vec>* mapping){
    mapping->clear();
    return;
}

// Free the mapping pointer
void mapDestroy(unordered_map<string,vec>* mapping){
    delete mapping;
    return;
}

#ifdef __cplusplus
}
#endif