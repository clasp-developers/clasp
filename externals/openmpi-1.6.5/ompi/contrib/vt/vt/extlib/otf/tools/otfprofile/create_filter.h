/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
 */

#ifndef CREATE_FILTER_H
#define CREATE_FILTER_H

#include "datastructs.h"
#include <vector>

/**
 * Enumeration to set filter method:
 *   - FILTERREC: recursive filter
 *   - FILTERNOT: don't filter this path
 *   - FILTEROUT: filter this path, but not recursively
 *   - TIMEFILTER: filter this path to a specific point of time
 *   - PREVFILTER: filter that was set by an other filterfile
 */
enum filterRule {
    FILTERREC = 0, FILTERNOT = 1, FILTEROUT = 2, TIMEFILTER = 3, PREVFILTER = 4
};

struct Pair_int {

    uint32_t a;
    uint32_t b;

    Pair_int() :
        a(0), b(0) {
    }
    Pair_int(long aa, long bb) :
        a(aa), b(bb) {
    }
    ~Pair_int() {
    }
};

struct ltPair_int {

    bool operator()(const Pair_int& p1, const Pair_int& p2) const {

        /* a is the major number for comparison, this gives a better
         order when reducing the entries over the first argument */

        if (p1.a == p2.a) {

            return p1.b < p2.b;

        } else {

            return p1.a < p2.a;
        }
    }
};

/**
 * CTree is a Tree to save Path information. It Points to the Parent node and
 *  contains child notes, as well as
 * filter rule, name, id and hash_str.
 */
template<class T> class CTree {
public:
    T item;
    int rule;
    uint64_t n;
    uint32_t id;
    size_t str_hash;

    CTree<T> *parent;
    std::vector<CTree<std::string>*> children;
    double timeFilt;
    CTree() {
        parent = NULL;
        timeFilt = 0;
        rule = 1;
        n = 0;
        id = 0;
        str_hash = 0;
    }
    ~CTree() {

        while (!children.empty()) {
            CTree<T>* tmp = children.back();
            children.pop_back();
            if (tmp != NULL) {
                delete (tmp);
                tmp = NULL;
            }
        }
    }
};

/* create Filtert */
bool CreateFilter(AllData& alldata);

/* add path element to path tree */
CTree<string>* addToTree(string parent, uint32_t pid, string child,
        uint32_t cid, int rule, uint64_t n, string timeFilt);

/* load filter information from an existing filter file (result.filter)*/
void addOldToTree(AllData& alldata, std::ostringstream& old_filter);

/* iteration step through the callpath tree to write filter file */
void postOrder(CTree<string>* node, fstream& filter_file);

/* parse callpath and add pairs of parent and childs to the tree  */
void parsePath(AllData& alldata, string path, int rule, uint64_t n,
        double timeB);

/* parse callpath and add pairs of parent and childs to the tree  */
void parsePath(AllData& alldata, string path, int rule);

#endif /* CREATE_FILTER_H */
