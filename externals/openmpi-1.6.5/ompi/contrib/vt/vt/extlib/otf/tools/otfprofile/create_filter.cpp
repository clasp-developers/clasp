/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <functional>
#include <math.h>
#include <string.h>
#include <time.h>

#include "otfprofile.h"
#include "create_filter.h"
#include "OTF_inttypes.h"
#include "OTF_Definitions.h"
#include "OTF_Platform.h"

CTree<string> *cur_pos;
CTree<string> callpath_tree;

map<uint32_t, CTree<string>*> filtered;
uint64_t maxCount, filterCount;
uint32_t idCount;
map<Pair_int, int, ltPair_int> edgesEdg;
map<Pair_int, uint64_t, ltPair_int> edgesEdg_sec;
map<string, CTree<string>*> CallpathNodes;

#define STARTTEXT "# VampirTrace dispersion callpath filter specification"
#define STARTTEXT2 "# generated with otfprofile"
#define STARTTEXT3 "# previous filter content"

bool CreateFilter(AllData& alldata) {
	maxCount = idCount = filterCount = 0;
	callpath_tree.parent = NULL;
	callpath_tree.id = idCount++;
	callpath_tree.item = " ";
	callpath_tree.str_hash = 0;
	callpath_tree.n = 0;
	callpath_tree.rule = FILTERNOT;

	bool error = false;
	cur_pos = NULL;

	// Use this function only if marker is set and dispersion data
	// is collected
	if ((alldata.params.dispersion.options & DISPERSION_OPT_FILTER) == 0 )
		return error;

	/*create string streams to save callpath tree information and
     filter information*/
	std::ostringstream filter_os;
	addOldToTree(alldata, filter_os);
	// setup file writer
	string filter_file_name = alldata.params.output_file_prefix + ".filter";
	fstream filter_file;
	filter_file.open(filter_file_name.c_str(), ios::out | ios::trunc);
	if (!filter_file.good()) {
		cerr << "ERROR: Unable to open file '" << filter_file_name
				<< "' for writing." << endl;
		return error;
	}

	time_t t = time(0);
	char* ts = ctime(&t);
	ts[strlen(ts)-1] = '\0';

	filter_file << STARTTEXT << endl << STARTTEXT2 << " on "<< ts << endl;

	map<TripleCallpath, FunctionDispersionData, gtTripleCallpathSortByCallpath>
	   ::const_iterator itc = alldata.functionDispersionCallpathMap.begin();
	map<TripleCallpath, FunctionDispersionData, gtTripleCallpathSortByCallpath>
        ::const_iterator itcend = alldata.functionDispersionCallpathMap.end();

	list<string> callpathes;
	string tmp;
	string word;

	while (itc != itcend) {

		if (itc->first.b == "") {
			itc++;
			continue;
		}

		tmp = "";
		maxCount += itc->second.count;
		parsePath(alldata, itc->first.b, itc->second.filterRule,
				itc->second.count, (itc->second.excl_time_95_percent
						/ alldata.timerResolution));

		itc++;
	}

	filter_file << endl << endl;
	postOrder(&callpath_tree, filter_file);

	filter_file << endl << endl << STARTTEXT3 << endl << filter_os.str()
			<< endl;
	filter_file.close();

	while (!callpath_tree.children.empty()) {
		CTree<string>* tmp = callpath_tree.children.back();
		callpath_tree.children.pop_back();
		if (tmp != NULL) {
			delete (tmp);
			tmp = NULL;
		}
	}

	return !error;
}

CTree<string>* addToTree(string parent, uint32_t pid, string child,
		uint32_t cid, int rule, uint64_t n, double timeFilt) {
	if (cur_pos == NULL) {
		for (uint32_t i = 0; i < callpath_tree.children.size(); i++)
			if (callpath_tree.children.at(i)->item == parent) {
				cur_pos = callpath_tree.children.at(i);
				break;
			}
	}

	if (cur_pos == NULL) {

		CTree<string> *children = new CTree<string> ();
		children = children;
		children->item = parent;
		children->str_hash = pid;

		children->parent = &callpath_tree;
		if (child == "") {
			children->n = n;
			children->rule = rule;
		} else {
			children->n = 1;
			children->rule = FILTERNOT;
		}

		children->id = idCount++;
		callpath_tree.children.push_back(children);
		cur_pos = children;
	}

	CTree<string> *tmp;
	tmp = cur_pos;
	while (parent != cur_pos->item) {
		if (cur_pos->parent == NULL)
			break;
		cur_pos = cur_pos->parent;
	}

	bool exists = false;
	for (uint32_t i = 0; i < cur_pos->children.size(); i++) {
		if ((cur_pos->children.at(i))->item == child) {
			exists = true;
			cur_pos = (cur_pos->children.at(i));
			cur_pos->n += n;
			if (timeFilt > 0)
				cur_pos->timeFilt = timeFilt;
			if (rule != -1 && cur_pos->rule != PREVFILTER) {
				if (cur_pos->rule == -1)
					cur_pos->rule = rule;
				else if (cur_pos->rule != FILTERNOT)
					cur_pos->rule = rule;

				tmp = cur_pos;
				if (rule == FILTERNOT)
					while (tmp->parent != NULL && tmp->parent->rule
							== FILTERREC) {
						tmp->parent->rule = FILTEROUT;
						tmp = tmp->parent;
					}
			}
			return NULL;
		}
	}
	if (child == "")
		exists = true;

	if (!exists) {

		CTree<string> *children = new CTree<string> ();
		children->item = child;
		children->rule = rule;
		children->n = n;
		children->id = idCount++;
		children->str_hash = cid;
		children->parent = cur_pos;
		if (timeFilt > 0)
			children->timeFilt = timeFilt;
		cur_pos->children.push_back(children);
		cur_pos = children;

		tmp = cur_pos;
		if (rule == FILTERNOT)
			while (tmp->parent != NULL && tmp->parent->rule == FILTERREC) {
				tmp->parent->rule = FILTEROUT;
				tmp = tmp->parent;
			}

		return cur_pos;
	}
	return NULL;
}

void trimString(string& str) {
	string::size_type pos1 = str.find_first_not_of(' ');
	string::size_type pos2 = str.find_last_not_of(' ');
	str = str.substr(pos1 == string::npos ? 0 : pos1,
			pos2 == string::npos ? str.length() - 1 : pos2 - pos1 + 1);
}

void addOldToTree(AllData& alldata, std::ostringstream& old_filter) {
	vector<string> pathes;

	if( !alldata.params.dispersion.filter_file_name.empty())
	{
        string filter_file_name = alldata.params.dispersion.filter_file_name;
        ifstream b_file(filter_file_name.c_str());
        if (b_file.good()) {
            string line;
            while (std::getline(b_file, line)) {
                trimString(line);
                if (line.substr(0, 1) != "#" && line.find_first_of("--")
                        != line.npos) {
                    pathes.push_back(line);
                } else {
                    if (line != STARTTEXT && line != STARTTEXT2 && line
                            != STARTTEXT3)
                        old_filter << line << endl;
                }
            }
            b_file.close();
        }

        while (!pathes.empty()) {
            string path = pathes.back();
            string tmp_path = path;
            pathes.pop_back();
            int pos = path.find_last_of("--") - 1;
            string tail = path.substr(pos, path.length() - pos);
            path = path.substr(0, pos);
            trimString(path);
            trimString(tail);
            string func = path.substr(path.find_last_of(";") + 1, path.length()
                    - (path.find_last_of(";") + 1));
            if (tail.find("C") != tail.npos && tail.find(" 0 ") != tail.npos) {
                parsePath(alldata, path, FILTEROUT);
            } else {
                old_filter << tmp_path << endl;
            }
        }
	}
}

void postOrder(CTree<string>* node, fstream& filter_file) {


	if(node->rule != FILTERREC)
	{
        for (uint32_t i = 0; i < node->children.size(); i++)
            postOrder(node->children.at(i), filter_file);
	}
	CTree<string>* tmp;
	if (node->rule != FILTERNOT && node->rule != -1) {
		string path = "";
		path = node->item + " -- 0 C";
		tmp = node->parent;
		while (tmp->parent != NULL) {
			if (tmp->rule == FILTERNOT || tmp->rule == -1)
				path = tmp->item + ";" + path;
			tmp = tmp->parent;
		}
		filter_file << path << endl;
		filterCount++;

	}
}

void parsePath(AllData& alldata, string path, int rule, uint64_t n,
		double timeB) {
	trimString(path);
	if (rule != 1) {
		timeB = -1;
	}

	cur_pos = NULL;
	uint32_t cid;
	string child;
	while (path != "") {
		uint32_t pid = atoi(path.substr(0, path.find_first_of(" ")).c_str());
		string parent = alldata.functionIdNameMap[pid];
		path = path.substr(path.find_first_of(" ") + 1);
		if (path == "") {
			if (cur_pos != NULL)
				break;
			cid = 5;
			child = parent;
			child = "";
		} else {
			cid = atoi(path.substr(0, path.find_first_of(" ")).c_str());
			child = alldata.functionIdNameMap[cid];
		}

		if (path.find_first_of(" ") == path.npos) {
			addToTree(parent, pid, child, cid, rule, n, timeB);
			break;
		} else
			addToTree(parent, pid, child, cid, -1, 0, -1);
	}
}

void parsePath(AllData& alldata, string path, int rule) {
	cur_pos = NULL;
	uint32_t cid;
	string child;
	while (path != "") {
		uint32_t pid = 5;
		string parent = path.substr(0, path.find_first_of(";"));
		path = path.substr(path.find_first_of(";") + 1);
		if (path == "") {
			if (cur_pos != NULL)
				break;
			cid = 5;
			child = parent;
			child = "";
		} else {
			cid = 5;
			child = path.substr(0, path.find_first_of(";"));
		}
		if (path.find_first_of(";") == path.npos) {
			addToTree(parent, pid, child, cid, rule, 0, -1);
			break;
		} else
			addToTree(parent, pid, child, cid, -1, 0, -1);
	}
}
