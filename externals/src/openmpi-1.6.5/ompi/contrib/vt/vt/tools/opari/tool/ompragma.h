/****************************************************************************
**  SCALASCA    http://www.scalasca.org/                                   **
**  KOJAK       http://www.fz-juelich.de/jsc/kojak/                        **
*****************************************************************************
**  Copyright (c) 1998-2008                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYING in the package base directory for details         **
****************************************************************************/

#ifndef OMPRAGMA_H
#define OMPRAGMA_H

#include <string>
  using std::string;
#include <vector>
  using std::vector;

class OMPragma {
public:
  OMPragma(const string& f, int l, int pl, int pp, bool a)
	  : filename(f), lineno(l), pline(pl), ppos(pp), asd(a) {}
  void find_name();
  bool is_nowait();
  bool has_copypriv();
  string find_sub_name();
  virtual void add_nowait() = 0;
  virtual void add_descr(int n) = 0;
  virtual OMPragma* split_combined() = 0;
  virtual ~OMPragma() {}

  string filename;
  int    lineno;
  unsigned          pline;               // current parsing line
  string::size_type ppos;                // current parsing position
  bool asd;
  string name;
  vector<string> lines;

private:
  virtual string find_next_word() = 0;
  virtual bool find_word(const char* word, unsigned& line,
		         string::size_type& pos) = 0;
};

class OMPragmaF : public OMPragma {
public:
  OMPragmaF(const string& f, int l, int p, const string& line, int pomp, bool a)
	  : OMPragma(f, l, 0, p, a), slen(5+pomp) {
    lines.push_back(line);
    sentinel = pomp ? "$pomp" : "$omp";
  }
  virtual void add_nowait();
  virtual void add_descr(int n);
  virtual OMPragma* split_combined();

private:
  virtual string find_next_word();
  virtual bool find_word(const char* word, unsigned& line,
		         string::size_type& pos);
  void remove_empties();

  string sentinel;
  int slen;
};

class OMPragmaC : public OMPragma {
public:
  OMPragmaC(const string& f, int l, int pl, int pp, vector<string>& stmts,
            bool a) : OMPragma(f, l, pl, pp, a) {
    lines.swap(stmts);
  }
  virtual void add_nowait();
  virtual void add_descr(int n);
  virtual OMPragma* split_combined();

private:
  virtual string find_next_word();
  virtual bool find_word(const char* word, unsigned& line,
		         string::size_type& pos);
  void remove_empties();
};

#endif
