/*
    File: fs.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
#define DEBUG_LEVEL_FULL

#include <clasp/core/fileSystem.h>
#include <clasp/core/lisp.h>
#include <clasp/core/lispDefinitions.h>

namespace bf = boost_filesystem;

namespace core {

void Path_O::initialize() {
  this->Base::initialize();
}

/*
__BEGIN_DOC(classes.Path.!class.Path)
\optionalKeyed{path:}{Text::path}

Create a Path object that maintains a system independant path to a file in the file system.
__END_DOC
*/
void Path_O::lispInitialize(Cons_sp keyed, Lisp_sp env) {
  _G();
  string sp = keyed->getStringAndRemoveOrDefault("path", "");
  if (sp != "") {
    this->setPath(sp);
  }
}

#if defined(OLD_SERIALIZE)
void Path_O::serialize(ArchiveP node) {
  IMPLEMENT_ME();
}
#endif

void Path_O::setPath(const string &pth) {
  _G();
  bf::path p(pth);
  this->_Path = p;
}

string Path_O::string() {
  _G();
  return this->_Path.string();
}

string Path_O::stem() {
  _G();
  return this->_Path.stem();
}

string Path_O::extension() {
  _G();
  return this->_Path.extension();
}

bool Path_O::exists() {
  return boost_filesystem::exists(this->_Path);
}

Cons_sp directory(Path_sp rpath) {
  _G();
  bf::path p(rpath->getPath());
  Cons_sp list, tail;
  Str_sp fileName;
  list = Cons_O::create(this->env(), _Nil<T_O>(), _Nil<T_O>());
  tail = list;
  bf::directory_iterator end_iter;
  for (bf::directory_iterator itr(p); itr != end_iter; itr++) {
    tail->setCdr(Cons_O::create(this->env(), Str_O::create(itr->path().string()), _Nil<T_O>()));
    tail = tail->cdr();
  }
  return list->cdr();
}

void rename(Path_sp rpath1, Path_sp rpath2) {
  _G();
  return bf::rename(rpath1->getPath(), rpath2->getPath());
}

bool delete_file(Path_sp rpath) {
  _G();
  return bf::remove(rpath->getPath());
}

int removeAll(Path_sp rpath) {
  _G();
  return bf::remove_all(rpath->getPath());
}

bool createDirectory(Path_sp rpath) {
  _G();
  return bf::create_directory(rpath->getPath());
}

class Path_Exposer : public Exposer {
  void exposeCando() {
    class_<Path_O>()
        .def("setPath", &Path_O::setPath)
        .def("string", &Path_O::string)
        .def("stem", &Path_O::stem)
        .def("extension", &Path_O::extension)
        .def("exists", &Path_O::exists);
    af_def(CorePkg, "directory", &directory);
    af_def(CorePkg, "delete-file", &delete_file);
    af_def(CorePkg, "rename", &rename);
    af_def(CorePkg, "delete-file-all", &removeAll);
    af_def(CorePkg, "createDirectory", &createDirectory);
  }

  void exposePython() {
#ifdef USEBOOSTPYTHON //[
    PYTHON_CLASS(CorePkg, Path, "", "");
#endif //]
  }
};

REGISTER_EXPOSE_CLASS(core, core, Path_O, Path_Exposer);
};
