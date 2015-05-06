/*
    File: svm.cc
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

string ScorerVirtualMachine_O::sourceLine(uint lineNumber) {
  if (lineNumber >= this->_Program.size()) {
    return "";
  }
  ScoreCommand &cmd = this->_Program[lineNumber];
  string sourceLine = scoreCommandAsString(cmd, this, _lisp);
  string prog = "SETUP";
  stringstream ss;
  if (lineNumber >= this->_ScorerProgramCounterStart) {
    prog = "score";
  }
  ss << "(" << setw(3) << lineNumber << ") ";
  ss << prog << " " << sourceLine;
  if (lineNumber < this->_ProgramComments.size()) {
    if (this->_ProgramComments[lineNumber] != "") {
      ss << "; " << this->_ProgramComments[lineNumber];
    }
  }
  return ss.str();
};

string ScorerVirtualMachine_O::sourceCode() {
  _OF();
  stringstream code;
  code << "Program contains " << this->_Program.size() << " statements" << std::endl;
  for (uint i = 0; i < this->_Program.size(); i++) {
    string line = this->sourceLine(i);
    code << line << std::endl;
    LOG(BF("Source line: %s") % line);
  }
  LOG(BF("Source code: %s") % code.str());
  return code.str();
}

void ScorerVirtualMachine_O::eraseProgram() {
  this->_Program.clear();
  this->_RealConstants.clear();
  this->_PointProviders.clear();
  this->_RunSetup = true;
}

Cons_sp ScorerVirtualMachine_O::programAsCons() {
  Cons_sp first = _lisp->create<Cons_O>();
  Cons_sp cur = first;
  for (vector<ScoreCommand>::iterator it = this->_Program.begin();
       it != this->_Program.end(); it++) {
    ScoreOperationEnum sop = (ScoreOperationEnum)(it->_Operation);
    Cons_sp oneOp = Cons_O::createList(symbolForScoreOperation(sop, _lisp),
                                       Fixnum_O::create((int)(it->_ArrayId)),
                                       Fixnum_O::create((int)(it->_Index)));
    Cons_sp one = _lisp->create<Cons_O>(oneOp);
    cur->setCdr(one);
    cur = one;
  }
  return first->cdr();
}

Cons_sp ScorerVirtualMachine_O::programAsConsOfStrings() {
  Cons_sp first = _lisp->create<Cons_O>();
  Cons_sp cur = first;
  for (vector<ScoreCommand>::iterator it = this->_Program.begin();
       it != this->_Program.end(); it++) {
    Str_sp oneCmd = Str_O::create(scoreCommandAsString(*it, this, _lisp));
    Cons_sp oneOp = Cons_O::create(oneCmd);
    Cons_sp one = _lisp->create<Cons_O>(oneOp);
    cur->setCdr(one);
    cur = one;
  }
  return first->cdr();
}

void ScorerVirtualMachine_O::_reset() {
  this->_RealStack.clear();
  this->_AliasStack.clear();
  this->_RunFlag = true;
}

void ScorerVirtualMachine_O::synchronizeWithStaticProviders() {
  this->_DynamicPointProviders.clear();
  for (uint i = 0; i < this->_PointProviders.size(); i++) {
    PointProviderHolder &ph = this->_PointProviders[i];
    if (ph.provider().isA<O_Builder>()) {
      this->_DynamicPointProviders.push_back(i);
    } else {
      ph.synchronizeWithStaticProvider();
    }
  }
}

void ScorerVirtualMachine_O::synchronizeWithDynamicProviders() {
  for (uint i = 0; i < this->_DynamicPointProviders.size(); i++) {
    PointProviderHolder &ph = this->_PointProviders[this->_DynamicPointProviders[i]];
    ph.synchronizeWithDynamicProviderIfNecessary();
  }
}

void ScorerVirtualMachine_O::prepareToRun(ScorerState_sp ss) {
  this->_reset();
  this->_ScorerState = ss;
  if (this->_RunSetup) {
    this->_ProgramCounter = 0; /* Setup is at 0 */
    this->_RunSetup = false;
  } else {
    this->_ProgramCounter = this->_ScorerProgramCounterStart;
  }
}

/*! Return true if the step succeeded and false if it didnt */
bool ScorerVirtualMachine_O::step() {
  _G();
  if (this->_ProgramCounter >= this->_Program.size() || !this->_RunFlag) {
    LOG(BF("----SVM_Program reached end"));
    return false;
  }
  ScoreCommand &cmd = this->_Program[this->_ProgramCounter++];
  LOG(BF("SVM_Program: %s") % this->sourceLine(this->_ProgramCounter - 1));
  try {
    SVMEvalCallback evalCallback = _SVMEvalCallbackTable[cmd._Operation];
    (evalCallback)(cmd, this);
  } catch (HardError &err) {
    TOSS(_lisp->error(BF("Error while evaluating ScorerVirtualMachine: %s") % err.message()));
  } catch (Condition &err) {
    TOSS(err.conditionObject());
  } catch (...) {
    TOSS(_lisp->error(BF("Unknown error while evaluating ScorerVirtualMachine")));
  }
  return true;
}

void ScorerVirtualMachine_O::writeMachineStateToScorerState(SReal score) {
  _OF();
  ASSERTNOTNULL(this->_ScorerState);
  ASSERT(this->_ScorerState.notnilp());
  this->_ScorerState->clear();
  this->_ScorerState->setScore(score);
  for (uint i = 0; i != this->_PointProviders.size(); i++) {
    PointProviderHolder &pph = this->_PointProviders[i];
    Matrix m;
    if (pph.hasTransform()) {
      m = pph.transform();
    }
    this->_ScorerState->addTransform(pph.symbol(), m);
  }
}

void ScorerVirtualMachine_O::evaluate(ScorerState_sp scorerState) {
  this->run(scorerState);
}

void ScorerVirtualMachine_O::run(ScorerState_sp scorerState) {
  _OF();
  this->prepareToRun(scorerState);
  this->cont();
}

void ScorerVirtualMachine_O::cont() {
  _OF();
  while (this->step())
    ;
}

SReal ScorerVirtualMachine_O::lookupRealConstant(int index) {
  lisp_ASSERT(_lisp, (uint)index < this->_RealConstants.size());
  return this->_RealConstants[index];
}
