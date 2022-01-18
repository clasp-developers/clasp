---
name: Bug report
about: Create a report to help us improve
title: ''
labels: bug
assignees: ''

---

**Describe the bug**
A clear and concise description of what the bug is.

**Expected behavior**
A clear and concise description of what you expected to happen.

**Actual behavior**
A clear and concise description of what happened instead. This should include error messages and backtraces if there are any. If that would be more than a few dozen lines, please attach it in a file instead.

**Code at issue**

```lisp
If applicable, example code that can be evaluated or compiled to reproduce the problem.
Ideally, this is short and self contained.
If it is not practicable to eliminate dependencies, please at least try to reduce them.
```

If the code is more than a few dozen lines, please attach a source file instead.

**Other steps to reproduce**
If the problem cannot be reproduced from just a code sample, include other steps here.
1. Step one
2. Step two
3. Etc.

**Context**
* Versions or commit hashes of Clasp on which you observed the problem (if you have built from up to date source, saying so is fine, you don't need to dig around for hashes)
* If relevant, your operating system and other aspects of your computing environment
* If you believe the compiler may be at fault, relevant declarations, including optimize settings (use `(clasp-cltl2:declaration-information 'optimize)`)
* Any other context about the problem
