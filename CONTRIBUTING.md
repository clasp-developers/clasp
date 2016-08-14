# Contributing

Clasp is a fast-moving project, but we love extra help of all
kinds. There are a few guidelines that we want contributors to follow
on Github so that we can keep on top of things.

Join the #clasp IRC chat on [Freenode](https://freenode.net/) for
questions, suggestions, or generally to just hang out with us!

## Getting Started

* Make sure you have a [GitHub account](https://github.com/signup/free)
* [Fork the repository](https://github.com/drmeister/clasp/fork) on GitHub
* [Submit an issue](https://github.com/drmeister/clasp/issues/new),
  if one needs to be created.

## Tips for Making Changes

* Create a topic branch from where you want to base your work.
  * This is usually the `dev` branch.
  * **Do NOT send a PR to the `testing` or `preview` branches.** These are
    currently used for CI purposes.
  * To quickly create a topic branch based on dev; `git checkout -b
    fix/my_contribution dev`. Please avoid working directly on the
    `dev` branch.
* Make commits of logical units.
* Check for unnecessary whitespace with `git diff --check` before committing.
* Try to write [good commit messages](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html)

# Additional Resources

* [Issue tracker](https://github.com/drmeister/clasp/issues)
* [GitHub pull request documentation](https://help.github.com/send-pull-requests/)
* #clasp IRC channel on freenode.org ([Archive](https://irclog.tymoon.eu/freenode/clasp))
* [clasp mailing list](https://mailman.common-lisp.net/listinfo/clasp-devel)
