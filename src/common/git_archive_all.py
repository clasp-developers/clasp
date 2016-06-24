#! /usr/bin/env python
# coding=utf-8

# The MIT License (MIT)
#
# Copyright (c) 2010 Ilya Kulakov
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

from __future__ import print_function
from __future__ import unicode_literals

import logging
from os import extsep, path, readlink, curdir
from subprocess import CalledProcessError, Popen, PIPE
import sys
import tarfile
from zipfile import ZipFile, ZipInfo, ZIP_DEFLATED
import re

__version__ = "1.13"


class GitArchiver(object):
    """
    GitArchiver

    Scan a git repository and export all tracked files, and submodules.
    Checks for .gitattributes files in each directory and uses 'export-ignore'
    pattern entries for ignore files in the archive.

    >>> archiver = GitArchiver(main_repo_abspath='my/repo/path')
    >>> archiver.create('output.zip')
    """
    LOG = logging.getLogger('GitArchiver')

    def __init__(self, prefix='', exclude=True, force_sub=False, extra=None, main_repo_abspath=None):
        """
        @param prefix: Prefix used to prepend all paths in the resulting archive.
            Extra file paths are only prefixed if they are not relative.
            E.g. if prefix is 'foo' and extra is ['bar', '/baz'] the resulting archive will look like this:
            /
              baz
              foo/
                bar
        @type prefix: string

        @param exclude: Determines whether archiver should follow rules specified in .gitattributes files.
        @type exclude:  bool

        @param force_sub: Determines whether submodules are initialized and updated before archiving.
        @type force_sub: bool

        @param extra: List of extra paths to include in the resulting archive.
        @type extra: list

        @param main_repo_abspath: Absolute path to the main repository (or one of subdirectories).
            If given path is path to a subdirectory (but not a submodule directory!) it will be replaced
            with abspath to top-level directory of the repository.
            If None, current cwd is used.
        @type main_repo_abspath: string
        """
        if extra is None:
            extra = []

        if main_repo_abspath is None:
            main_repo_abspath = path.abspath('')
        elif not path.isabs(main_repo_abspath):
            raise ValueError("You MUST pass absolute path to the main git repository.")

        try:
            path.isdir(".git") or self.run_shell("git rev-parse --git-dir > /dev/null 2>&1", main_repo_abspath)
        except Exception as e:
            raise ValueError("{0} not a git repository (or any of the parent directories).".format(main_repo_abspath))

        main_repo_abspath = path.abspath(
            self.read_git_shell('git rev-parse --show-toplevel', main_repo_abspath)
            .rstrip()
        )

        self.prefix = prefix
        self.exclude = exclude
        self.extra = extra
        self.force_sub = force_sub
        self.main_repo_abspath = main_repo_abspath

    def create(self, output_path, dry_run=False, output_format=None):
        """
        Create the archive at output_file_path.

        Type of the archive is determined either by extension of output_file_path or by output_format.
        Supported formats are: gz, zip, bz2, xz, tar, tgz, txz

        @param output_path: Output file path.
        @type output_path: string

        @param dry_run: Determines whether create should do nothing but print what it would archive.
        @type dry_run: bool

        @param output_format: Determines format of the output archive. If None, format is determined from extension
            of output_file_path.
        @type output_format: string
        """
        if output_format is None:
            file_name, file_ext = path.splitext(output_path)
            output_format = file_ext[len(extsep):].lower()
            self.LOG.debug("Output format is not explicitly set, determined format is {0}.".format(output_format))

        if not dry_run:
            if output_format == 'zip':
                archive = ZipFile(path.abspath(output_path), 'w')

                def add_file(file_path, arcname):
                    if not path.islink(file_path):
                        archive.write(file_path, arcname, ZIP_DEFLATED)
                    else:
                        i = ZipInfo(arcname)
                        i.create_system = 3
                        i.external_attr = 0xA1ED0000
                        archive.writestr(i, readlink(file_path))
            elif output_format in ['tar', 'bz2', 'gz', 'xz', 'tgz', 'txz']:
                if output_format == 'tar':
                    t_mode = 'w'
                elif output_format == 'tgz':
                    t_mode = 'w:gz'
                elif output_format == 'txz':
                    t_mode = 'w:xz'
                else:
                    t_mode = 'w:{0}'.format(output_format)

                archive = tarfile.open(path.abspath(output_path), t_mode)

                def add_file(file_path, arcname):
                    archive.add(file_path, arcname)
            else:
                raise RuntimeError("Unknown format: {0}".format(output_format))

            def archiver(file_path, arcname):
                self.LOG.debug("Compressing {0} => {1}...".format(file_path, arcname))
                add_file(file_path, arcname)
        else:
            archive = None

            def archiver(file_path, arcname):
                self.LOG.info("{0} => {1}".format(file_path, arcname))

        self.archive_all_files(archiver)  # this will take care of submodule init and update

        if archive is not None:
            archive.close()

    def get_exclude_patterns(self, repo_abspath, repo_file_paths):
        """
        Returns exclude patterns for a given repo. It looks for .gitattributes files in repo_file_paths.

        Resulting dictionary will contain exclude patterns per path (relative to the repo_abspath).
        E.g. {('.', 'Catalyst', 'Editions', 'Base'), ['Foo*', '*Bar']}

        @type repo_abspath:     string
        @param repo_abspath:    Absolute path to the git repository.

        @type repo_file_paths:  list
        @param repo_file_paths: List of paths relative to the repo_abspath that are under git control.

        @rtype:         dict
        @return:    Dictionary representing exclude patterns.
                    Keys are tuples of strings. Values are lists of strings.
                    Returns None if self.exclude is not set.
        """
        if not self.exclude:
            return None

        def read_attributes(attributes_abspath):
            patterns = []
            if path.isfile(attributes_abspath):
                attributes = open(attributes_abspath, 'r').readlines()
                patterns = []
                for line in attributes:
                    tokens = line.strip().split()
                    if "export-ignore" in tokens[1:]:
                        patterns.append(tokens[0])
            return patterns

        exclude_patterns = {(): []}

        # There may be no gitattributes.
        try:
            global_attributes_abspath = self.read_shell("git config --get core.attributesfile", repo_abspath).rstrip()
            exclude_patterns[()] = read_attributes(global_attributes_abspath)
        except:
            # And it's valid to not have them.
            pass

        for attributes_abspath in [path.join(repo_abspath, f) for f in repo_file_paths if f.endswith(".gitattributes")]:
            # Each .gitattributes affects only files within its directory.
            key = tuple(self.get_path_components(repo_abspath, path.dirname(attributes_abspath)))
            exclude_patterns[key] = read_attributes(attributes_abspath)

        local_attributes_abspath = path.join(repo_abspath, ".git", "info", "attributes")
        key = tuple(self.get_path_components(repo_abspath, repo_abspath))

        if key in exclude_patterns:
            exclude_patterns[key].extend(read_attributes(local_attributes_abspath))
        else:
            exclude_patterns[key] = read_attributes(local_attributes_abspath)

        return exclude_patterns

    def is_file_excluded(self, repo_abspath, repo_file_path, exclude_patterns):
        """
        Checks whether file at a given path is excluded.

        @type repo_abspath: string
        @param repo_abspath: Absolute path to the git repository.

        @type repo_file_path:   string
        @param repo_file_path:  Path to a file within repo_abspath.

        @type exclude_patterns:     dict
        @param exclude_patterns:    Exclude patterns with format specified for get_exclude_patterns.

        @rtype: bool
        @return: True if file should be excluded. Otherwise False.
        """
        if exclude_patterns is None or not len(exclude_patterns):
            return False

        from fnmatch import fnmatch

        file_name = path.basename(repo_file_path)
        components = self.get_path_components(repo_abspath, path.join(repo_abspath, path.dirname(repo_file_path)))

        is_excluded = False
        # We should check all patterns specified in intermediate directories to the given file.
        # At the end we should also check for the global patterns (key '()' or empty tuple).
        while not is_excluded:
            key = tuple(components)
            if key in exclude_patterns:
                patterns = exclude_patterns[key]
                for p in patterns:
                    if fnmatch(file_name, p) or fnmatch(repo_file_path, p):
                        self.LOG.debug("Exclude pattern matched {0}: {1}".format(p, repo_file_path))
                        is_excluded = True

            if not len(components):
                break

            components.pop()

        return is_excluded

    def archive_all_files(self, archiver):
        """
        Archive all files using archiver.

        @param archiver: Callable that accepts 2 arguments:
            abspath to file on the system and relative path within archive.
        """
        for file_path in self.extra:
            archiver(path.abspath(file_path), path.join(self.prefix, file_path))

        for file_path in self.walk_git_files():
            archiver(path.join(self.main_repo_abspath, file_path), path.join(self.prefix, file_path))

    def walk_git_files(self, repo_path=''):
        """
        An iterator method that yields a file path relative to main_repo_abspath
        for each file that should be included in the archive.
        Skips those that match the exclusion patterns found in
        any discovered .gitattributes files along the way.

        Recurs into submodules as well.

        @type repo_path:    string
        @param repo_path:   Path to the git submodule repository relative to main_repo_abspath.

        @rtype:     iterator
        @return:    Iterator to traverse files under git control relative to main_repo_abspath.
        """
        repo_abspath = path.join(self.main_repo_abspath, repo_path)
        repo_file_paths = self.read_git_shell(
            "git ls-files --cached --full-name --no-empty-directory",
            repo_abspath
        ).splitlines()
        exclude_patterns = self.get_exclude_patterns(repo_abspath, repo_file_paths)

        for repo_file_path in repo_file_paths:
            # Git puts path in quotes if file path has unicode characters.
            repo_file_path = repo_file_path.strip('"')  # file path relative to current repo
            file_name = path.basename(repo_file_path)
            main_repo_file_path = path.join(repo_path, repo_file_path)  # file path relative to the main repo

            # Only list symlinks and files that don't start with git.
            if file_name.startswith(".git") or (
                not path.islink(main_repo_file_path) and path.isdir(main_repo_file_path)
            ):
                continue

            if self.is_file_excluded(repo_abspath, repo_file_path, exclude_patterns):
                continue

            yield main_repo_file_path

        if self.force_sub:
            self.run_shell("git submodule init", repo_abspath)
            self.run_shell("git submodule update", repo_abspath)

        gitmodulesfile = path.join(repo_path, ".gitmodules")
        if path.isfile(gitmodulesfile):
            with open(gitmodulesfile) as f:
                for line in f.readlines():
                    m = re.match("^\s*path\s*=\s*(.*)\s*$", line)
                    if m:
                        submodule_path = m.group(1)
                        submodule_path = path.join(repo_path, submodule_path)
                        for file_path in self.walk_git_files(submodule_path):
                            yield file_path

    @staticmethod
    def get_path_components(repo_abspath, abspath):
        """
        Split given abspath into components relative to repo_abspath.
        These components are primarily used as unique keys of files and folders within a repository.

        E.g. if repo_abspath is '/Documents/Hobby/ParaView/' and abspath is
        '/Documents/Hobby/ParaView/Catalyst/Editions/Base/', function will return:
        ['.', 'Catalyst', 'Editions', 'Base']

        First element is always '.' (concrete symbol depends on OS).

        @param repo_abspath: Absolute path to the git repository. Normalized via os.path.normpath.
        @type repo_abspath: string

        @param abspath: Absolute path to a file within repo_abspath. Normalized via os.path.normpath.
        @type abspath: string

        @return: List of path components.
        @rtype: list
        """
        repo_abspath = path.normpath(repo_abspath)
        abspath = path.normpath(abspath)

        if not path.isabs(repo_abspath):
            raise ValueError("repo_abspath MUST be absolute path.")

        if not path.isabs(abspath):
            raise ValueError("abspath MUST be absoulte path.")

        if not path.commonprefix([repo_abspath, abspath]):
            raise ValueError(
                "abspath (\"{0}\") MUST have common prefix with repo_abspath (\"{1}\")"
                .format(abspath, repo_abspath)
            )

        components = []

        while not abspath == repo_abspath:
            abspath, tail = path.split(abspath)

            if tail:
                components.insert(0, tail)

        components.insert(0, curdir)
        return components

    @staticmethod
    def run_shell(cmd, cwd=None):
        """
        Runs shell command.

        @type cmd:  string
        @param cmd: Command to be executed.

        @type cwd:  string
        @param cwd: Working directory.

        @rtype:     int
        @return:    Return code of the command.

        @raise CalledProcessError:  Raises exception if return code of the command is non-zero.
        """
        p = Popen(cmd, shell=True, cwd=cwd)
        p.wait()

        if p.returncode:
            raise CalledProcessError(returncode=p.returncode, cmd=cmd)

        return p.returncode

    @staticmethod
    def read_shell(cmd, cwd=None, encoding='utf-8'):
        """
        Runs shell command and reads output.

        @type cmd:  string
        @param cmd: Command to be executed.

        @type cwd:  string
        @param cwd: Working directory.

        @type encoding: string
        @param encoding: Encoding used to decode bytes returned by Popen into string.

        @rtype:     string
        @return:    Output of the command.

        @raise CalledProcessError:  Raises exception if return code of the command is non-zero.
        """
        p = Popen(cmd, shell=True, stdout=PIPE, cwd=cwd)
        output, _ = p.communicate()
        output = output.decode(encoding)

        if p.returncode:
            if sys.version_info > (2, 6):
                raise CalledProcessError(returncode=p.returncode, cmd=cmd, output=output)
            else:
                raise CalledProcessError(returncode=p.returncode, cmd=cmd)

        return output

    @staticmethod
    def read_git_shell(cmd, cwd=None):
        """
        Runs git shell command, reads output and decodes it into unicode string

        @type cmd:  string
        @param cmd: Command to be executed.

        @type cwd:  string
        @param cwd: Working directory.

        @rtype:     string
        @return:    Output of the command.

        @raise CalledProcessError:  Raises exception if return code of the command is non-zero.
        """
        p = Popen(cmd, shell=True, stdout=PIPE, cwd=cwd)
        output, _ = p.communicate()
        output = output.decode('unicode_escape').encode('raw_unicode_escape').decode('utf-8')

        if p.returncode:
            if sys.version_info > (2, 6):
                raise CalledProcessError(returncode=p.returncode, cmd=cmd, output=output)
            else:
                raise CalledProcessError(returncode=p.returncode, cmd=cmd)

        return output


def main():
    from optparse import OptionParser

    parser = OptionParser(
        usage="usage: %prog [-v] [--prefix PREFIX] [--no-exclude] [--force-submodules]"
              " [--extra EXTRA1 [EXTRA2]] [--dry-run] OUTPUT_FILE",
        version="%prog {0}".format(__version__)
    )

    parser.add_option('--prefix',
                      type='string',
                      dest='prefix',
                      default=None,
                      help="""prepend PREFIX to each filename in the archive.
                          OUTPUT_FILE name is used by default to avoid tarbomb.
                          You can set it to '' in order to explicitly request tarbomb""")

    parser.add_option('-v', '--verbose',
                      action='store_true',
                      dest='verbose',
                      help='enable verbose mode')

    parser.add_option('--no-exclude',
                      action='store_false',
                      dest='exclude',
                      default=True,
                      help="don't read .gitattributes files for patterns containing export-ignore attrib")

    parser.add_option('--force-submodules',
                      action='store_true',
                      dest='force_sub',
                      help='force a git submodule init && git submodule update at each level before iterating submodules')

    parser.add_option('--extra',
                      action='append',
                      dest='extra',
                      default=[],
                      help="any additional files to include in the archive")

    parser.add_option('--dry-run',
                      action='store_true',
                      dest='dry_run',
                      help="don't actually archive anything, just show what would be done")

    options, args = parser.parse_args()

    if len(args) != 1:
        parser.error("You must specify exactly one output file")

    output_file_path = args[0]

    if path.isdir(output_file_path):
        parser.error("You cannot use directory as output")

    # avoid tarbomb
    if options.prefix is not None:
        options.prefix = path.join(options.prefix, '')
    else:
        import re

        output_name = path.basename(output_file_path)
        output_name = re.sub(
            '(\.zip|\.tar|\.tgz|\.txz|\.gz|\.bz2|\.xz|\.tar\.gz|\.tar\.bz2|\.tar\.xz)$',
            '',
            output_name
        ) or "Archive"
        options.prefix = path.join(output_name, '')

    try:
        handler = logging.StreamHandler(sys.stdout)
        handler.setFormatter(logging.Formatter('%(message)s'))
        GitArchiver.LOG.addHandler(handler)
        GitArchiver.LOG.setLevel(logging.DEBUG if options.verbose else logging.INFO)
        archiver = GitArchiver(options.prefix,
                               options.exclude,
                               options.force_sub,
                               options.extra)
        archiver.create(output_file_path, options.dry_run)
    except Exception as e:
        parser.exit(2, "{0}\n".format(e))

    sys.exit(0)


if __name__ == '__main__':
    main()
