#!/usr/bin/python

# Copyright 2004 Vladimir Prus
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)

# Tests the link-directory rule used to create the
# common boost/ directory in the new git layout.

import BoostBuild

def ignore_config(t):
    """These files are created by the configuration logic in link.jam
    They may or may not exist, depending on the system."""
    t.ignore("bin/symlink/test-hardlink")
    t.ignore("bin/test-hardlink-source")
    t.ignore("bin/test-symlink")
    t.ignore("bin/test-symlink-source")

def test_basic():
    """Test creation of a single link"""
    t = BoostBuild.Tester()
    t.write("jamroot.jam", """\
    import link ;
    link-directory dir1-link : src/dir1/include : <location>. ;
    """)

    t.write("src/dir1/include/file1.h", "file1")

    t.run_build_system()

    t.expect_addition("include/file1.h")
    t.expect_content("include/file1.h", "file1")
    ignore_config(t)
    t.expect_nothing_more()
    t.cleanup()

def test_merge_two():
    """Test merging two directories"""
    t = BoostBuild.Tester()
    t.write("jamroot.jam", """\
    import link ;
    link-directory dir1-link : src/dir1/include : <location>. ;
    link-directory dir2-link : src/dir2/include : <location>. ;
    """)

    t.write("src/dir1/include/file1.h", "file1")
    t.write("src/dir2/include/file2.h", "file2")

    t.run_build_system()

    t.expect_addition("include/file1.h")
    t.expect_content("include/file1.h", "file1")
    t.expect_addition("include/file2.h")
    t.expect_content("include/file2.h", "file2")
    ignore_config(t)
    t.expect_nothing_more()
    t.cleanup()

def test_merge_existing():
    """Test adding a link when a different symlink already exists"""
    t = BoostBuild.Tester()
    t.write("jamroot.jam", """\
    import link ;
    link-directory dir1-link : src/dir1/include : <location>. ;
    link-directory dir2-link : src/dir2/include : <location>. ;
    """)

    t.write("src/dir1/include/file1.h", "file1")
    t.write("src/dir2/include/file2.h", "file2")

    t.run_build_system(["dir1-link"])

    t.expect_addition("include/file1.h")
    t.expect_content("include/file1.h", "file1")
    ignore_config(t)
    t.expect_nothing_more()

    t.run_build_system(["dir2-link"])

    t.expect_addition("include/file2.h")
    t.expect_content("include/file2.h", "file2")
    # If include is a symlink to src/dir1/include, then
    # we have to delete it and add a directory.
    t.ignore_removal("include/file1.h")
    ignore_config(t)
    t.expect_nothing_more()

    t.cleanup()

def test_merge_recursive():
    "Test merging several directories including common prefixes"
    t = BoostBuild.Tester()
    t.write("jamroot.jam", """\
    import link ;
    link-directory dir1-link : src/dir1/include : <location>. ;
    link-directory dir2-link : src/dir2/include : <location>. ;
    link-directory dir3-link : src/dir3/include : <location>. ;
    """)

    t.write("src/dir1/include/file1.h", "file1")
    t.write("src/dir2/include/file2.h", "file2")
    t.write("src/dir2/include/nested/file3.h", "file3")
    t.write("src/dir3/include/nested/file4.h", "file4")

    t.run_build_system()

    t.expect_addition("include/file1.h")
    t.expect_content("include/file1.h", "file1")
    t.expect_addition("include/file2.h")
    t.expect_content("include/file2.h", "file2")
    t.expect_addition("include/nested/file3.h")
    t.expect_content("include/nested/file3.h", "file3")
    t.expect_addition("include/nested/file4.h")
    t.expect_content("include/nested/file4.h", "file4")
    ignore_config(t)
    t.expect_nothing_more()

    t.cleanup()

def test_include_scan():
    """Make sure that the #include scanner finds the headers"""
    t = BoostBuild.Tester()
    t.write("jamroot.jam", """\
    import link ;
    link-directory dir1-link : src/dir1/include : <location>. ;
    link-directory dir2-link : src/dir2/include : <location>. ;
    obj test : test.cpp :
        <include>include
        <implicit-dependency>dir1-link
        <implicit-dependency>dir2-link ;
    """)

    t.write("src/dir1/include/file1.h", "#include <file2.h>\n")
    t.write("src/dir2/include/file2.h", "int f();\n")
    t.write("test.cpp", """\
    #include <file1.h>
    int main() { f(); }
    """);

    t.run_build_system(["test"])

    t.expect_addition("bin/$toolset/debug/test.obj")

    t.run_build_system()
    t.expect_nothing_more()

    t.cleanup()

test_basic()
test_merge_two()
test_merge_existing()
test_merge_recursive()
test_include_scan()
