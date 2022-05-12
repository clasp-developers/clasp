class Cando < Formula
  desc "Cando chemisty and Common Lisp implementation that brings Common Lisp and C++ together"
  homepage "https://github.com/clasp-developers/clasp"
  url "https://github.com/clasp-developers/clasp/archive/refs/heads/main.tar.gz"
  version "1.0.0"
  sha256 "d0bbebb1f885ad353e69d77f3d8297869d598e2619acebed9daf61c47385e150"
  license "GPL-2.0-or-later"

  depends_on "boost" => :build
  depends_on "libunwind-headers" => :build
  depends_on "ninja" => :build
  depends_on "pkg-config" => :build
  depends_on "sbcl" => :build
  depends_on "expat"
  depends_on "fmt"
  depends_on "gmp"
  depends_on "llvm"
  depends_on "netcdf"
  uses_from_macos "libffi"

  def install
    ENV.deparallelize
    system "sbcl", "--load", "koga.lisp", "--quit", "--end-top-level-options", "--bin-path=#{bin}", "--share-path=#{share}/clasp/", "--lib-path=#{lib}/clasp/", "--jupyter-path=#{share}/jupyter/", "--jupyter", "--extensions=cando,seqan-clasp"
    system "ninja", "-C", "build"
    system "ninja", "-C", "build", "install"
  end

  test do
    assert_match "clasp-boehmprecise-1.0.0", shell_output("#{bin}/clasp --version")
  end
end
