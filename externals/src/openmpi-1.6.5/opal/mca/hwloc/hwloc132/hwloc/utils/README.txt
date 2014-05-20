Open MPI doesn't need this tree from hwloc.  But automake *requires*
that this directory has to be here.  So we have an empty directory
with a README in it, a) just to explain why it's here, and b) so that
hg clones won't delete the directory (because it's empty).
