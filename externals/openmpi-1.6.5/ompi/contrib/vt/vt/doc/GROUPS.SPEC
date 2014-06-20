# VampirTrace function group specification
#
# Group definitions and region assignments:
#
# Syntax: <group>=<functions>
#
#   group      group name 
#   regions    semicolon-seperated list of functions
#              (can contain wildcards)
#
# Example:
#
#   MATH=add;sub;mul;div
#   USER=app_*
#
# These group assignments associate the functions "add", "sub", "mul" and "div"
# with group "MATH" and all functions with the prefix "app_" are associated with
# group "USER".
