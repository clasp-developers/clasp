#!/bin/bash
# from https://github.com/hgomez/devops-incubator/tree/master/forge-tricks

readonly PROGNAME=$(basename $0)
readonly PROGDIR=$(readlink -m $(dirname $0))
readonly ARGS="$@"

#
# Get up to date JSON file of stable plugins
#
get_update_center() {

  echo "Fetching JSON from update center"

# fetch up to date update center
  $CURL_CMD http://updates.jenkins-ci.org/stable/update-center.json -o $PLUGIN_TEMPDIR/update-center.json

# cleanup json
  sed -i 's|updateCenter.post(||g' $PLUGIN_TEMPDIR/update-center.json
  sed -i 's|);||g' $PLUGIN_TEMPDIR/update-center.json
}

#
# Fetch and install plugin
#
fetch_plugin()
{
  local DEP_LOOP
  local DEPENDENCIES
  local DEPENDENCY
  local URL
  local VERSION
  local SFILENAME
  local FFILENAME
  local PPLUGIN
  local OPTIONALP
  local FPLUGIN=$1

  if [ ! -z "$EXCLUDED_PLUGINS" ]; then
  for PPLUGIN in $EXCLUDED_PLUGINS
  do
    if [ "$FPLUGIN" = "$PPLUGIN" ]; then
    echo "$FPLUGIN is already provided, exiting"
    return
    fi
  done
  fi

  URL=`cat $PLUGIN_TEMPDIR/update-center.json | python -c "import sys, json; print json.load(sys.stdin)[\"plugins\"][\"$FPLUGIN\"][\"url\"]"`
  VERSION=`cat $PLUGIN_TEMPDIR/update-center.json | python -c "import sys, json; print json.load(sys.stdin)[\"plugins\"][\"$FPLUGIN\"][\"version\"]"`

  SFILENAME=`basename $URL | sed -e "s|.hpi|.jpi|g"`
  FFILENAME=`basename $URL | sed -e "s|.hpi|-$VERSION.jpi|g"`

  INCLUDEDPLUGINS="$INCLUDEDPLUGINS$FPLUGIN:$VERSION\n"

  if [ ! -f $PLUGIN_TEMPDIR/$FFILENAME ]; then
  echo "Downloading $FPLUGIN"
  $CURL_CMD $URL -o $PLUGIN_TEMPDIR/$FFILENAME
  fi

  cp $PLUGIN_TEMPDIR/$FFILENAME $PLUGINS_DIR/$SFILENAME

  DEPENDENCIES=`cat $PLUGIN_TEMPDIR/update-center.json | python -c "import sys, json; print json.load(sys.stdin)[\"plugins\"][\"$FPLUGIN\"][\"dependencies\"]"`

  if [ "$DEPENDENCIES" != "[]" ]; then

  for DEP_LOOP in 0 1 2 3 4 5 6 7 8 9 10; do
    DEPENDENCY=`cat $PLUGIN_TEMPDIR/update-center.json | python -c "import sys, json; print json.load(sys.stdin)[\"plugins\"][\"$FPLUGIN\"][\"dependencies\"][$DEP_LOOP][\"name\"]" 2>/dev/null || true`

    # No more dependency, exit loop
    if [ "$DEPENDENCY" = "" ]; then
    break;
    fi

    OPTIONALP=`cat $PLUGIN_TEMPDIR/update-center.json | python -c "import sys, json; print json.load(sys.stdin)[\"plugins\"][\"$FPLUGIN\"][\"dependencies\"][$DEP_LOOP][\"optional\"]" 2>/dev/null|| true`

    # Don't fetch optional dependencies
    if [ "$OPTIONALP" = "True" ]; then
    echo "$DEPENDENCY plugin is optional, it won't be included"
    continue;
    fi

    fetch_plugin $DEPENDENCY
  done
  fi
}

usage() {
  cat <<- EOF
  usage: $PROGNAME options

  Install or update Jenkins Plugins.

  OPTIONS:
     -p --plugins  file containing plugins list
     -x --xplugins   file containing excluded plugins list
     -d --plugindir  directory where to deploy plugins (.jpi)

  Examples:

     Run:
     $PROGNAME --plugins okplugins --excludedplugins nokplugins --plugindir /var/lib/myjenkins/plugins
EOF

  exit 1
}

#
# Parse command line
#
cmdline() {
  # got this idea from here:
  # http://kirk.webfinish.com/2009/10/bash-shell-script-to-use-getopts-with-gnu-style-long-positional-parameters/
  local arg=
  for arg
  do
    local delim=""
    case "$arg" in
      #translate --gnu-long-options to -g (short options)
      --plugins)     args="${args}-p ";;
      --xplugins)    args="${args}-e ";;
      --plugindir)     args="${args}-d ";;
      --help)      args="${args}-h ";;
      --verbose)     args="${args}-v ";;
      --debug)       args="${args}-x ";;
      #pass through anything else
      *) [[ "${arg:0:1}" == "-" ]] || delim="\""
        args="${args}${delim}${arg}${delim} ";;
    esac
  done

  #Reset the positional parameters to the short options
  eval set -- $args

  while getopts "hvxp:e:d:" OPTION
  do
     case $OPTION in
     v)
       readonly VERBOSE=1
       ;;
     x)
       readonly DEBUG='-x'
       set -x
       ;;
     h)
       usage
       exit 0
       ;;
     p)
       readonly PLUGINS_FILE=$OPTARG
       ;;
     e)
       readonly EXCLUDED_PLUGINS_FILE=$OPTARG
       ;;
     d)
       readonly PLUGINS_DIR=$OPTARG
       ;;
    esac
  done

  if [ -z "$PLUGINS_FILE" ]; then
    echo "You must provide plugin file"
    usage
  fi

  if [ -z "$PLUGINS_DIR" ]; then
    echo "You must provide plugin directory"
    usage
  fi

  readonly PLUGINS=`cat $PLUGINS_FILE`

  if [ ! -z "$EXCLUDED_PLUGINS_FILE" ]; then
    readonly EXCLUDED_PLUGINS=`cat $EXCLUDED_PLUGINS_FILE`
  fi

  if [ "$VERBOSE" = "1" ]; then
  CURL_CMD="curl -L"
  else
  CURL_CMD="curl -L --silent"
  fi

  if [ "$DEBUG" = "-x" ]; then
  CURL_CMD="$CURL_CMD -v"
  fi
}


main() {

  cmdline $ARGS

  readonly PLUGIN_TEMPDIR=`mktemp -d /tmp/batchjpi.XXXXXXX`

  get_update_center

  for PLUGIN in $PLUGINS
  do
  echo "Fetching plugin $PLUGIN and dependencies"
  fetch_plugin $PLUGIN
  done

  rm -rf $PLUGIN_TEMPDIR
}

main

