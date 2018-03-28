#!/bin/sh

SCRIPT_DIR=`dirname "$0"`
SCRIPT_DIR=`readlink -f ${SCRIPT_DIR}`

${SCRIPT_DIR}/fetch-git-revision.sh ${SCRIPT_DIR}/waf https://github.com/waf-project/waf.git "" waf-2.0.6

cd ${SCRIPT_DIR}/waf/ && python ./waf-light --tools=clang_compilation_database,why,build_logs configure build

cp ${SCRIPT_DIR}/waf/waf ${SCRIPT_DIR}/../
