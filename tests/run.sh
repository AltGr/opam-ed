#!/bin/bash
set -ue
#set -x

echo "## Testing empty stdin"
diff -u <(./opam-ed "add bin [\"foo\"]" <&-) <(echo "bin: [\"foo\"]")

file=tests/opam
if [ -e $file ]; then rm $file; fi
touch $file

# populate
for field in 'opam-version "2.0"' \
  'name "foo"' \
  'version "4.2"' \
  'depends ["ocaml" "bar" "quz"]' \
  'synopsis "Package descr"' \
  'description "Syn"' \
  'maintainer "Maintained"' \
  'authors "Auth"' \
  'homepage "url"' \
  'bug-reports "url"' \
  'tags "comp"' \
  ; do
  ./opam-ed "add $field" -f $file --inplace
done

opam lint $file

diff -u <(./opam-ed field-list -f $file) - << EOF
opam-version
name
version
depends
synopsis
description
maintainer
authors
homepage
bug-reports
tags
EOF

check-field () {
  op=$1
  field=$2
  in=$3
  if [ $# -eq 4 ]; then
    out=$4
  else
    out=""
  fi
  echo "## Testing $3"
  case $op in
    existent)
      diff -u <(./opam-ed "$in" -f $file | grep "^$field") <(echo "$out")
      ;;
    missing)
      test ! `grep $field <(./opam-ed "$in" -f $file)`
      ;;
    first)
      diff -u <(./opam-ed "$in" -f $file | head -1) <(echo "$out")
      ;;
    last)
      diff -u <(./opam-ed "$in" -f $file | tail -n 1) <(echo "$out")
      ;;
  esac
}

check-field  missing   "description"  'remove description'                      'opam-version: "2.0"'
check-field  existent  "version"      'replace version "2.4"'                   'version: "2.4"'
check-field  existent  "version"      'add-replace version "4.2"'               'version: "4.2"'
check-field  existent  "description"  'add-replace description "snd one"'       'description: "snd one"'
check-field  existent  "depext"       'append depext "m4"'                      'depext: ["m4"]'
check-field  existent  "tags"         'append tags "ap"'                        'tags: ["comp" "ap"]'
check-field  existent  "tags"         'prepend tags "pre"'                      'tags: ["pre" "comp"]'
check-field  existent  "depext"       'prepend depext "m4"'                     'depext: ["m4"]'
check-field  existent  "depends"      'map depends tr a A'                      'depends: ["ocAml" "bAr" "quz"]'
check-field  existent  "depends"      'filter depends grep a'                   'depends: ["ocaml" "bar"]'
check-field  existent  "depends"      'replace-item depends "quz" "baz"'        'depends: ["ocaml" "bar" "baz"]'
check-field  existent  "depends"      'add-replace-item depends "quz" "baz"'    'depends: ["ocaml" "bar" "baz"]'
check-field  existent  "depends"      'add-replace-item depends "miss" "baz"'   'depends: ["ocaml" "bar" "quz" "baz"]'
check-field  existent  "depends"      'remove-item depends "quz"'               'depends: ["ocaml" "bar"]'

rm $file
