#!/usr/bin/env bash

if [ "$#" -ne 2 ] && [ "$#" -ne 3 ]; then
  echo "Usage: $0 input_file pkgdb_directory [-c]"
  exit 1
fi

if [[ ! -f $1 || ! -d $2 ]]; then
  echo "file or pkgdb directory does not exist!"
  exit 1
fi

leave_check=false

if [ "$#" -eq 3 ]; then
  if [ "$3" = "-c" ]; then
    leave_check=true
  else
    echo "Unknown flag: $3"
    echo "Usage: $0 input_file pkgdb_directory [-c]"
    exit 1
  fi
fi

base_name=$(basename "$1" | sed 's/\(.*\)\..*/\1/')
pkg_path=$PWD/$2/pkgdb
script_path="$(realpath "$(dirname "$0")")"
expect_script="${script_path}/runGhci.expect"
mapfile -t files \
  < <(awk '/^\s*module/ && !/^\s*module +Check +/ {sub(/module /,"")sub(/ (where|\().*/,".hs"); print}' "$1")
files=("${files[@]/#/$base_name/}")
CYAN='\033[0;36m'
GREEN='\033[0;32m'
LIGHT_GREEN='\033[1;32m'
RED='\033[0;31m'
NC='\033[0m'
current_file=0
hlint_removed=false
ghc_removed=false
scan_removed=false

mkdir -p "/tmp/flex-test"

echo -e "${CYAN}Testing $base_name!\n${NC}"
echo -e "${CYAN}Writing .hs files...\n${NC}"

mkdir -p "${base_name}"
true >"${files[0]}"

while IFS= read -r line || [ -n "$line" ]; do
  # Check for module separator
  if [[ "$line" =~ === ]]; then
    ((current_file++))
    true >"${files[$current_file]}"
    continue
  fi
  echo "${line//$'\r'/}" >>"${files[$current_file]}"
done <"$1"

cp "$1" "$base_name/config.txt"

echo -e "${CYAN}Interpreting the code files...${NC}"

export GHC_PACKAGE_PATH=$pkg_path
ghc_file=$(find "$pkg_path" -name "ghci*" -print -quit)
temp="${ghc_file##*/ghci-}"
ghc_version="${temp%-*.conf*}"

cd "$base_name" || exit 1
expect "$expect_script" "$ghc_version" |
  sed -e 's/.*\*\*\*/\*\*\*/g' -e '/GHCi, version/d' -e '/ghci> /d' -e '/modules loaded./d' |
  ansi2html >ghc.html
if [ "$(grep -vw "Compiling" -c "ghc.html")" -eq 51 ]; then
  rm ghc.html
  ghc_removed=true
  echo -e "${GREEN}No Warnings!\n${NC}"
else
  echo -e "${GREEN}GHC reported warnings!\n${NC}"
fi

echo -e "${CYAN}Writing Hlint report...${NC}"
hlint . --report="hlint.html" -q --hint="${script_path}/.hlint.yaml"
if grep -q "No hints" hlint.html; then
  rm hlint.html
  hlint_removed=true
  echo -e "${GREEN}No Suggestions!\n${NC}"
else
  echo -e "${RED}Suggestions available!\n${NC}"
fi

echo -e "${CYAN}Running Check.hs scan...${NC}"
"$script_path"/scan_check.sh
if [ "$(grep -w "class=header" -c scan_check.html)" -eq 0 ]; then
  rm scan_check.html
  scan_removed=true
  echo -e "${GREEN}No Issues!\n${NC}"
else
  echo -e "${RED}Issues detected!\n${NC}"
fi

echo -e "${CYAN}Deleting intermediate files...\n${NC}"

if $hlint_removed && $ghc_removed && $scan_removed && ! $leave_check; then
  cd ..
  rm -f -r "$base_name"
else
  files=("${files[@]#$base_name/}")
  rm -f config.txt
  for file in "${files[@]}"; do
    rm -f "$file"
  done
fi

echo -e "${LIGHT_GREEN}Done! Check the reports in ghc.html, hlint.html and scan_check.html.\n${NC}"
