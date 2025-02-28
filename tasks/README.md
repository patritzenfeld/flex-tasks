# Test Script

Use the test.sh script to develop your task configurations.
It interprets the individual files in GHC and runs HLint on them.
In addition, the script interpolates the `Check.hs` module and scans it for optimization opportunities.

If any suggestions are found, a folder named after your task is created.
This folder contains the modified `Check.hs` module and HTML summaries of the three phases described above.
If there are no suggestions, the folder won't be created.

## Usage

`./test.sh TASK_PATH PKG_DB_PATH [-c]`

### example

`./test.sh proplogic.txt flex-pkgdb`

## Requirements

- stack
- ghci version used in the package database (if you don't know, execute the script and look for an error message)
- ansi2html (colorized-logs)

## Notes

- Generate a package database using [our tool](https://github.com/fmidue/haskell-template-setup)
- Both `TASK_PATH` and `PKG_DB_PATH` are given as relative paths (have to be in this folder)
- The `-c` flag can optionally be supplied to preserve the generated Check.hs file even if no reports were generated
