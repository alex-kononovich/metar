# METAR reports

This is a small utility capable of parsing
[METAR](https://en.wikipedia.org/wiki/METAR) reports, generating random METAR
reports and reporting average wind speed on a set of reports.

## Setup

To begin, you need to install
[Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install), and
then install appropriate GHC

    $ stack setup

and build the project

    $ stack build

## Usage

Project consists of two executables: `metar` and `metar-generate`. You can run
them by prepending `stack exec` before the name. Running without arguments or
with `--help` flag will show usage instructions.

    $ stack exec metar -- --help

### `metar-generate`

This utility helps to generate random METAR reports for testing. For example,
generate 200000 reports and save them to file named `records.txt`:

    $ stack exec metar-generate -- -n 200000 -f records.txt

### `metar`

This utility can parse METAR record and print it back, or read a stream (from
`stdin` or file) of records and keep a running average on them.

Parse a single record:

    $ stack exec metar -- 'YYZ 122201Z 12023MPS'

Read a stream:

    $ stack exec metar -- -f records.txt

It is also possible to utilize unix pipes to simultaneously generate and read a
stream of records:

    $ stack exec metar-generate -- -n 200000 --stdout | stack exec metar -- --stdin

## Testing

Project includes tests for parsers, to run tests execute

    $ stack test

