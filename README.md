## Autograter - generates Canvas gradebook import from Autograder export

Autograter (if you'll forgive the pun) generates a Canvas gradebook from some
[Autograder](https://eecs-autograder.github.io/autograder.io/) export.

**Build:** run `cabal build` in project root

**Usage:**
```
Autograter PROJECT TEMPLATE_PATH [-t|--with-test]
                  [-o|--output OUTPUT_PATH]
```

The program takes an Autograder project name (`PROJECT`) and the path to a
Canvas gradebook template (`TEMPLATE_PATH`). The program expects the Autograder
export CSV file to be named `PROJECT.csv`. If there exists a mutation testing
component ("tests for the students' tests") for your assignment, then the
mutation testing part should live in a separate Autograder project and its
export should be named `PROJECT_TEST.csv`. The program generates a file if
`OUTPUT_PATH` is supplied; otherwise, the program prints to standard output.

**Available options:**
```
  PROJECT                  Name of your Autograder project
  TEMPLATE_PATH            Path to the Canvas gradebook template file
  -t,--with-test           Whether your project has a mutation testing component
  -o,--output OUTPUT_PATH  Path to the output file
  -h,--help                Show this help text
```
