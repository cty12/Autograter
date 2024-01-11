Autograter (if you'll forgive the pun) generates a Canvas gradebook from some
[Autograder](https://eecs-autograder.github.io/autograder.io/) export.

Usage:

```
Autograter - generate Canvas gradebook from Autograder export

Usage: Autograter PROJECT TEMPLATE_PATH [-t|--with-test]
                  [-o|--output OUTPUT_PATH]

  Takes an Autograder project name and path to a Canvas gradebook template. The
  Autograder export CSV file should be named 'PROJECT.csv'. Export of mutation
  testing, if exists as a separate file, should be named 'PROJECT_TEST.csv'
  Outputs to 'OUTPUT_PATH' if given and stdout otherwise

Available options:
  PROJECT                  Name of the Autograder project
  TEMPLATE_PATH            Path to the Canvas gradebook template file
  -t,--with-test           Whether the project has mutation testing
  -o,--output OUTPUT_PATH  Path to the output file
  -h,--help                Show this help text
```
