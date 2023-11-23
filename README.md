[![MIT License][license-shield]][license-url]

# SimpleParser

SPBU 3rd semester homework assignment.

<ol>
    <li>
        <a href="#language-grammar">Language Grammar</a>
    </li>
    <li>
        <a href="#getting-started">Getting Started</a>
        <ul>
            <li><a href="#prerequisites">Prerequisites</a></li>
            <li><a href="#installation">Installation</a></li>
            <li><a href="#usage">Usage</a></li>
        </ul>
    </li>
    <li>
        <a href="#license">License</a>
    </li>
</ol>

## Language grammar

Definition using [EBNF][grammar-notation] notation:

`[x]` denotes one or more

`{x}` denotes zero or more

`(x)` denotes a capture group

```ignorelang
digit = "1" | ... | "9"

number = { digit }

letter = "a" | ... | "z" | "A" | ... | "Z"

identifier = letter { letter | digit }

factor = identifier | number | ifExpression | (factor "*" factor)

add = factor | (add "+" add)

arithmeticExpression = factor | add

booleanValue = "true" | "false"

relationalOperator = "=" | "<>" | "<=" | "<" | ">=" | ">"

booleanExpression = booleanValue | (arithmeticExpression | identifier) relationalOperator (arithmeticExpression | identifier)

ifExpression = "if" "(" booleanExpression ")" "then" 
                   "(" [ arithmeticExpression | booleanExpression | ifExpression ] ")"
               "else"
                   "(" [ arithmeticExpression | booleanExpression | ifExpression ] ")"                

command = "print:" (arithmeticExpression | identifier)

assignment = identifier "=" (booleanValue | arithmeticExpression | identifier | ifExpression)

statement = assignment | command

program = {statement}
```

## Getting Started

To get a local copy up and running follow the steps below.

### Prerequisites

[.NET >= 7.0][net-link]

### Installation

1. Clone the repo:

    ```shell
    git clone https://github.com/artem-burashnikov/SimpleParser.git
    ```
   
2. Build the project:

    ```shell
    ./build.sh
    ```

### Usage

CLI is available:

```ignorelang
USAGE: SimpleParser [--help] <path>

FILEPATH:

    <path>                Specify a path to the file 
                          containing a program.

OPTIONS:

    --help                Display this list of options.
```
Following a language definition, a simple program may look like this:
```
x=3
y=2
z=if(true)then(x*y+7)else(1)
print:z
```
Outputs `13` to the console.

## License

Distributed under the MIT License. See [LICENSE][license-url] for more information.

<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[license-shield]: https://img.shields.io/github/license/artem-burashnikov/SimpleParser.svg?style=for-the-badge
[license-url]: https://github.com/artem-burashnikov/SimpleParser/blob/main/LICENSE
[net-link]: https://dotnet.microsoft.com/en-us/download
[grammar-notation]: https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form
