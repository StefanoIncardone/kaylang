# Feature Ideas

>[!WARNING]
> no feature is final, modifications can happen at any moment

## 0.6.3/?.?.? - invoking internal tools

Ability to invoke internal tools such as assembler and linkers, basically turning the compiler into
an assembler/linker frontend/wrapper:

```shell
# any argument after the --assembler flag gets sent to the assembler
kay compile --assembler main.asm # invoke the assembler

# any argument after the assemble command gets sent to the assembler
kay assemble main.asm # invoke the assembler

# any argument after the --linker flag gets sent to the linker
kay compile --linker main.o # invoke the linker

# any argument after the link command gets sent to the assembler
kay link main.o # invoke the linker
```

and all of the previous commands will produce the same final executable

## 0.6.4 - Revised comments

- i like the `#` for compiler directives instead of say `@`
- could treat `#` as a compile directive:
- use a second `#` to signal a line comment:

    ```kay
    #compiler_directive
    ## line comment
    ```

- use a opening/closing symbol to signal a block comment:

    ```kay
    ## current block comment ## does not allow nested comments ## ##
    #* block comment #* would allow for nested block comments *# *#
    #/ block comment /#
    #/ as an easter-egg, a block comment formatted like this resembles a % symbol
    /#
    #/ comments with a difference of one character look like a slide /#
    #/ comments with a difference of one character are like sliding /#
    #" block comment "#
    #"
        block comment
    "#
    #< block comment >#
    #{ block comment }#
    #{
     block comment 
    }#
    ```

- use a second `#` followed by the previous "directives" for a documentation comment:

    ```kay
    ### documentation line comment
    ##* documentation block comment *##
    ```

maybe experiment with deprecation periods:

- support both old and new comment styles and emit a warning/error when encountering the old style:
    - would need to develop a proper log with warning/error/note/hint system

## ?.?.? - Disallowing optional trailing comma, make it mandatory

```kay
let i = [1, 2, 3]; # would not be allowed
let i = [1, 2, 3,]; # trailing comma would be mandatory for consistency

# would allow to move line up and down without running into "missing comma" errors
let i = [
    1,
    2,
    4 # missing comma because we moved a line up
    3,
];
let i = [
    1,
    2,
    4, # would not be a problem if a trailing comma was mandatory
    3,
];

# should we do it for function arguments?
fn sum: i64 = sum(a: i64, b: i64,) {
    return a + b;
}

# should we do it for function calls?
let sum = sum(12, 21,);

# should we do it for generics?
struct Point<I,>(
    x: I,
    # should we do it for struct/enum/... fields?
    y: I,
)

# should we do it for generics?
let p = Point<i64,>(x = 12, y = 21,);

if a { println "a"; }
else if b { println "b" } # could we allow "trailing semicolons" before curly brackets?
```

might just be a compiler or linter flag

```shell
kay run ... ... --mandatory-trailing-commas
```

or maybe make the trailing comma optional for single line statements and mandatory for multiline:

```kay
let i = [1, 2, 3, 4];
let i = [1, 2, 3, 4,]; # optional trailing comma
let i = [
    1,
    2,
    3,
    4 # Error: mandatory trailing comma
]
```

could be applied to semicolons as well:

```kay
if ... { println "foo"; }
if ... { println "foo" } # optional trailing semicolon
if ... {
    println "foo"; # mandatory trailing semicolon
}
```

## ?.?.? - explicit loop continue/break block

Allow for a loop to specify what should happen when using `continue` and `break`:

```kay
var i = 0;
loop i < 10 {
    continue { i += 1; }
    break { println "done"; }

    if i & 1 == 0 {
        continue; # goes inside the continue block previously declared
    } else if i == 3 {
        break; # goes inside the break block previously declared
    }
    # each block except the first will start executing from the continue block
}

# or
loop i < 10
continue { i += 1; }
break { println "done"; } {
    if i & 1 == 0 {
        continue; # goes inside the continue block previously declared
    } else if i == 3 {
        break; # goes inside the break block previously declared
    }
    # each block except the first will start executing from the continue block
}

# or
loop i < 10 {
    if i & 1 == 0 {
        continue; # goes inside the continue block
    } else if i == 3 {
        break; # goes inside the break block declared
    }
    # each block except the first will start executing from the continue block
} continue {
    i += 1;
} break {
    println "done";
}

# could just be
loop i < 10 {
    :break_label {
        :continue_label {
            if condition {
                break:break_label;
            }
            if other_condition {
                continue:continue_label;
            }
        }

        # continue logic
        continue;
    }

    # break logic
    break;
}
```

## ?.?.? - Expressions formatting

emit a warning for ambiguos use of unary/binary operators, i.e.:

```kay
1 + 2 -3
    # ^ this means `1 minus 2 minus 3` but it might mean `1 minus 2 *missing* negative 3`
    # Help: to avoid ambiguity consider formatting the code as `1 + 2 - 3`, or if you meant
    # negative 3 you might be missing an operator between `2` and `-3` -> `1 + 2 *op* -3`
```

## ?.?.? - Built-in tags

Implement a way to recognize and collect todos, and other tags

```kay
# at: file.kay

(12) # TODO(stefano) implement this features
#                   ^^^^^^^^^^^^^^^^^^^^^^^^ everything after the TODO(stefano) is part of the message
(42) # IDEA(stefano)genious
#                   ^notice the missing space
```

running the `kay tags file.kay` command would output something like:

```text
TODO(stefano): file.kay:12: implement this feature
IDEA(stefano): file.kay:42:genious
```

Could create a config file that specifies what tags to look for, like a tags.toml, or create
dedicated cli commands and flags:

```shell
kay tags -n TODO -n IDEA -n NOTE # would recognize TODO, IDEA and NOTE
kay tags # Error: no specified tags to look for
```

## 0.6.3/0.6.4 - Sub-menu help commands

printing the help message could lead to some useful information being offscreen, since the more
relevant options are usually listed first, could only print some information and provide some sort
of "sub-menu" system:

```shell
kay help

1: ...
2: ...

Note: use `kay help *specific command*` for further explanation
```

## 0.6.3/0.6.4 - More output file names flags

currently only the output path (`-o`, `--output`) can be specified and the names of the generated artifacts is
generated from the source file name, i.e:

| source file path | output directory path | assembly file path | object file path | executable file path |
| :--------------- | :-------------------- | :----------------- | :--------------- | :------------------- |
| `test.kay`       |                       | `test.asm`         | `test.o`         | `test`               |
| `test.kay`       | `out`                 | `out/test.asm`     | `out/test.o`     | `out/test`           |

could introduce flags to customize individual file output directories and file names separately:

| file kind   | output directory flag        | output file path flag      |
| :---------- | :--------------------------- | :------------------------- |
| general     | `-o`, `--output`             | `-n`, `--name`             |
| assembly    | `-oa`, `--output-assembly`   | `-na`, `--name-assembly`   |
| object file | `-oo`, `--output-object`     | `-no`, `--name-object`     |
| executable  | `-oe`, `--output-exectuable` | `-ne`, `--name-exectuable` |

or combined:

- assembly output name and directory: `-oa .. -n ..`, `--output-assembly .. --name ..`
- object output name and directory: `-oo .. -n ..`, `--output-object .. --name ..`
- executable output name and directory: `-oe .. -n ..`, `--output-exectuable .. --name ..`

so using the `compile` command with these extra arguments would work like (probably too specific,
the user could just wrap this in a shell script to obtain the same behavior):

| command                                                    | assembly file path | object file path | executable file path  |
| :--------------------------------------------------------- | :----------------- | :--------------- | :-------------------- |
| `test.kay -oa asm`                                         | `asm/test.asm`     | `test.o`         | `test`                |
| `test.kay -no test_obj`                                    | `test.asm`         | `test_obj.o`     | `test`                |
| `test.kay -o out -oa asm`                                  | `out/asm/test.asm` | `out/test.o`     | `out/test`            |
| `test.kay -o out -oa asm -oo obj -oe exe`                  | `out/asm/test.asm` | `out/obj/test.o` | `out/exe/test`        |
| `test.kay -oa asm -oe exe -ne test_executable -oo out/obj` | `asm/test.asm`     | `out/obj/test.o` | `exe/test_executable` |

### Artifacts creation opt-in/opt-out

Ability to customize the artifacts creation behaviour:

```shell
kay run main.kay --clean-artifacts # clean artifacts after succesfull build, implies keep by default
kay run main.kay --keep-artifacts # keep artifacts after build, implies clean by default
```

maybe make the compiler clean the artifacts by default

maybe repurpose the `-o` flag:

```shell
kay run main.kay # clean artifacts after succesfull build
kay run main.kay -o foo/ # artifacts in foo/
kay run main.kay -o-asm foo/ # only main.asm in foo/
kay run main.kay -o-obj foo/ # only main.obj in foo/
kay run main.kay -o-obj foo/ -o-asm bar/ # main.obj in foo/, main.asm in bar/
kay run main.kay -o-obj foo/ -o-asm bar/ # main.obj in foo/, main.asm in bar/
kay run main.kay -o-obj foo/ -n-obj foo -o-asm bar/ -n-asm bar # foo.obj in foo/, bar.asm in bar/
kay run main.kay -n-obj foo -o-asm bar/ -n-asm bar -n-exe baz # foo.obj in ./, bar.asm in bar/, baz.exe or baz in ./
kay run main.kay -n-obj foo -o-asm bar/ -n-asm bar -n-exe baz # foo.obj in ./, bar.asm in bar/, baz.exe or baz in ./
```

## 0.7.0 - Configuration files

ability to take in configurations from a file, akin to a project file like `Cargo.toml`:

```toml
# in config.toml
[run]
output = "foo"
obj_name = "obj"
...

[check]
... = ...
... = ...
```

```shell
kay run main.kay --config config.toml # would take the relevant configurations from the [run] entry
kay check main.kay --config config.toml # would take the relevant configurations from the [check] entry
```

could move to self hosting the compilation process, with a `build.kay` "build system":

```shell
kay run build.kay
```

## ?.?.? - Language version embedded in the binary executable

## ?.?.? - Amount of crash information

To reduce binary size we could allow to specify a flag controlling the amount of information printed
in the case of crashes.

As of right now when crashing due to arithmetic constraints we print the general following message:

```text
Crash: something specific wrong happened
at: file.kay:21:12
```

While this is useful because it cleary shows whan cause the problem and where it happened, it comes
with the drawback that every function now needs to accept extra information, thus leading to bigger
binary sizes due to:

- reason of the crash
- file name, line and column numbers of where the crash happened

Every function is thus bigger and more complex, so we could let a cli flag such as
`--crashinfo` followed by some crash info degree such as:

- `full`: as shown above:
- `none`: no reason, file, line and column information:

    ```text
    Crash: program crashed
    ```

- `reason`: only the reason of the crash:

    ```text
    Crash: something specific wrong happened
    ```

- `location`: only the location of the crash:

    ```text
    Crash: program crashed
    at: file.kay:21:12
    ```

This could also speedup performance since less information would be passed to functions, namely
reason of the crash, file, line and column number

## 0.7.0 - Loops/ifs

- loops similar to Odin's [for loops](https://odin-lang.org/docs/overview/#for-statement)
- ifs similar to Odin's [if statements](https://odin-lang.org/docs/overview/#if-statement)

## ?.?.? - Switch statements

- as little effort required to refactor from a regular if to a switch statement
- introducing as little new keywords as possible (just the `case` keyword, and possibly `fall`)
- avoiding added nesting:

```kay
# regular if:
# - 1 level of indentation
# - two keywords: `if`, `else`
if answer == 19 {
    println "lucky";
} else if answer == 21 {
    println "you stoopid";
} else if answer == 42 {
    println "that's the right answer";
} else {
    println "too bad";
}

# switch statement:
# - 1 level of indentation
# - one keyword plus the two from before: `if`, `else`, `case`
#   - possibly a `fall` keyword to specify a fallthrough case
# - same semantics as a regular if statement
#   - every case is like an `else if` branch
#   - it's basically just syntactic sugar
# - actually less code
# - requires minimal structural changes and refactoring:
#   - replace the first `==` and the rest of the `else if answer ==` with a `case`
#   - keep the `else` keyword for the 'default' case
if answer
case 19 {
    println "lucky";
} case 21 {
    println "you stoopid";
} case 42 {
    println "that's the right answer";
} else {
    println "too bad";
}

# switch statement:
# - 1 level of indentation
# - no additional keywords, let would become the "pattern matching" keyword
# - would allow to mix and match patterns and regular comparisons 
# - same semantics as a regular if statement
#   - every case is like an `else if` branch
if answer # note: this code makes no sense, it's just to showcase possible syntaxes
let Ok(19) {
    println "lucky";
} answer >= 21 {
    println "you stoopid";
} let 42 {
    println "that's the right answer";
} else {
    println "too bad";
}

# alternative switch statement:
# - 1 level of indentation
# - two keywords plus the two from before: `if`, `else`, `switch`, `case`
#   - possibly a `fall` keyword to specify a fallthrough case
# - same semantics as a regular if statement
#   - every case is like an `else if` branch
#   - it's basically just syntactic sugar
# - actually less code
# - requires minimal structural changes and refactoring:
#   - replace the first `if` keyword with the `switch` keyword
#   - replace the first `==` and the rest of the `else if answer ==` with a `case`
#   - keep the `else` keyword for the 'default' case
switch answer
case 19 {
    println "lucky";
} case 21 {
    println "you stoopid";
} case 42 {
    println "that's the right answer";
} else {
    println "too bad";
}

# pattern matching:
# - only requires to change `==` and `else if answer ==` to `case`
# - values inside pattern matching (i.e.: `ok` and `err`) are only available in the corresponding branch
# - split into multiple lines if preferred
# - short and concise
# - can declare mutability modifiers `let` or `var` on matched values
if answer
case let Ok(ok) { println ok; } # ok is available only in the do statement and is immutable
case var Err(err) { println err; } # err is available only in the do statement and is mutable
case let Err2(var err1, err2) {
    println err1; # err1 is available only in this block and is mutable
    println err2; # err2 is available only in this block and is immutable
}
case var Err2(let err1, err2) {
    println err1; # err1 is available only in this block and is immutable
    println err2; # err2 is available only in this block and is mutable
}

# could benefit from rust's mutability modifiers
# - would get rid of the initial mutabilty modifiers
if answer
case Ok(ok) { println ok; } # ok is available only in the do statement and is immutable by default
case Ok_b(let ok) { println ok; } # `let` is redundant
case Err(var err) { println err; } # err is available only in the do statement and is mutable
case Err2(var err1, err2) {
    println err1; # err1 is available only in this block and is mutable
    println err2; # err2 is available only in this block and is immutable
}

# rust-inspired let else syntax:
# - `ok` will be available from now on
let Ok(ok) = answer else {
    println "err";
    return; # branch need to diverge
}

# rust-inspired let else syntax, but more consistent with a regular pattern match:
# - `ok` will be available from now on
if Ok(let ok) = answer else {
    println "err";
    return; # branch need to diverge
}

# oh no! i need to access the error value
# - literally just add the pattern corresponding to the err case
# - 'err' will only be available in it's switch branch
# - `case` required to allow for more consistency when adding multiple cases
let Ok(ok) = answer else case let Err(err) {
    println err
    return;
}

# would allow for acces to other values in other patterns if needed
# - just add the other patterns
# - debate wheter the repetition of the `else` kewword should be addressed
let Ok(ok) = answer else
case let Err0(err0) {
    println err0;
    return;
} case var Err1(err1) {
    println err1;
    return;
} else {
    println "err";
    return;
}

# would just be syntactic sugar for
let ok = if answer
case let Ok(ok) {
    break ok;
}
case let Err0(err0) {
    println err0;
    return;
} case var Err1(err1) {
    println err1;
    return;
} else {
    println "err";
    return;
}
```

possibly allow for the operator before the first case to propagate, basically sugar for a regular if

```kay
# this would be treated as pattern matching
if answer
case 19 ...;
case 21 ...;
case 42 ...;
else ...;

# this would use the `==` operator to "pattern match" on cases
if answer ==
case 19 ...;
case 21 ...;
case 42 ...;
else ...;

# would be sugar for this
if answer == 19 ...;
else if answer == 21 ...;
else if answer == 42 ...;
else ...;

# this would use the `>` operator to "pattern match" on cases
if answer >
case 19 ...;
case 21 ...;
case 42 ...;
else ...;

# would be sugar for this
if answer > 19 ...;
else if answer > 21 ...;
else if answer > 42 ...;
else ...;

# this would use the `%` operator to "pattern match" on cases
if answer %
case 19 == 0 ...; # same as answer % 19 == 0
case 21 == 3 ...; # same as answer % 21 == 3
case 42 ...; # same as answer > 42 -> error: other branches evaluated to booleans while this evaluated to i64
else ...;

# would be sugar for this
if answer % 19 == 0 ...;
else if answer % 21 == 3 ...;
else if answer % 42 ...;
else ...;
```

### compared to C

```c
// regular if:
// - 1 level of indentation
// - two keywords: `if`, `else`
if (answer == 19) {
    printf("lucky");
} else if (answer == 21) {
    printf("you stoopid");
} else if (answer == 42) {
    printf("that's the right answer");
} else {
    printf("too bad");
}

// switch:
// - 2 levels of indentation
// - two/three keywords plus the two from before: `if`, `else`, `switch`, `case`, `break`
// - different semantics than a regular if statement
//  - each case is basically a goto statement
// - more code
// - requires lots of structural change and refactoring
switch (answer) {
    case 19: {
        printf("lucky");
        break;
    }
    case 21: {
        printf("you stoopid");
        break;
    }
    case 42: {
        printf("that's the right answer");
        break;
    }
    default: {
        printf("too bad");
    }
}

// switch:
// - same as above
// - 1 level of indentation
// - ugly
switch answer {
case 19: {
    printf("lucky");
    break;
}
case 21: {
    printf("you stoopid");
    break;
}
case 42: {
    printf("that's the right answer");
    break;
}
default: {
    printf("too bad");
}
}
```

### compared to Rust

```rust
// regular if:
// - 1 level of indentation
// - two keywords: `if`, `else`
if answer == 19 {
    print!("lucky");
} else if answer == 21 {
    print!("you stoopid");
} else if answer == 42 {
    print!("that's the right answer");
} else {
    print!("too bad");
}

// match:
// - 2 levels of indentation
// - one keyword plus the two from before: `if`, `else`, `match`
// - different semantics
//  - allows for pattern matching
//  - requires commas in single statements
// - comparable amount of code, less in many cases
// - requires a lot of structural change and refactoring
match answer {
    19 => {
        print!("lucky");
    },
    21 => {
        print!("you stoopid");
    },
    42 => {
        print!("that's the right answer");
    },
    _ => {
        print!("too bad");
    },
}

// let else:
// - short and concise
let Ok(ok) = answer else {
    // oh no! how do i access the error value? which is exactly what an else case should allow for
    print!("{???}");
    return ???;
};

// i need to completely refactor to this:
// - repetition of `if let`
// - complete change to code structure
//  - need to extract ok to a separate variable
let ok = if let Ok(ok) = answer {
    ok
} else if let Err(err) = answer {
    print!("{err}");
    return err;
};

// or to this:
// - same as above
let ok = match answer {
    Ok(ok) => ok,
    Err(err) => {
        print!("{err}");
        return err;
    },
};
```

## 0.7.0 - Operators

>[!NOTE]
> all of these operators could just be functions thanks to "operator" overloading with identifier
> strings

- boolean operators:

    | operator | shortcircuting | non-shortcircuiting | bitwise |
    | :------- | :------------: | :-----------------: | :-----: |
    | and      |      `&&`      |        `&&&`        |   `&`   |
    | or       |     `\|\|`     |      `\|\|\|`       |  `\|`   |
    | xor      |       NA       |        `^^^`        |   `^`   |

- checked (`++`, `--`, `//`, ..., or `+?`, `-?`, `/?`, ...):
    - overflow/underflow may return both the result and the overflow of the addition
    - division will return either the result or an error value
- unchecked, skip safety checks, maybe using the '?' or the '!' suffix:
    - `<<?`, `<<<?`, `>>>?`, or `<<!`, `>>!`, `<<<!`, `>>>!` -> skip the check for a positive 6bit shift amount
    - `**?` or `**!` -> skip the check for a neagtive power
- divmod:

    ```kay
    let division, remainder = 3 /% 2; # will result in 1, 1
    ```

- boolean flip operator `=!`:
    `boolean = !boolean;` -> `boolean =!;`

### Revised remainder/mod operators

| strategy                    | symbol | math equation                                                          |
| :-------------------------- | :----: | :--------------------------------------------------------------------- |
| **with truncation**         |  `%`   | `remainder = dividend - divisor * trunc(dividend / divisor)`           |
| **with floor**              |  `%-`  | `remainder = dividend - divisor * floor(dividend / divisor)`           |
| **with ceil**               |  `%+`  | `remainder = dividend - divisor * ceil(dividend / divisor)`            |
| **with round**              |  `%*`  | `remainder = dividend - divisor * round(dividend / divisor)`           |
| **with euclidian division** |  `%%`  | `remainder = dividend - abs(divisor) * floor(dividend / abs(divisor))` |

### Revised shift operator

| strategy                     | symbol | x86-64 instruction |
| :--------------------------- | :----: | :----------------- |
| **left logical shift**       |  `<<`  | `shl` / `shlx`     |
| **left arithmetical shift**  |  `<<`  | `sal`              |
| **right logical shift**      |  `>>`  | `shr` / `shrx`     |
| **right arithmetical shift** | `>>-`  | `sar` / `sarx`     |

## 0.7.0 - Labels on blocks

```kay
# possible label syntax
let x = loop:loop_label {
    loop {
        break;
    }
    loop {
        let i = if:if_label condition {
            # forcing breaks to "return" values from blocks and loops
            break:if_label 12;
        } else {
            break:if_label 21;
        }

        let x = :scope_label {
            break:scope_label 12;
        }
        
        # specialized syntax
        let i = if condition {
            # forcing breaks to "return" values from blocks and loops
            break:if 12;
        } else {
            break:if 21;
        }

        let x = loop {
            if condition {
                break:loop i; # breaks from direct loop parent
                break:if i; # breaks from direct if parent
            }
        }
        break:loop_label x;
    }
}
```

```kay
# no semicolon required when assigning the result of a block to a variable
let i = {
    ... # other computation
    break 21;
}

# i know why the semicolon is required, but i find that annoying in rust
var i = ...;
... # other computation
{
    i = 21;
}

# i go to refactor
let i = {
    ... # other computation
    break 21;
} # if the block is long i don't like having to go to the end of the block to add a semicolon
```

compared to rust:

```rust
'label: loop ... {
    loop {
        break;
    }
    loop {
        loop {
            break 'label;
        }
    }
    ...
}

let x = 'label: loop ... {
    ...
    break 'label 12;
};
let x = 'label: {
    ...
    break 'label 21;
};
```

## ?.?.? - String and character literals

- multiline strings are prefixed by a `m`
- may follow C-style string concatenation with some safety modifications:
    - literal string concatenation would only be allowed with a `+`:

    ```kay
    let long_string_literal = "long long long"
        + "long long long"
        + "long long long"
        + "long long string";

    let long_string = "long long long"
        + string_variable
        + "other literal"; # Error: would not be allowed
    ```

- lines will have newline characters appended to them unless they end in a `\`, which can be escaped using a `\\`
- like in Java, whitespace will be preserved (except before the closing quote) and leading whitespace is calculated based on the
    position of the closing quote, or by the text furthest to the left:

    ```java
    String s = """
        multiline
    """
    ```

    ```kay
    let s = m"
        multiline
        string
    "
    ```

- options can appear in any order right before the opening quote, but only once:
    - `frm""`, `fr""`, `rm""` are valid
    - `frrm"`, `ff ""`, `r ""` are not valid

### Character types

| type name  | type    | type size (bytes) | example  | notes                                 |
| :--------- | :------ | :---------------- | :------- | :------------------------------------ |
| ascii char | `ascii` | 1                 | `'h'`    | guaranteed to be valid ascii and utf8 |
| utf32 char | `utf32` | 4                 | `u32'è'` | guaranteed to be valid utf32          |

### String types

| type name    | type                                  | pointer type       | size per character/code point | example      | notes                                 |
| :----------- | :------------------------------------ | :----------------- | :---------------------------- | :----------- | :------------------------------------ |
| ascii string | `str`                                 | `ascii*`           | 1 \* len                      | `"hello"`    | guaranteed to be valid ascii and utf8 |
| utf8 string  | `utf8str` or `u8str` or `str_utf8`    | `u8*` or `utf8*`   | 1 to 4 \* len                 | `u8"hellò"`  | guaranteed to be valid utf8           |
| utf16 string | `utf16str` or `u16str` or `str_utf16` | `u16*` or `utf16*` | 2 or 4 \* len                 | `u16"hellò"` | guaranteed to be valid utf16          |
| utf32 string | `utf32str` or `u32str` or `str_utf32` | `utf32*`           | 4 \* len                      | `u32"hellò"` | guaranteed to be valid utf32          |

- utf8str/utf16str indexing, since characters might be more than one byte long, indexing doesn't
    work, i.e. `string[12]` might land in the middle of a multibyte character, so we could introduce
    rounding indexing (syntax subject to discussion):
    - ceil indexing: `string[+:12]` or `string.at_or_next(12)`, would mean that if the index lands on a non starting byte, it
        would find the next character and return that or `none` if out of bounds
    - floor indexing: `string[-:12]` or `string.at_or_previous(12)`, would mean that if the index lands on a non starting byte, it
        would find the previous character and return that or `none` if out of bounds
    - checked indexing: `string[?:12]` or `string.at_or_none(12)`, would mean that if the index lands on a non starting byte, it
        would return a `none` value, else the value of the character
    - unchecked indexing: `string[!:12]` or `string.at_byte(12)`, would just return the byte at index 12
    - regular indexing: `string[12]` or `string.at(12)`, would mean that if the index lands on a non starting byte, it
        would crash

    might have a general function that returns an enum with the possible cases:

    ```kay
    enum UtfIndex(
        Character(...)
        NonContinuation(...)
        None
    )
    ```

## ?.?.? - Arrays

stack-allocated collection of a compile time known fixed amount of items:

```kay
# initial capacity cannot be specified from variables
let capacity = 19;
let code: i64[capacity]; # error

# unless we introduce compile-time constants
const capacity = 19;
let code: i64[capacity]; # works

# initialized arrays could opt not to specify their lengths, it will get inferred where possible
let codes: i64[] = [1, 2, 3]; # will be of length 3

# or (need to decide wether to keep these syntaxes and only allow to specify the type after the colon)
let codes = i64[1, 2 ,3];                   # array of 3 items with indexes 0, 1 and 2 initialized to 1, 2, 3
let codes = i64[3: 1, 2, 3];                # array of 6 items with indexes 0, 1 and 2 initialized to 1, 2, 3
# will borrow useful features from C like indexed initialization:
let codes: i64[19] = [
    2 = 5, # element at index 2 will contain the value 5
    0 = 9,
    4, # Error: cannot specify positional element after index element
    3..18 = 3, # items from index 3 to index 18 will contain the value 3
    42 = 7, # Error: out of bounds
];
let codes = i64[6: 1 = 1, 3 = 2, 0 = 3, .. = 12];    # array of 6 items with indexes 1, 3 and 0 initialized to 1, 2, 3 and everything else initialized to 12
let codes = i64[6: 1 = 1, 3 = 2, 0 = 3, .. = ---];    # array of 6 items with indexes 1, 3 and 0 initialized to 1, 2, 3 and everything left uninitialized

```

### Arrays of bits

boolean values just need 1 bit to store all possible states (true: 1, false: 0), hence a single
`bool` (8 bits) wastes 7 bits. A `bool[n]` would waste `7 * n` bits, thus a solution maybe of
storing arrays of booleans as a `u8[ceil(n / 8)]` and packing the information of 8 booleans into a
single `u8`, or alternatively with the `bit[n]` -> `bit[8 * n + e]` -> `u8[n + ceil(e / 8)]`, so:

- `bit[7]` -> `u8` and only use 7 out 8 bits
- `bit[8]` -> `u8` and use 8 bits
- `bit[9]` -> `u8[2]` and use 8 bits of the first element and only 1 out of 8 of the second
- `bit[32]` -> `u8[4]` ...
- `bit[33]` -> `u8[5]` ...
- `bit[8 * n]` -> `u8[n]`

## ?.?.? - Dynamic array (Lists) (could just be a "user" defined type, like rust's Vec)

heap-allocated collections of a possibly unknown amount of items:

```kay
# the question mark denotes a dynamic array, or a list
# the initial capacity of the list will be set to some amount (e.g. 4/8/16) for performance
let codes: i64[..];

# an optional initial capacity can be specified
let codes: i64[19..]; # for consistency with initializing some members
let codes: i64[19..: 1 = 0, 3 = 5]; # for consistency with initializing some members in arrays

# initial capacity can be specified from variables
let capacity = 19;
let code: i64[capacity..]
```

they can be manipulated in different ways (syntax yet to be dicided):
(need to decide if these will give an error if the specified index is out of bounds,
maybe have unchecked and checked versions)

```kay
codes.append(3); # adding an element to the end
codes.pop();

codes.insert(2, 4) # inserting an element at index 2

codes.remove(3); # removing at index 3
```

## ?.?.? - Type unions

ability to create a type with a "tag" discriminating which type is currently active

```kay
type i64_or_bool = i64 | bool;

let x: i64_or_bool = 1;

if x is i64 {
    # x type is now inferred as i64
} else {
    # x type is now inferred as bool
}

let y: i64 | bool = true; # type unions can also be implicit
```

type unions can be used with if-let expressions:

```kay
let s0 = "franco";
let s1 = "giovasanni";
let s2 = "aldo";

let s3 = "franco";
let s4 = "giovanni";
let s5 = "aldo";

let b = ["hello", "from", "kay"];
let a = ["hello", "from", "stefano"];

if array_eq(a, b)
let mismatch: none { println("equals"); } # would not be reached since there was a mismatch
let mismatch: u64 { println(f"mismatch at index {mismatch}"); } # mismatch would have the value of 2

if array_eq(a, b)
case let mismatch: none { println("equals"); } # would not be reached since there was a mismatch
case let mismatch: u64 { println(f"mismatch at index {mismatch}"); } # mismatch would have the value of 2
else { ... } # unreachable branch: all variants have been matched

# T[*: N] means just the pointer part of the array 
fn mismatch_index: u64 | none = array_eq[T: type, N: u64](dst: T[*: N], src: T[*: N]) {
    loop var i = N; i > 0; i -= 1 {
        if dst* != src* {
            return i;
        }

        # incrementing the pointer based on the pointer size
        # so a pointer to an array would get incremented by the size of a single element
        dst += 1;
        src += 1;
    }

    return none;
}
```

## ?.?.? - Structs

structs are just an aggregation of types, basically named heterogeneous arrays:

```kay
# using round brackets instead of curly brackets for consistency with function definitions and calls
struct Rgb(
    r: u8,          # type specific default inizialization, which for u8 is 0
    g: u8 = 255,    # explicit default initialization
    b: u8 = ?,      # intentionally uninitialized member, may contain garbage
    b: u8 = ...,    # intentionally uninitialized member, may contain garbage
    b: u8 = ---,    # intentionally uninitialized member, may contain garbage
            # optional trailing coma
)

# named arguments, just like functions
let rgb = Rgb(r = 255, g = 255, b = 255);

# or specifying the arguments in order
let rgb = Rgb(255, 255, 255);

# will raise an error, since `r` is not marked as having a default value
let rgb = Rgb(g = 255);

# `b` initialized to possibly garbage values
let rgb = Rgb(r = 255, g = 255);
```

### method functions

say we now create a constructor function:

- rust-like:

    ```kay
    impl Rgb {
        # associated function or java's "static" method
        # marked as @constructor to allow modifications to fields that can only be set during construction
        @constructor fn Self = new(...) { ... }

        fn self.do_stuff(...) { ... }
    }

- new ideas (mainly to avoid having an additional indentation coming from the impl block):

    - impl markers:

        ```kay
        impl Rgb; # from this point onwards every function is a function related to Rgb

        # Rust-like associated function of Rgb
        fn function_of_Rgb(...) { ... }

        # method function of Rgb
        fn method_of_Rgb(self: Self, ...) { ... }

        impl; # would reset function defintion to being normal functions

        fn regular_function(...) { ... }

        impl Foo; # from this point onwards every function is a function related to Foo
        ...

        impl Rgb; # can reopen implementations
        ...

        impl Bar; # now related to Bar
        ...
        ```

    - function markers:

        ```kay
        fn Self = Rgb.new(...) { ... }

        # first argument is of type Self, meaning this is a method of a variable of type Rgb
        # the name of the first parameter could be anything, unlike Rust
        fn Rgb.do_stuff(self, ...) { ... }

        fn Rgb.do_stuff(rgb, ...) { ... }

        # no need to convert from curly brackets to round brackets, but could need to use the `struct`
        # keyword to avoid colliding with a possible function named `Rgb`
        # "equivalent" constructor function, has no access to fields that can only be set inside the struct constructor
        fn Rgb = Rgb(r: u8, g: u8, b: u8) {
            return struct Rgb(r, g, b);
        }

        let rgb = Rgb(r = 0, g = 0, b = 0);         # this will call a function named `Rgb`
        let rgb = struct Rgb(r = 0, g = 0, b = 0);  # this will call the struct constructor for `Rgb`
        let rgb = Rgb { r = 0, g = 0, b = 0 };      # traditional way of calling the struct constructor for `Rgb`
        let rgb = Rgb.new(r = 0, g = 0, b = 0);     # this will call the function `Rgb.new`

        rgb.do_stuff();
        ```

#### comparison to rust

```rust
struct Rgb {
    r: u8,
    g: u8,
    b: u8,
}

impl Rgb {
    fn new(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }
}

// with the struct initialization syntax
// - can use named 'arguments'
let rgb = Rgb { r: 255, g: 255, b: 255 }

// with the 'constructor' function
// - complete syntax change
// - had to convert curly brackets to round brackets
// - lost ability to use named arguments
let rgb = Rgb::new(255, 255, 255);
```

### Asymetric fields visibility and read/write privileges

ability to specify who can read and write a struct field

```kay
# struct methods and functions always have read access to every kind of field
# may enforce the usage of `let` and `var`
struct Foo(
    # public read: no
    # public write: no
    # private read: yes
    # private write: no
    #
    # only set during construction, never able to be modified again
    private let x0: i64,
    let x0: i64, # implies private
    x0: i64, # implies private let

    # public read: no
    # public write: no
    # private read: yes
    # private write: yes
    #
    # can be modified inside struct method and functions
    private var x0: i64,
    var x0: i64, # implies private

    # public read: yes
    # public write: no
    # private read: yes
    # private write: no
    #
    # only set during construction, never able to be modified again
    # can be read from outside
    public let private let x0: i64,
    public let x0: i64, # implies private let
    public x0: i64, # implies public let and private let

    # public read: yes
    # public write: no
    # private read: yes
    # private write: yes
    #
    # can be modified inside struct method and functions, but only read from outside
    public let private var x0: i64,
    public let x0: i64, # implies private let
    public x0: i64, # implies public let and private let

    # public read: yes
    # public write: yes
    # private read: yes
    # private write: no
    #
    # disallowed: public var disagrees with private let, makes no sense being able to be modified
    # outside of the struct methods and functions and not inside
    public var private let x0: i64,

    # public read: yes
    # public write: yes
    # private read: yes
    # private write: yes
    #
    # can be accessed and modified from everywhere
    public var private var x0: i64,
    public var x0: i64, # implies private var
)
```

blanket modifiers:

```kay
# every field follows it's declaration modifiers
var foo = struct Foo(x0 = 0);

# every field is immutable, read access to fields are not changed
let foo = struct Foo(x0 = 0);
```

### Nameless/temporary/Rust-like tuple structs

basically structs whith unnamed fields (referred to by index)

named tuples:

```kay
struct Point(i64, i64);

let point = Point(19, 21);

let x = point.0; # Point { 19, 21 }
                 #         ^^  ^^
                 # index:  0   1

let y = point.1;
```

name-less tuples:

```kay
let stefano: struct(str, i64) = struct("stefano", 23);

# type can be omitted and therefore inferred
let stefano = struct("stefano", 23);

let name = stefano.0;
let age = stefano.1;
```

or with explicit struct keyword and named fields:

```kay
let range: struct(min: i64, max: i64) = struct(1, 2);
println range.min;
println range.max;
```

### Inheritance

inheritance is just syntactic sugar, this allows for any extended type to be passed as "base" type only carrying
the fields defined in the base type:

```kay
struct Rgba(
    rgb: using Rgb,

    # these fields (of the used Rgb struct are implicitly added)
    # r: u8,
    # g: u8,
    # b: u8,

    a: u8,
)

# the above type is equivalent to:
struct Rgba(
    union(
        rgb: Rgb,
        struct(
            r: u8,
            g: u8,
            b: u8,
        )
    )

    a: u8,
)

# 'using' the same struct multiple times is not allowed
struct Rgba(
    rgb: using Rgb,
    rgb2: using Rgb, # not allowed
    a: u8
)

# but 'using' multiple different struct is
struct Point(
    x: i64,
    y: i64,
)

struct Pixel(
    rgba: using Rgba,
    position: using Point,
)

# which is equivalent to
struct Pixel(
    union(
        rgba: Rgba,
        struct(
            union(
                rgb: Rgba,
                struct(
                    r: u8,
                    g: u8,
                    b: u8,
                ),
            ),
            a: u8,
        )
    ),
    union(
        position: Point,
        struct(
            x: i64,
            y: i64,
        ),
    ),
)

let rgb = Rgb(r = 255, g = 255, b = 255);

# this
let rgba: Rgba = rgb;

# is equivalent to:
let rgba = Rgba(r = rgb.r, g = rgb.g, b = rgb.b, a = 0);

# otherwise to:
let rgba = Rgba(rgb = rgb, a = 0);

# or to:
let rgba = Rgba(
    rgb, # field with same name shorthand
    a = 0,
);
```

if we have a function defined for the "base" struct only the "base" part of the struct will be passed:

```kay
# so this
function_for_Rgb(rgba);

# is desugared to
function_for_Rgb(rgba.rgb);

# and
function_for_rgb(pixel);

# is desugared to
function_for_Rgb(pixel.rgba.rgb);
```

if we dont explicity extend inside a struct it's going to result in an error

```kay
struct Rgb(
    r: u8,
    g: u8,
    a: u8,
)

struct Rgba(
    rgb: Rgb, # no explicit "using"
    a: u8,
)

function_for_Rgb(rgba.rgb); # works
function_for_Rgb(rgba); # doesn't work
```

## ?.?.? - Enums

collection of constant values:

```kay
enum Colors: u32( # optional data type
    # default value for when converting from u32s that don't match the actual enum value
    # for example converting from 0x00ff00 will result in GREEN being chosen
    # when converting from 0x00beef will result in RED being chosen or the returning of an error
    default RED = 0xff0000,
    GREEN = 0x00ff00,
    BLUE = 0x0000ff,
)
```

## ?.?.? - Unions

C-like unions:

```kay
union Rgba(
    struct(
        r: u8,
        g: u8,
        b: u8,
        a: u8,
    )

    rgba: u32,
)
```

## ?.?.? - Enum unions

Rust-like collection of variants:

```kay
enum union Statement: u8( # optional discriminant type
    Empty,
    Single(Node),
    Multiple(Node[]),
)
```

## ?.?.? - struct/enum/variable memory layout/info

### Variables layout

Having a variable such as:

```kay
# at: file.kay:12:0
let name: str = "Stefano";
```

getting the variable layout could be done with the command `kay layout name file.kay:12:0`, which
could output the following valid kay code result:

```kay
# size = 16, align = 8
let name: str = "Stefano";
```

### Structs layout

Having a struct such as:

```kay
struct Foo(
    x: i64,
    y: ascii,
    z: str,
)
```

getting the struct layout could be done with the command `kay layout Foo`, which could output the
following valid kay code result:

```kay
# size = 32, align = 8
struct Foo(
    x: i64,   # size = 8,  offset = 0,  align = 8 -> 0:  |#|#|#|#|#|#|#|#|
    y: ascii, # size = 1,  offset = 8,  align = 1 -> 8:  |#| | | | | | | |
    z: str,   # size = 16, offset = 16, align = 8 -> 16: |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|
)
```

could also emit warnings when wasting space, so a struct such as:

```kay
# at: file.kay:12:0

# size = 40, align = 8
struct Foo(
    a: ascii, # size = 1,  align = 1, offset = 0:  |#| | | | | | | |
    x: i64,   # size = 8,  align = 8, offset = 8:  |#|#|#|#|#|#|#|#|
    y: ascii, # size = 1,  align = 1, offset = 16: |#| | | | | | | |
    z: str,   # size = 16, align = 8, offset = 24: |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|
)
```

would produce the following warnign message

```text
Warning: struct has unoptimal field layout
 at: file.kay:12:0
   |
11 | # size = 40, align = 8
12 | struct Foo(
13 |        a: ascii, # size = 1,  align = 1,offset = 0:  |#| | | | | | | |
   |        ^ this field occupies only 1 byte
   |
14 |        x: i64,   # size = 8,  align = 8, offset = 8:  |#|#|#|#|#|#|#|#|
15 |        y: ascii, # size = 1,  align = 1, offset = 16: |#| | | | | | | |
   |        ^ this field also occupies only 1 byte, but is separate from the previous
   |
16 |        z: str,   # size = 16, align = 8, offset = 24: |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|
17 | )
   |
Help: an optimized layout could look like this
   |
11 | # size = 32, align = 8
12 | struct Foo(
13 |        a: ascii, # size = 1,  align = 1, offset = 0:  |#|_| | | | | | |
14 |        y: ascii, # size = 1,  align = 1, offset = 1:  |_|#| | | | | | |
   |        ^ this field is placed next to the previous one, thus not wasting space
   |
15 |        x: i64,   # size = 8,  align = 8, offset = 8:  |#|#|#|#|#|#|#|#|
16 |        z: str,   # size = 16, align = 8, offset = 16: |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|
17 | )
   |
Note: a packed layout could look like this
   |
11 | # size = 26, align = 1
12 | @packed struct Foo(
13 |        x: i64,   # size = 8,  align = 8, offset = 8:  |#|#|#|#|#|#|#|#|
14 |        z: str,   # size = 16, align = 8, offset = 16: |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|
15 |        a: ascii, # size = 1,  align = 1, offset = 24: |#|_|
16 |        y: ascii, # size = 1,  align = 1, offset = 25: |_|#|
   |        ^ these fields are placed last, thus not wasting space
17 | )
   |
```

a `___` could be a padding member, meaning retaining the usual padding amount:

>[!NOTE]
> this `___` field is equivalent to a `u8[N]`, basically just empty bytes

```kay
# size = 26, align = 1
@packed struct Foo(
    a: ascii, # size = 1,  align = 1, offset = 0:  |#|_|_|_|_|_|_|_|_|
    x: i64,   # size = 8,  align = 8, offset = 1:  |_|#|#|#|#|#|#|#|#|
    y: ascii, # size = 1,  align = 1, offset = 9:  |#|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
    z: str,   # size = 16, align = 8, offset = 10: |_|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|
)
```

could be use as:

```kay
# size = 33, align = 1
@packed struct Foo(
    a: ascii, # size = 1,  align = 1, offset = 0:  |#|_|_|_|_|_|_|_|
    ___,      # size = 7,  align = 1, offset = 1:  |_|#|#|#|#|#|#|#|
    x: i64,   # size = 8,  align = 8, offset = 8:  |#|#|#|#|#|#|#|#|
    y: ascii, # size = 1,  align = 1, offset = 16: |#|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
    z: str,   # size = 16, align = 8, offset = 17: |_|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|
)
```

## ?.?.? - Pointers

pointers are going to come in different flavours (introducing `none` keyword):

```kay
let answer = 42;

let pointer: i64*; # owned pointer, pointing to owned memory (will free the memory it owns when going out of scope or something)
let reference: i64&; # borrowed pointer, pointing to non-owned memory (will possibly support lifetimes)

# avery pointer type can be created with the same syntax
pointer = &answer;
reference = &answer;

let dereferenced: i64;

# checking for none is enforced by the compiler
if reference != none {
    # after this point the compiler knows that "reference" is not none and can safely dereference
    dereferenced = *reference;
}
# after this point the compiler can't guarantee that "reference" is not none, so from now on it's again mandatory to check for null

# or you can forcefully dereference (say for example if you for sure know the pointer is valid), crashing in case of a null pointer
dereferenced = ^reference;
```

## ?.?.? - Index pointers

basically just 'type safe' indexes with semantics roughly similar to pointers and borrow checking

```kay
# imagine there being different kinds of integers: u8, u16, u32, u64, u64

let some_array: i64[3] = [1, 2, 3];
# would basically get the value of the index between brackets, syntax is similar to regular pointers
let index_pointer: i64&<u8, some_array ## can specify to what this index refers to ##> = &some_array[0];
let index_pointer: i64&<u8 ## or it can be inferred from the right hand side of the assignment ##> = &some_array[0];

# would basically be syntactic sugar for
let index_pointer: u8 = 0;

# or with inference
let index_pointer = &<u8>some_array[0];

# if the array has a known length bigger that the index pointer size it would result in an error
let some_array: i64[257] = [...];
let index_pointer: i64&<u8> = &some_array[0]; # Error: index type is too small to index into all array items

# would need no bounds checking since bounds checking has already been performed during index pointer definition
let first_item = some_array[index_pointer];
```

index pointer should be treated differently than regular pointers

```kay
let list: i64[3..] = [1, 2, 3]; # growable array
# indexes of width smaller that the list's length are allowed since length is not known at compile
# time, hence its the programmer's responsibility to make sure to have the proper index type,
# thus this u8 index pointer can only reach the first 255 items of the list
let list_index_pointer = &<u8>list[0];
let list_pointer = &list[0];

fn append(list: i64[..], item: i64) {
    # append operation only adds items to the end of the list:
    # - does not invalidate previously created indexes
    # - it may invalidate regular pointers if the list were to reallocate
    ...
}

# could create attributes to signal possible indexs invalidation of the specified list
fn i64 = pop(@invalidates_indexes list: i64[..]&var) {
    # pop operation only removes from the end of the list:
    # - may invalidate indexe poitners that pointed to the end of the list
    # - may invalidate regular pointers that pointed to the end of the list
    ...
}

let last_element_index = &<u8>list[len list - 1];
let last_element = pop(&var list); # Error: cannot pop, it would invalidate index 'last_element_indexe'

# example usage
fn &i64 = get(list: &var i64[..], index: i64&<u8, list>) { ... }
```

## ?.?.? - Optional types (nullable pointers)

types that may or may not contain a value (introducing the `none` keyword/value):
they are basically tagged unions in the case of non-pointer variables (like Rust's Options)

```kay
# nullable pointers are just "optional pointers"
let nullable: i64*?;
let nullable: i64&?;
let nullable: i64& | none;

let option: i64? = 42; # this will create a variable that has a value
let option: i64? = none; # this will create a variable that doesn't have a value
let option: i64 | none = none; # this will create a variable that doesn't have a value

let maybe: i64?;

# checking for none is enforced by the compiler
if option != none {
    # after this point the compiler knows that "option" is not none and can safely dereference
    maybe = option*; # dereferencing like pointers;
    maybe = *option; # dereferencing like pointers;
}
# after this point the compiler can't guarantee that "option" is not none, so from now on it's again mandatory to check for none

# or you can forcefully dereference, crashing in case of a none
maybe = option^;
maybe = ^option; # or like this
```

### Optionals/Error

maybe this is not useful, could be implemented as a type union to reduce the language complexity:

```kay
# optionals
let optional_i64: i64?;
let optional_i64: i64 | none;

# or
type Option<T> = T | none;

# or
enum Option<T>(
    Some(T),
    None,
)

# thus
let optional_i64_in_rust: Option<i64>;

# errors
let i64_or_error: i64!SomeError;
let i64_or_error: i64 | SomeError;
let i64_or_i64_error: i64 | i64; # would need to find a way to express this

# or "force" the user to find better naming (create a distinct i64 error type or alias)
type i64_error = i64;
alias i64_error = i64;
let i64_or_i64_error: i64 | i64_error;

# or to avoid creating a lot of "new" error types
enum Result<T, E>(
    Ok(T),
    Err(E),
)

# thus
let i64_or_i64_error: Result<i64, i64>;

# or create temporary distinct types (syntax subject to discussion)
# this could introduce inline type aliases
# so a function could use them like
fn result: i64 as ok | i64 as err = foo(i: i64) {
    if i
    case 0 { return 1 as err; }
    case 12 { return 21 as err; }
    case 21 { return 42 as ok; }
}

# so to match on it would look like this
let result = foo(i);
if result
case let integer: ok {
    # integer is of type `i64`
} case let err_code: err {
    # integer is of type `i64` as well
}

# different syntaxes
let i64_or_i64_error: i64 | err ! i64;
let i64_or_i64_error: i64 | i64 ! err;
let i64_or_i64_error: i64 | err: i64;
let i64_or_i64_error: i64 | i64 alias err;
let i64_or_i64_error: i64 | err alias i64;

# or a manual implementation, akin to typescript
type Result<T, E> = {
    success = true,
    data: T,
} | {
    success: false, # or using a value as a type
    err: E,
}

# which would allow for manual optimizations
type c_like_i64_return =
    None {
        error := -1, # or with a special `value as type syntax`
    } | Some {
        data := 0..,
    }
```

or a `type enum`

``` kay
type enum i64_or_bool(
    integer: i64,
    boolean: bool,
)

type enum i64_or_error_code(
    integer: i64,
    error_code: i64,
)

type enum Option<T>(
    Some: T,
    None, # empty value
)

type enum Result<T, E>(
    Ok: T,
    Err: E,
)

# so to match on it would look like this
let result = i64_or_error_code.integer(1);
if result
case let i64_or_error_code.integer(integer) {
    # `integer` is of type `i64`
} case let i64_or_error_code.err_code(code) {
    # `code` is of type `i64` as well
}

let result: Result<i64, bool> = Result.Ok(1);
if result
case let Result.Ok(integer) {
    # `integer` is of type `i64`
} case let Result.Err(err) {
    # `err` is of type `bool`
}

# or
let result: Result<i64, bool> = Result.Ok(1);
if result
case let integer: Result.Ok {
    # `integer` is of type `i64`
} case let err: Result.Err {
    # `err` is of type `bool`
}

# inline type enum
let i64_or_bool: type enum(file: File, err: ReadFileError);
# compared to what was discussed above
let i64_or_bool: File | ReadFileError;
```

or remove type unions altogether and treat enum as type unions

```kay
# this
type enum Result<T, E>(
    Ok: T,
    Err: E,
)

# would become
enum Result<T, E>(
    Ok(T),
    Err(E),
)

# which would solve type collisions, but would be more verbose
enum integer_or_error_code(
    Integer(i64),
    ErrorCode(i64),
)

# would solve this
let i64_or_i64_error: i64 | err: i64;
type i64_or_i64_error = i64 | err: i64;
```

## ?.?.? - casts

ability to define/overload the casting operator for specific types.
types with explicit conversions can be bit-casted to other types when possible
basically defining different interpretations of the same data

```kay
struct Rgba like u32(
    r: u8,
    g: u8,
    b: u8,
    a: u8,
)

# or (would be more consistent with regular as conversions, e.g.: true as i64)
struct Rgba as u32(
    r: u8,
    g: u8,
    b: u8,
    a: u8,
)

# or
struct Rgba alias u32(
    r: u8,
    g: u8,
    b: u8,
    a: u8,
)

# or (would be more consistent with variable type hints)
struct Rgba: u32(
    r: u8,
    g: u8,
    b: u8,
    a: u8,
)

# basically equivalent to, could also be the default to avoid extra language complexity
union Rgba(
    rgba: u32,
    struct(
        r: u8,
        g: u8,
        b: u8,
        a: u8,
    )
)

# this would result in a type size mismatch, or in some other constrait (need to be defined) being broken
struct Rgba like u8(
    r: u8,
    g: u8,
    b: u8,
    a: u8,
)
```

when bit casts are used inside expressions they incour in no performance penalty, as the compiler would just
treat the values are of different types:

```kay
let rgba: Rgba;
# bit-casting should be a nop, in this case just a plain copy or rgba memory
let rgba_u32: u32 = rgba as u32;
let rgba_u32: u32 = rgba alias u32;
let rgba_u32: u32 = rgba cast u32;
let rgba_u32: u32 = rgba view u32;

let red = Rgba { r = 255 };
let green = Rgba { g = 255 };

# the compiler would treat this as Rgba + Rgba
let red_plus_green = red + green;

# while this would be treated as u32 + u32 and no conversion code would be run
let red_plus_green = red as u32 + green as u32;

# so it avoids this
var red_plus_green: Rgba;
red_plus_green.r = red.r + green.r;
red_plus_green.g = red.g + green.g;
red_plus_green.b = red.b + green.b;
red_plus_green.a = red.a + green.b;
```

casts that call conversion functions/builtins that are not just bit reinterpretations:

```kay
struct SomeStruct(...)
struct SomeOtherStruct(...)

impl SomeStruct {
    # member function
    op SomeOtherStruct = cast(self, other: SomeOtherStruct) { ...; return ...; }
    op SomeOtherStruct = into(self, other: SomeOtherStruct) { ...; return ...; }
    op SomeOtherStruct = convert(self, other: SomeOtherStruct) { ...; return ...; }
    op SomeOtherStruct = into(self, other: SomeOtherStruct, other_args: ...) { ...; return ...; }
    op SomeOtherStruct = self into other: SomeOtherStruct ## how do i add other args? ## { ...; return ...; }
}

# freestanding function
op SomeOtherStruct = cast(self: SomeStruct, other: SomeOtherStruct) { ...; return ...; }
```

### **BREAKING**: Bit-casting operator for primitive types and removal of implicit conversions

## 0.7.0 - compile time constants

```kay
const answer = 40 + 2; # would evaluate the constant expression and just copy paste the result everytime
let i = answer; # equivalent to `let i = 42`
```

## ?.?.? - compile time functions excution

```kay
const fn i64 = answer() { return 42 };
let i = const answer(); # equivalent to `let i = { return 42 }` -> `let i = 42`
```

## ?.?.? - experiment with no dynamic dispatch

use unions instead, which have to be checked (kinda like what Casey Muratori explained in
["Clean" Code, Horrible Performance](https://www.youtube.com/watch?v=tD5NrevFtbU)).

dynamically dispatched objects rely on interfaces/traits/concept (whatever name) stipulating that a
type implements a specific function, so the compiler can basically inject the tagged union
representing the polymorfic object and the check for the type of the object by itself.

maybe optionally enable true dynamic dispatch on demand with v-tables and stuff

## ?.?.? - MATLAB-inspired [functions](https://www.mathworks.com/help/matlab/ref/function.html) definitions

```kay
# introductory keyword
fn

# return values
result: i64, remainder: i64

# return values' names are optional
i64, i64

# equals sign to make it easey to copy paste this function definition in code
=

# name of the function
divmod

# function arguments
(dividend: i64, divisor: i64)

# body of the function, can also be in the do single-statement form
{
    # we can name our return values
    return result = dividend / divisor, remainder = dividend % divisor;

    # or not, where return values' names just serve as comments
    return dividend / divisor, dividend % divisor;
}
```

putting it all together:

```kay
# no return values
fn answer() { return 42 };

# with unnamed return values
fn i64, i64 = divmod(dividend: i64, divisor: i64) {
    return dividend / divisor, dividend % divisor;
}

# with named return values (NOTE: naked returns are not going to be allowed)
fn result: i64, remainder: i64 = divmod(dividend: i64, divisor: i64) {
    return result = dividend / divisor, remainder = dividend % divisor;
}
```

going from function definition to usage would look like this

```kay
# function definition
fn result: i64, remainder: i64 = divmod(dividend: i64, divisor: i64) {
    return result = dividend / divisor, remainder = dividend % divisor;
}

# from here onwards we are pretending that each line is the progression of steps needed to go from function definition to the usage

# copy paste the definition
fn result: i64, remainder: i64 = divmod(dividend: i64, divisor: i64)

# change 'fn' to 'let'/'var'
# - explicit mutability qualifiers needed for each variable
let result: i64, var remainder: i64 = divmod(dividend: i64, divisor: i64)

# add a semicolon at the end
let result: i64, var remainder: i64 = divmod(dividend: i64, divisor: i64);

# remove the function arguments' type hints and you are done!
let result: i64, var remainder: i64 = divmod(dividend, divisor);
```

going from usage to function definition would look like this

```kay
let dividend = 3;
let divisor = 2;

# usage
let result: i64, var remainder: i64 = dividend / divisor, dividend % divisor;

# from here onwards we are pretending that each line is the progression of steps needed to go from usage to the function definition

# copy paste the usage
let result: i64, var remainder: i64 = dividend / divisor, dividend % divisor;

# remove 'let'/'var' and add the 'fn' keyword at the start of the line
fn result: i64, remainder: i64 = dividend / divisor, dividend % divisor;

# add the function name and arguments
fn result: i64, remainder: i64 = divmod(dividend: i64, divisor: i64) dividend / divisor, dividend % divisor;

# add the function body, with no named returns
fn result: i64, remainder: i64 = divmod(dividend: i64, divisor: i64) {
    return dividend / divisor, dividend % divisor;
}

# optionally remove named returns
fn i64, i64 = divmod(dividend: i64, divisor: i64) {
    return dividend / divisor, dividend % divisor;
}

# or add them back
fn result: i64, remainder: i64 = divmod(dividend: i64, divisor: i64) {
    return result = dividend / divisor, remainder = dividend % divisor;
}

# and done!
```

### Inline functions

Ability to inline a function at the call site for finer granularity, while still retaining a hint to
the compiler to inline it when it sees fit:

```kay
# marked as inline, the rust inspired "!" avoids extra keywords (could find another symbol)
fn result: i64, remainder: i64 = divmod_inline!(dividend: i64, divisor: i64) {
    return result = dividend / divisor, remainder = dividend % divisor;
}

let result, let remainder = divmod_inline(21, 12); # regular function call would not be allowed, or would emit a warning 
let result, let remainder = divmod_inline!(21, 12); # inline function call syntax would be mandatory
let result, let remainder = divmod_inline: { # would be inlined as this as many times as possible, could emit a warning when inlining could be performed
    let dividend = 21;
    let divisor = 12;
    break divmod_inline: result = dividend / divisor, remainder = dividend % divisor;
}

# not marked as inline
fn result: i64, remainder: i64 = divmod(dividend: i64, divisor: i64) {
    return result = dividend / divisor, remainder = dividend % divisor;
}

let result, let remainder = divmod(21, 12); # regular function call
let result, let remainder = divmod!(21, 12); # inlined at the call site

let foo = foo(...); # no inline
let foo = foo!(...); # inline
let foo = foo!?(...); # let the compiler decide wether to inline

# or
let foo = foo!!(...); # no inline
let foo = foo!(...); # inline
let foo = foo(...); # let the compiler decide wether to inline
```

## ?.?.? - Function overloading

function overloading should follow the function's philosofy of resembling the shape of the usage of
the function:

```kay
# base overload
fn foo[i32](a: i32) { ... }
fn foo[f32](a: f32) { ... }
fn foo[str](a: str) { ... }

# nestable
fn foo[str][i32](a: str, b: i32) { ... }; let a = foo[str][i32]("21", 12);
fn foo[str; i32](a: str, b: i32) { ... }; let a = foo[str; i32]("21", 12);

# base overload
fn bar[str, i32](a: str, b: i32) { ... }; let a = foo[str; i32]("21", 12);

# nested overload
fn bar[str, i32][f32](a: str, b: i32, c: f32) { ... }
fn bar[str, i32][f32](a: str, b: i32, c: f32) { ... }
fn bar[str, i32][i64](a: str, b: i32, c: i64) { ... }
fn bar[str, i32; f32](a: str, b: i32, c: f32) { ... }

# possible type inference
fn bar[*, *; *](a: str, b: i32, c: f32) { ... } # will produce bar[str, i32; f32]
fn bar[*, *; *](a: i64, b: Foo, c: str) { ... } # will produce bar[i64, Foo; str]
fn bar[*; *, *](a: i64, b: Foo, c: str) { ... } # will produce bar[i64; Foo, str]

# explicit full name qualification
let a = bar[str, i32; f32]("21", 12, 19.10); # will call bar[str, i32; f32]
let b = bar[str, i32]("21", 12, 19.10); # will call bar[str, i32; f32]
let b = bar[str, i32]("21", 12, 1910); # will call bar[str, i32; i64]
let b = bar[str, i32]("21", 12); # will call bar[str, i32]

# implicit name qualification, inferred from the arguments types
let b = bar("21", 12, 19.10); # will call bar[str, i32; f32]
let b = bar("21", 12, 1910); # will call bar[str, i32; i64]

# could force calling overloaded functions explicitly or with explicit request for inference
let b = bar[**]("21", 12, 1910); # will infer a call to bar[str, i32; i64]
let b = bar[**; **]("21", 12, 19.10); # will infer a call to bar[str, i32; f32]
let b = bar[str, **; **]("21", 12, 19.10); # will infer a call to bar[str, i32; f32]

# or
let b = bar("21" @ str, 12 @ i32, 19.10 @ f32); # will call bar[str, i32; f32]
let b = bar("21" @ str, 12 @ i32, 19.10 @ i64); # will call bar[str, i32; i64]
let b = bar("21": str, 12: i32, 1910: i64); # will call bar[str, i32; i64]
let b = bar("21": *, 12: *, 1910: *); # will call bar[str, i32; i64]

# overload types integrated into the function arguments
fn foo[a: str] {}
fn foo[a: i32] {}
fn foo[a: str, b: i32] {}
fn foo[a: i32, b: i32] {}
fn foo[a: str, b: i32; c: f32] {}
fn foo[a: str, b: i32][c: f32] {}
fn foo[a: *, b: *; c: f32] {}
fn foo@(a: *, b: *; c: f32) {}
fn foo@(a: *, b: *; c: f32) {}

let a = foo["21", 12; 19.10];
let a = foo@("21", 12; 19.10);

# function pointers
let b = &foo[str, i32; f32]; # use the square bracket syntax to access the "name" of the overload

# could specify alias names
fn foo[str, i64; f32](...) @name(foo_str_i32_f32) { ... }
fn foo[str, i64; f32](...) @foo_str_i32_f32 { ... }
fn foo[str, i64; f32](...) | foo_str_i32_f32 { ... }
fn foo[str, i64; f32](...) alias foo_str_i32_f32 { ... }
fn foo[str, i64; f32] alias foo_str_i32_f32(...) { ... }
fn foo[str, i64; f32] | foo_str_i32_f32(...) { ... }
fn foo[str, i64; f32] | something_totally_arbitrary(...) { ... }
fn foo[str, i64; f32] | something_totally_arbitrary(...) { ... }
fn(foo) something_totally_arbitrary[str, i64, f32](a: str, b: i64, c: f32) { ... }
fn(foo[str, i64, f32]) something_totally_arbitrary(a: str, b: i64, c: f32) { ... }
fn(foo) foo_str_i64_f32(a: str, b: i64, c: f32) { ... }
fn(foo) foo_i64_str_i32(a: i64, b: str, c: i32) { ... }
fn foo @ foo_i64_str_i32(a: i64, b: str, c: i32) { ... }
fn foo alias foo_i64_str_i32(a: i64, b: str, c: i32) { ... }
alias foo = fn foo_i64_str_i32(a: i64, b: str, c: i32) { ... }

fn foo[str, i64; f32](...) { ... }
alias foo_str_i32_f32 = foo[str, i64; f32];

foo("21", 12, 19.10); # would call foo_str_i64_f32
foo(12, "21", 1910); # would call foo_i64_str_i32
foo[i64, str, i32](12, "21", 1910); # would call foo_i64_str_i32
foo_i64_str_i32(12, "21", 1910); # the original name can still be used
something_totally_arbitrary(12, "21", 1910);

# so these would refer to the same function
let f = foo[str, i64; f32](...);
let f = foo_str_i64_f32(...);
let f = foo("21", 12, 19.10);

# alias could work on names too
let f alias g = foo("21", 12, 19.10);
```

### Named arguments (inspired by swift)

```kay
# dividend and divisor would be user facing named arguments
fn result: i64, remainder: i64 = divmod(dividend: i64, divisor: i64) {
    return result = dividend / divisor, remainder = dividend % divisor;
}

fn result: i64, remainder: i64 = divmod(dividend: i64, divisor: i64) {
let result: i64, remainder: i64 = divmod(dividend: i64, divisor: i64) {
let result, remainder = divmod(dividend: i64, divisor: i64) {
let result, remainder = divmod(dividend = 12, divisor = 21) {
let result, remainder = divmod(dividend = 12, divisor = 21);

# a and b would be user facing named arguments
fn result: i64, remainder: i64 = divmod(a = dividend: i64, b = divisor: i64) {
    return result = dividend / divisor, remainder = dividend % divisor;
}

fn result: i64, remainder: i64 = divmod(a = dividend: i64, b = divisor: i64) {
let result: i64, remainder: i64 = divmod(a = dividend: i64, b = divisor: i64) {
let result, remainder = divmod(a = dividend: i64, b = divisor: i64) {
let result, remainder = divmod(a = 12, b = 21) {
let result, remainder = divmod(a = 12, b = 21);
```

### Operator overloading

operator overloading should follow the function's philosofy of resembling the shape of the usage of
the function/operator, so as an example, the definition for the `+` operator might look like this:

- for a more "traditional" style:

    ```kay
    op i64 = +(lhs: i64, rhs: i64) {
        return lhs + rhs;
    }
    ```

- for a closer look to it's usage, but still consistent with normal functions declarations:

    ```kay
    op i64 = (lhs: i64) + (rhs: i64) {
        return lhs + rhs;
    }
    ```

- for an even closer look to it's usage:

    ```kay
    op i64 = lhs: i64 + rhs: i64 {
        return lhs + rhs;
    }
    ```

- with function/operator overloading

    ```kay
    op i64 = [i64, i64] lhs: i64 + rhs: i64 { ... }
    op i32 = [i32, i32] lhs: i32 + rhs: i32 { ... }
    op Foo = [i32, Bar] lhs: i32 + rhs: Bar { ... }
    op Foo = [Bar, i32] lhs: Bar + rhs: Foo { ... }
    op Foo = +[Bar, i32](lhs: Bar, rhs: Foo) { ... }
    op Foo = [Bar + i32](lhs: Bar, rhs: Foo) { ... }
    op i64 = lhs: f32 [+] rhs: f32 { ... }
    op Foo = lhs: Bar [+] rhs: Foo { ... }
    op Foo = lhs: Bar [+] rhs: Foo alias `Bar + Foo` { ... } # could require the usare of aliases

    let i = Bar + Foo; # would call `Bar + Foo`
    let i = Bar [+] Foo; # would call `Bar + Foo`
    ```

might also be able to specify that the function should track the caller's line and column for error
messages:

```kay
@track_caller op i64 = lhs: i64 + rhs: i64 {
    return lhs + rhs;
}
```

so going from usage to function would look like this;

```kay
let lhs = 21;
let rhs = 42;

# 1
let i = lhs + rhs;

# 2
op i = lhs + rhs;

# 3a
op i: i64 = lhs + rhs;

# 3b
op i64 = lhs + rhs;

# 4
op i64 = lhs: i64 + rhs: i64;

# 5
op i64 = lhs: i64 + rhs: i64 {
    return lhs + rhs;
}
```

and back from function to usage would look like this;

```kay
# 1
op i64 = lhs: i64 + rhs: i64 {
    return lhs + rhs;
}

# 2
op i64 = lhs: i64 + rhs: i64;

# 3
op i64 = lhs + rhs;

# 4
let i64 = lhs + rhs;

# 5
let i = lhs + rhs;
```


### Custom operators

identifier strings would allow for "custom" operators

```kay
@track_caller inline op division: i64, remainder: i64 = lhs: i64 `/%` rhs: i64 {
    return lhs / rhs, lhs % rhs;
}

let division, let remainder = 12 `/%` 21;

op i64 = lhs: Vec2 `.` rhs: Vec2 {
    return lhs.x * rhs.x + lhs.y * rhs.y;
}

# or
op i64 = lhs: Vec2 `.*` rhs: Vec2 {
    return lhs.x * rhs.x + lhs.y * rhs.y;
}

# or
fn i64 = `.*`(lhs: Vec2, rhs: Vec2) {
    return lhs.x * rhs.x + lhs.y * rhs.y;
}

let lhs = Vec2(x = 12, y = 21);
let rhs = Vec2(x = 21, y = 12);
let dot_product = lhs `.` rhs;
let dot_product = lhs `.*` rhs;
let dot_product = lhs.`.*`(rhs);
```

### Named operators instead of overloading

```kay
op i64 = lhs: i64 plus rhs: i64 { ... }; let twenty_one = 9 plus 10;
op i64 = lhs: i64 `+` rhs: i64 { ... }; let twenty_one = 9 `+` 10;

# could specify what kind of operator it is
op(infix) i64 = lhs: i64 `+` rhs: i64 { ... }
op(prefix) i64 = `+` lhs: i64 { ... }
op(postfix) i64 = lhs: i64 `*` { ... }

# the "*" inside the parentheses would be a placeholder for the operator
# the types in the shape specification would allow to avoid repeating them in the function signature
op(i64 * i64) i64 = lhs `*` rhs { ... }

# could force explicit usage of the keyword during usage to encode the shape of the operator
op i64 = first: i64 op `+` second: i64 { ... }
op i64 = first: i64 op plus second: i64 { ... }
let twenty_one = 9 op`+` 10;
let twenty_one = 9 op plus 10;

# this could allow for emulation of arbitrary expressions
op i64 = condition: bool op `?` value_if_true: i64 op `:` value_if_false: i64 { ... }
# could allow specifying the name of the underlying function
op i64 = condition: bool op `?` value_if_true: i64 op `:` value_if_false: i64
fn i64 = ternary(condition: bool, value_if_true: i64, value_if_false) { ... }

let twenty_one = true op`?` 21 op`:` 19;
let twenty_one = ternary(true, 21, 19);

# "operator overloading" could be become syntactic sugar for any function
fn i64 = ternary(condition: bool, value_if_true: i64, value_if_false: i64)
op condition op `?` value_if_true op `:` value_if_false { ... }

fn i64 = dot(lhs: Matrix, rhs: Matrix)
op lhs op `.*` rhs { ... }

# this would play nicely with function overloading
fn i64 = ternary[bool; i64, i64](condition: bool, value_if_true: i64, value_if_false: i64)
op condition op `?` value_if_true op `:` value_if_false { ... }

fn Foo = ternary[bool; Foo, Foo](condition: bool, value_if_true: Foo, value_if_false: Foo)
op condition op `?` value_if_true op `:` value_if_false { ... }

# could remove the op keyword entirely
fn i64 = negate(value: i64) -> op`!` value { ... }
fn i64 = unwrap(value: Option<i64>) -> value op`?` { ... }; let f = option op`?`; let f = unwrap(option) # borrowing from rust
fn i64 = dot(lhs: Matrix, rhs: Matrix) -> lhs op `.*` rhs { ... }
fn i64 = dot(lhs: Matrix, rhs: Matrix)
-> lhs op `.*` rhs { ... }

# would need to provide a way to specify precedence
fn i64 = dot(lhs: Matrix, rhs: Matrix) -> lhs op `.*` rhs alias * { ... } # same precedence as the * operator
fn i64 = dot(lhs: Matrix, rhs: Matrix) -> lhs op `.*` rhs | alias * { ... } # same precedence as the * operator
fn i64 = dot(lhs: Matrix, rhs: Matrix) -> lhs op `.*` rhs => * { ... } # same precedence as the * operator
fn i64 = dot(lhs: Matrix, rhs: Matrix) -> lhs op `.*` rhs == * { ... } # same precedence as the * operator
fn i64 = dot(lhs: Matrix, rhs: Matrix) -> lhs op `.*` rhs as * { ... } # same precedence as the * operator
fn i64 = dot(lhs: Matrix, rhs: Matrix) -> lhs op `.*` rhs -> * { ... } # same precedence as the * operator

# "operator overloading" could become this, so this would tell the compiler to add "+[i64, i64]" to
# the overloads for the operator "+", thus inferring the usage from the operator
fn i64 = +[i64, i64](lhs: i64, rhs: i64) { ... }

# casting and conversion functions could look like operators
fn<D, S> D = into(dst: type S) -> dst op into S { ... }
```

## ?.?.? - Better memory layout

```rust
pub(crate) union TokenPayload {
    text: TextIndex,
    bracket: Bracket,
    op: Op,
    ascii: ascii,
    none: (),
}

#[repr(u8)]
pub(crate) enum TokenTag {
    Foo,
    Bar,
    Baz,
}

// saves 3 bytes of padding per token
pub(crate) struct TokensNew {
    tags: Vec<TokenTag>,
    payload: Vec<TokenPayload>,
    col: Vec<offset32>,
}

pub(crate) struct TokenNew {
    tags: TokenTag,
    payload: TokenPayload,
    col: offset32,
}
```

## 0.7.0 - Distinct types and aliases

```kay
let a = 12;
alias b = a; # a and b would be treated as if they were the same entity
alias ascii = u8; # ascii is totally equivalent to u8, basically a form of "name overloading"
# is the same as in C `#define ascii u8`

let a: u8 = 12;
let b: ascii = b; # fine, since ascii is just an other name for u8

type ascii = u8; # ascii is a different type from u8
let a: u8 = 12;
let b: ascii = a; # Error: ascii is a different type from u8
```

## ?.?.? - Capturing scopes

Ability to capture only specific variables from the surrounding environment:

```kay
let outer_0 = ...;
let outer_1 = ...;
{
    let inner_0 = outer_0; # can see outer_0
    let inner_1 = outer_1; # can see outer_1
}
```

```kay
# restrict capturing to only specific entities
let outer_0 = ...;
let outer_1 = ...;

# syntax subject to change
@capture(outer_0) ## only capture outer_0 ## {
    let inner_0 = outer_0; # can see outer_0
    let inner_1 = outer_1; # Error: cannot see outer_1, not specified in the capturing group
}
@capture(outer_0, outer_1) ## only capture outer_0 and outer_1 ## {
    let inner_0 = outer_0; # can see outer_0
    let inner_1 = outer_1; # can see outer_1
}

@capture(!outer_0) ## capture everyting except outer_0 ## {
    let inner_0 = outer_0; # Error: cannot see outer_0, not specified in the capturing group
    let inner_1 = outer_1; # can see outer_1
}
@capture() ## capture nothing ## {
    let inner_0 = outer_0; # Error: cannot see outer_0, not specified in the capturing group
    let inner_1 = outer_1; # Error: cannot see outer_1, not specified in the capturing group
}

# need to implement other ways to specify what to capture
```

could be applied to functions as well (when global variables will be implemented):

```kay
let global_0 = ...;
let global_1 = ...;
fn foo(...) @capture(global_0) {
    let inner_0 = global_0; # can see global_0
    let inner_1 = global_1; # Error: cannot see global_1, not specified in the capturing group
}
```

## ?.?.? - Non-lexical scopes `no_scope`

Would create a distinction between a `scope` and a `no_scope`:

`scope`: every local definition belongs to that scope and is only accessible within it

```kay
let a = ...;
{
    let b = a;
}
let c = b; # Error: not in scope
```

`no_scope`: basically just a way to group statements with no scoping rules

```kay
@no_scope {
    let b = a;
}
# valid, in scope, as the @no_scope directive specifies that the block should not have scoping
# rules applied
let c = b;
```

Would only work on stand alone blocks, control flow and functions cannot be marked as `no_scope`:

```kay
if condition @no_scope { # Error: @no_scope directive cannot be applied to if blocks
    ...
}
```
