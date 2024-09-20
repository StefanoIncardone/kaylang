# Kay Lang

```kay
println "Kay let's go!";
```

Experimenting with programming languages and exploring how to create one.

> [!WARNING]
> The language could change at any moment for now, and could contain many bugs

## Syntem requirements

The language is designed to only **"work on my machine"** for now, meaning it runs on Windows 11
with [WSL](https://learn.microsoft.com/en-us/windows/wsl/install), and so it compiles to an `ELF`
executable file, thus it has been tested only on:

- Ubuntu 22.04.4 LTS
- CPU: Intel i7-8565u
- GPU: Nvidia MX250
- RAM: 16 GB

Therefore it will only compile code down to `x86-64` assembly, and it may work on other similar
machines and combination of components.

## Getting started

The language is written in [Rust](https://www.rust-lang.org/) version 1.80.0 using
[cargo](https://doc.rust-lang.org/cargo/) as the build system.
It also uses the [nasm](https://www.nasm.us/) assembler version 2.15.05 and the
[ld](https://ftp.gnu.org/old-gnu/Manuals/ld-2.9.1/html_mono/ld.html) linker verision 2.38, thus only
requiring the following steps to get started:

1. Checking if Rust and cargo are installed and up to version 1.80.0:

    ```shell
    cargo --version
    ```

    If an error stating that the `cargo` command could not be found occurs, try following the
    [installation guide](https://www.rust-lang.org/tools/install) to install it.

2. Checking if nasm is installed:

    ```shell
    nasm --version
    ```

    If an error stating that the `nasm` command could not be found occurs, try installing the nasm
    assembler:

    ```shell
    sudo apt install nasm
    ```

3. Cloning the repo:

    ```shell
    git clone https://github.com/StefanoIncardone/kaylang
    cd kaylang
    ```

4. Get to know the features of the compiler by taking a look at the provided help message:

    ```shell
    cargo run
    ```

## Syntax specification

See the [full language reference](LANGUAGE_REFERENCE.md) and the the
[full language syntax specification](SYNTAX.ebnf)

## Syntax highlighting

Currently only supporting Vscode with the provided [extension](syntax/vscode/) which can be
installed locally using the `Developer: Install Extension from Location...` command
