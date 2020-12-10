# structured-text
Tools for the IEC 61131-3 structured text (ST) language. See: https://en.wikipedia.org/wiki/IEC_61131-3.

# Build and install (on Ubuntu 20.04)

  1. Install the `haskell-stack` package:
     ```shell
     $ sudo apt install haskell-stack
     ```

  2. Clone this repo:
     ```shell
     $ git clone git@code.ornl.gov:defcon-scada/structured-text.git
     ```

  3. Enter the repo:
     ```shell
     $ cd structured-text
     ```

  4. Build and install:
     ```shell
     $ stack install
     ```
     This step will take a while the first time, as it'll install Haskell and
     all the build dependencies as well.

  5. Add `stack`'s default executable deployment directory to `$PATH`:
     ```shell
     $ export PATH=$HOME/.local/bin:$PATH
     ```
     Also consider adding this to `~/.bashrc`.

# Testing

To run the unit tests:
```shell
$ stack test
```

# Usage

The `stack install` command should build the executable `stxt`. For usage info:
```shell
$ stxt --help
```

