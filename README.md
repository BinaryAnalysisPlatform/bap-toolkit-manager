
# Overview

`bap-report` provides an easy way to run `bap` against variety of artifacts and analysis.
Underneath of the hood `bap-report` is an extra layer for docker containers, so there is no
need to install `bap` manually or to know how what arguments to feed to the docker.

The result of the running of `bap-report` is an html file that could be later opened
in any browser.

## Usage

### Hello, world!

Before going in the deep and describe `bap-report` options, let's just run a
simple example, which is quite descriptive itself
`bap-report --artifacts=/bin/echo --recipe=forbidden-symbol`
and open "results.html" in your favorite browser.

### Other options and examples
The proposed usage is either of the next ways:

- `bap-report --list-recipes` will print a list of recipes with a short decription of each one.

- `bap-report --list-artifacts` will print a list of available artifacts from the
  `binaryanalysisplatform/bap-artifacts` repository.

- `bap-report --artifacts=/bin/echo --recipe=forbidden-symbol`

- `bap-report --artifacts=/bin/echo,wpa_cli-2.2 --recipes=defective-symbol,jpl-rule-14`
  this will generate the file `results.html` with the results of applying
  of recipes `defective-symbol` and `jpl-rule-14`  to the artifacts `/bin/echo` and `wpa_cli-2.2`.
  Note, that the first artifact will be taken from the host machine, while the second
  one will be found in `bap-artifacts` docker repository

- `bap-report --schedule=<path>`
  reads a schedule of tasks from a file at `path`.
  the example of schedule file:
  ```
  (/bin/echo   av-rule-174)
  (wpa_cli-2.2 defective-symbol)
  (/bin/echo   (defective-symbol jpl-rule-14))
  ```
  this will generate the file `results.html`  with the results of applying
  of recipes `av-rule-174`, `defective-symbol` and `jpl-rule-14` to the
  artifact `/bin/echo`
  and of recipe `defective-symmbol` to the artifact `wpa_cli-2.2`.


## Build and install

### System dependencies
- docker
- opam 2.x

### Install in the fresh opam switch
There are many upcoming changes in this repository that will make
the installation easier.

But now one can do the following:

```
$: opam switch create 4.07.1
$: eval `opam config env`
$: opam install core_kernel.v0.11.1 monads cmdliner
$: git clone https://github.com/gitoleg/bap-report
$: cd bap-report
$: make && make install
```
