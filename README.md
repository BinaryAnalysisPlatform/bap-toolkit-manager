
# Overview

`bap-toolkit` provides an easy way to run `bap` against variety of artifacts and analysis.
Underneath of the hood `bap-toolkit` is an extra layer for docker containers, so there is no
need to install `bap` manually or to know what arguments feed to the docker.
Also, `bap-toolkit` has no complex dependecies neither from `bap` nor from other libraries,
that makes it easy to install it itself.

Also `bap-toolkit` provides a friendly way to observe the results of the analysis.
Those, if the analysis produce an incidents file, then an html report will be stored
in the `results.html` file.

## Build and install

### System dependencies
- docker
- opam 2.x

Also, don't forget to grant an access to the docker engine for the current user:
```
$: sudo usermod -a -G docker $USER
```
and re-login after that

### Install in the fresh opam switch
There are many upcoming changes in this repository that will make
the installation easier.

But now one can do the following:

```
$: opam switch create 4.07.1
$: eval `opam config env`
$: opam install core_kernel.v0.11.1 monads cmdliner
$: git clone https://github.com/BinaryAnalysisPlatform/bap-toolkit-manager
$: cd bap-toolkit
$: make && make install
```

## Usage

### Hello, world!

Before going in the deep and describe `bap-toolkit` options, let's just run a
simple example, which is quite descriptive itself
`bap-toolkit --artifacts=/bin/echo --recipe=forbidden-symbol`
and open "results.html" in your favorite browser.

### Report
An html report display incidents from the (spoiler!) incidents file.
The default name "results.html" can be overriden with
   `bap-toolkit --output="myresults.html`
   `bap-toolkit -o "myresults.html`

### Basic options and examples
- `bap-toolkit --list-recipes` will print a list of recipes with a short decription of each one.

- `bap-toolkit -t <user-provided-tool>`
  `bap-toolkit --tool=<user-provided-tool>`
   Being a frontend for `bap` docker infrastructure, `bap-toolkit` still assume an
   extensibility in the part of the underlying docker images. Thus, by default
   it capable to run all the analysis from `binaryanalysisplatform/bap-toolkit`
   docker image. But user can provide an own docker image with the analysis
   with the only prerequisite to have `bap` installed there.
   So, the output of
   `bap-toolkit --list-recipes` and
   `bap-toolkit -t <user-provided-tool> --list-recipes`
   can be different.

- `bap-toolkit --list-artifacts` will print a list of available artifacts from the
  `binaryanalysisplatform/bap-artifacts` repository.

- `bap-toolkit --artifacts=/bin/echo,wpa_cli-2.2 --recipes=defective-symbol,jpl-rule-14`
  `bap-toolkit -a /bin/echo -a wpa_cli-2.2 -r defective-symbol -r jpl-rule-14`
   will run the both of the next analysis: `defective-symbol` and `jpl-rule-14` to
   the file `/bin/echo` and `wpa_cli-2.2`
   Note, that the first artifact will be taken from the host machine, while the second
   one will be found in `bap-artifacts` docker repository. In other words, `bap-toolkit`
   searches the mentioned artifact first on the host machine and if can't be found continue
   to search in the `bap-artifacts` repository.
   Recipes could be run with parameters, just make sure that such recipes are mentioned
   separately from the others:
   ```
   bap-toolkit -r r1 --recipes=r2:par1=val1,par2=val2
   ```
   OR
   ```
   bap-toolkit -r r1 -r r2:par1=val1,par2=val2 --recipe r3"
   ```

- `bap-toolkit --schedule=<path>`
   reads a schedule of tasks from a file at `path`.
   the example of schedule file:
   ```
   /bin/echo   av-rule-174
   wpa_cli-2.2 defective-symbol
   /bin/echo   defective-symbol jpl-rule-14
   ```
   The syntax is pretty straightforward, with the exception of recipe parameters:
   they could be written in either of then next variants:
   ```
   artifact1 recipe1 (recipe2 (param1 val1) (param2 val2))
   artifact2 recipe4 (recipe3 param1=val1 param2=val2)
   ```
### Limits
   There is an option to set memory and time limits for a running instance of the each analysis, e.g.
   `bap-tookit --limit=20Gb --limit=42m`
   See `bap-toolkit --help` for details.

### Display
   There are two entities that influence the appearance of the html report:
   a list of confirmations and a view.

### Confirmations
   Confirmation is a known incident. There are two kinds of the confirmations:
   - MUST confirmation - the true positive incident, such that must occure
   in the analysis, i.e. a real bug, an expected outcome from the analysis.
   - MAY confirmation - the false negative incident, such that may occure
   in the analysis or may not, i.e. not a real bug, but a side effect that
   could pop up during the analysis.

   Confirmations themself need only for display purposes and could be
   set with
   `bap-toolkit -c <path>`
   `bap-toolkit --confirmations=<path>`
   Each line in the file at the given path describes a single confirmation.
   e.g.
   ```
   httpd-2.4.18  MAY  unused-return-value  0x44B118
   ```
   is a confirmation from the artifact `httpd-2.4.18` states
   that analysis `unused-return-value` may produce an incident at address
   `0x44B118`.
   Another example:
   ```
   juliet-cwe-476 MUST null-ptr-deref 0x11133 0x11113
   ```
   is a confirmation states that there must be an incident from
   the analysis `null-ptr-deref` at the address `0x11133` from
   the pointer introduced at address `0x11113`.

   So the addresses here describes an incident as precisely as it possible,
   i.e. it could be several locations. That makes incidents
   ```
   juliet-cwe-476 MUST null-ptr-deref 0x11133 0x11113
   juliet-cwe-476 MUST null-ptr-deref 0x11133 0x42424
   ```
   different from each other.

   ```
   <artifact> <MUST | MAY> <analysis-name> <location 1> <optional location 2> <optional location 3> ...
   ```

### View
   View is a list of hints how to display the results of the particular analysis in the html report.
   `bap-toolkit --view=<file>`
   `bap-toolkit -v <file>`

   There are next hints in the view file:
   - alias
   - web
   - tab

   `alias` hint provides an alternative name for the incident, e.g.
   `alias unused-return-value  "Unused return value"`

   `web` hint provides an external link for the analysys (i.e. for the name of the incident)
    e.g. `web memcheck-double-release "https://cwe.mitre.org/data/definitions/415.html"`

   and, finaly, `tab` hint describe how to display the results of the paticular analysis in
   a table like manner.

   there are three types of columns to choose:
   - name
   - addr
   - path

   `name` is a name of the function where incident happened or the function
   that was called last, depends of the each incident kind.
   `addr` is a address of the incident location
   `path N` is a list of calls that lead to the incident, where N
    is a number of calls to display ( <= 5).

    e.g.
    `tab unused-return-value (path 1) addr`
    states that the result of the analysis `unused-return-value`
    need to be displayed as a table with two columns:
    last function called before the incident and address of the incident.


### Misc
   - `bap-toolkit --incidents=<file>`
     `bap-toolkit -i <file`
     build a report from the incidents file without running any analysis.
     the name of the artifact will be taken from the name of the file for
     display purposes.

   - `bap-toolkit --store=<file>`
   - `bap-toolkit --update=<file>`
   - `bap-toolkit --from=<file>`
     The results of the analysis can be stored in a file to make some
     manipulations later (e.g. run analysis on a server and make a report on
     desktop machine).
     The next sequence of commands
     ```
     :~$ bap-toolkit --store=my.db -a /bin/echo -r av-rule-174
     :~$ bap-toolkit --update=my.db -a /bin/ls -r jpl-rule-14
     :~$ bap-toolkit --from=my.db
     ```
     will produce an html report for two artifacts: /bin/echo and /bin/ls.
