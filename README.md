naututil
=========

erlang module for interfacing with nautilus

### get rebar installed ###

### Dependencies

To build rebar you will need a working installation of Erlang R13B03 (or later).

Should you want to clone the rebar repository, you will also require git.

#### Building rebar

```sh
$ git clone git://github.com/rebar/rebar.git
$ cd rebar
$ ./bootstrap
Recompile: src/getopt
...
Recompile: src/rebar_utils
==> rebar (compile)
Congratulations! You now have a self-contained script called "rebar" in
your current working directory. Place this script anywhere in your path
and you can use rebar to build OTP-compliant apps.
```

#### building naututil
create a directory for downloaded erlang modules, then
in that directory

```sh
$ git clone git://github.com/bakkemo/naututil.git
$ rebar get-deps
$ rebar compile
```
ignore warnings from the get-deps command

set the nautilus authentication endpoint:
```sh
$ export NAUT_AUTH_URI=http://79eebd3a.ngrok.com/
```
tell erlang where to find these new extra modules
```sh
$ export ERL_LIBS=/path/to/created/lib/directory:/path/to/created/lib/directory/naututil/deps
```
run erlang, an in the erlang shell type:
```sh
1> naututil:start().
```
