
# folderfun [0.1.1] -- 2019-02-28

* Fixed a few issues with double-slashes that could get added or reported.

# folderfun [0.1] -- 2019-02-27

* Packaged for CRAN
* `optOrEnvVar` first looks up a name's value first as a option, then as an environment variable if the option value is unset or empty (string or vector).
* `setff` invocation can take several forms, and more extensive variable search can be performed; name for accessor function can be interpreted as name to lookup exactly, as uppercase, or as lowercase according to `optOrEnvVar`.
* Primary "value" argument to `setff` is now called `path` rather than `location`.

# folderfun [0.0.1] -- 2018-12-11

This is the initial package release.

* `setff` function establishes a variable accessor function.
* `listff` function lists available variable accessor functions.
