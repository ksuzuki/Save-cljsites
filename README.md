# save-cljsites - save the Clojure websites

This program saves the pages and its resources of some known Clojure websites.
Those are:

* [http://clojure.org/](http://clojure.org/)
* [http://richhickey.github.com/clojure/](http://richhickey.github.com/clojure/)
* [http://richhickey.github.com/clojure-contrib/](http://richhickey.github.com/clojure-contrib/)

## Usage

### in Repl

`(save-cljsites/save-all & option)`
> Save the Clojure websites to the user's current working directory. Adding the
> :verbose option prints extra status messages while saving the websites.

`(save-cljsites/save site-key & option)`
> Save the Clojure website specified by the given site key to the user's
> current working directory. Adding the :verbose option prints extra status
> messages while saving the website.
>
> e.g. `(save-cljsites/save :org :verbose)`
>
> Use print-sites to print known site keys, their URLs and directories to save.

`(savae-cljsites/print-sites)`
> Print all Clojure site keys, their URLs and directories to save.

### from the command line

When you run this program from the command line, all the Clojure websites above
are saved by default. You can also save only a specific Clojure website using
the --site option. Run this program with th --help option for more details of
the available options.

## Requirements

This program requires HttpClient 4.0 and dependencies from the Apache Software
Foundation ([http://hc.apache.org/]([http://hc.apache.org/)).

## License

Copyright (c) 2010 Kei Suzuki. All rights reserved.

The use and distribution terms for this software are covered by the Eclipse
Public License 1.0 ([http://opensource.org/licenses/eclipse-1.0.php](http://opensource.org/licenses/eclipse-1.0.php)) 
which can be found in the file epl-v10.html included with this distribution.
By using this software in any fashion, you are agreeing to be bound by the
terms of this license.

You must not remove this notice, or any other, from this software.
