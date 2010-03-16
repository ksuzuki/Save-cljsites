# save-cljsites - save the Clojure websites

This program saves the pages and its resources of some well known Clojure
websites. Those are:

* [http://clojure.org/](http://clojure.org/)
* [http://richhickey.github.com/clojure/](http://richhickey.github.com/clojure/)
* [http://richhickey.github.com/clojure-contrib/](http://richhickey.github.com/clojure-contrib/)

## Usage

You can run this program either in the repl or from the command line. To run
this program from the command line, build a stand-alone jar using leiningen.

### in the Repl

`(save-cljsites/save-all & option)`
> Save the Clojure websites to the user's current working directory. Adding the
> :verbose option will print extra status messages.

`(save-cljsites/save site-key & option)`
> Save the Clojure website specified by the site key to the user's current
> working directory. Adding the :verbose option will prints extra status
> messages.
>
> e.g. `(save-cljsites/save :org :verbose)`
>
> Run `(save-cljsites/print-sites)` to print known site keys, their URLs and
> directories to save.

`(save-cljsites/print-sites)`
> Print all Clojure site keys, their URLs and directories to save.

### from the Command Line

When you run this program from the command line with no options, all
Clojure websites above are saved by default. You can also save only a
specific Clojure website using the --site option. Run this program with the
--help option for more details of the available options.

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
