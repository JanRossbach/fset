# fset

A Clojure library designed to automatically rewrite B machines using lisb.
The rewrite removes enumerated and deferred sets up to a certain size and replaces them with boolean variables.
This is done to increase performance of partial order reduction optimizations in prob.

## Repl Usage and Configuration

``` clojure
(ns myns.core
  (:require
    [fset.lib.core :as fset]
    [lisb.translation.util :refer [b->ir ir->b]]))

(def ir (b->ir (slurp "path/to/b/machine"))) ;; Read in the B machine IR from a file

(fset/unroll-ops ir) ;; Just unrolling operations

(fset/boolencode ir) ;; Translation using the default config

(fset/boolencode ir :excluded-vars :all) ; Configuration can be done through kwargs
(fset/boolencode ir :excluded-vars #{:var1 :var2})

(fset/boolencode ir :logging true)

;; Or you can do a complete personal configuration
(def my-config
  {:max-unroll-size 200 ;; Upper bound for variable size
   :deff-set-size 2 ;; How many variables should deferred sets be expanded to?
   :logging true ;; Info level logging including errors and unsupported expressions
   :prob-logging false ;; All logging enabled including prob debugging
   :excluded-vars #{}}) ;; Exclude individual variables from being expanded

(fset/set-config! my-config) ;; You can apply your config as the default for the session

(fset/set-config-var! :logging false) ;; You can also set individual vars like that

(def result-ir (fset/boolencode ir)) ; This uses my-config but with logging set to false

(ir->b result-ir) ;; Translate the results back to B

```

## Installation of cli

To install the application under Linux, clone the repository and execute the build.sh.
Requirements:
 - Java (tested on version 11)
 - clojure cli tool: https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools

```
chmod +x build.sh
./build.sh
```

## Using the cli

In order to invoke the jar file to translate machines.
```
java -jar /path/to/fset.jar file_to_translate.mch
```
This will create/override a new file in the same directory as the target file with _auto before the extension,
which contains the translated version.

To configure the CLI similarly to the repl version, you can use option flags.
The -h flags prints information on the other available options.

```
java -jar /path/to/fset.jar -h
```

For convenient invocation it is currently recommended to alias the jar file in your bashrc/zshrc like so
```
alias fset="java -jar /path/to/fset/fset.jar"
```

## License

Copyright © 2021

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
