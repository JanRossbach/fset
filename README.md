# fset

A Clojure library designed to automatically rewrite B machines using lisb.
The rewrite removes enumerated and deferred sets up to a certain size and replaces them with boolean variables.
This is done to increase performance of partial order reduction optimizations in prob.

## Usage

- start a repl with
```
lein repl
```
- Run the core functions in a repl
- Examples can be found in experiment.clj
- Configuration can be done in config.clj

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
