CHANGES
=======

Version 0.3
-----------

Replace monad-control with unliftio-core.
As a result, immortal will no longer work with stateful monads like StateT,
which is considered a feature.

Version 0.2.2.1
---------------

Fix the README

Version 0.2.2
-------------

Add `onUnexpectedFinish`

Version 0.2.1
-------------

Add `wait` and `wait-stm` functions

Version 0.2
-----------

* Pass a `Thread` handle to the thread itself
* Add `mortalize`, `immortalize`
