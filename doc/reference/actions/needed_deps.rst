needed_deps
----

.. highlight:: dune

.. describe:: (needed_deps <file> ...)

   Parse the files to extract defined dependencies, and declare them as
   needed_deps for the rule. In other words, the needed_deps are the "real"
   dependencies of the rule, and the decision of rerunnig the rule depends only on
   whether these dependencies are up-to-date or not. 

   Example::

   (needed_deps .txt)