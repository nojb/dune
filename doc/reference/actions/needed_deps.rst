needed_deps
----

.. highlight:: dune

.. describe:: (needed_deps <file> ...)

   Parse the files to extract defined dependencies, and declare them as
   needed dependencies for the rule. Whenever any of these dependencies
   change, the rule is rerun. Note that for this to be meaningful, the 
   dependencies declared for the rule should be declared as order only
   dependencies (see :docs:`concepts/dependency-spec`), and then only
   the actual dependencies are passed to ``needed_deps``.
   
   The file passed as an argument contains one or more S-expression constructed from
   the following atoms:
   - ``(file <filename>)`` declares this file as a needed dependency.
   - ``(alias <alias_name>)`` declares the alias as a needed dependency. For instance, 
     when the construction of this alias changes, the rule is rerun.
   - ``(file_selector <glob>)`` declares all files matched by ``<glob>`` as needed 
     dependencies. See the :ref:`glob <glob>` for details.
   - ``(universe)`` declares the whole universe as a needed dependency. 

   In the following example, both a and b are declared as order only dependencies,
   but b is declared as a needed dependency for the rule. The rule will only be
   reran if b changes, otherwise nothing happens.
   
   Example::
   
   (rule
    (deps (order_only (file a) (file b)))
    (action
     (progn
     (with-stdout-to deps (echo "(file b)"))
     (needed_deps deps))))