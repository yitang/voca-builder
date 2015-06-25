Installation
============



Package Manager in Emacs
------------------------



The recommended way to install voca-builder is via ``package.el``, the build-in
package manager in Emacs.

voca-builder.el is available on the major community maintained
repository `http://melpa.org/ <http://melpa.org/>`_. You can install ``voca-builder`` with the
following command:

``M-x package-install [RET] voca-builder [RET]``

or by execute this bit of Emacs Lisp code

.. code-block:: scheme

    (unless (package-installed-p 'voca-builder)
      (package-install 'voca-builder))

If the installation doesn't work try to refresh the package list:

``M-x package-refresh-contents [RET]``

Install Manually
----------------



You can also install manually with two steps: first clone this
repository, 

.. code-block:: sh

    git clone https://github.com/yitang/voca-builder.git

then add the directory containing voca-builder.el to
Emacs's search path as follows:


.. code-block:: scheme

    (add-to-list 'load-path "~/path/to/voca-builder.el/")
    (require 'voca-builder)
