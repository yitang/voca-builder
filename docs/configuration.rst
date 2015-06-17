Configuration
=============



To see all the configurable settings in voca-builder, type 

``M-x customize-group [RET] voca-builder [RET]``

and you are able to customise these variables directly. However, the
recommended way to do is to add the non-interaactive way. 

The important variables are: 

voca-builder/voca-file
    where you wanna to save the new vocabulary
    and their meanings to, the default is ~/.vocabulary

voca-builder/current-tag
    which tags should added to the
    vocabulary nodes. The default is "GENE" (general). 

voca-builder/export-file
    where to save the exported vocabularies. 

The following is a sample. 

.. code-block:: scheme

    (setq voca-builder/voca-file "~/git/Learning/Emacs_Voca/voca_example.org") 
    (setq voca-builder/current-tag "Demo")
    (setq voca-builder/export-file "~/voca-builder-temp.org")

I read all the time and frequently confront words that I am not
familiar with, so I bind the keystroke ``F4`` to shows the meaning
quickly. 

.. code-block:: scheme

    (global-set-key (kbd "<f4>") 'voca-builder/search-popup)
