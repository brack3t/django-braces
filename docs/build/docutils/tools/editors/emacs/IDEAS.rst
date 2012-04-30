The following is a list of ideas of functionality which would be nice
to have in `rst.el`. In the examples a ``@`` stands for the cursor.

Convert to id
=============

* Convert the region to an HTML id

  * For instance "Eine Überschrift" to "eine-berschrift"

  * According the same rules as reST does this

Jump to internal target
=======================

* A command to jump to the internal target the point is on

* A target may be

  * A section title

  * Footnotes / citations

  * Inline internal targets

  * Hyperlink target definition

  * Substitution definition

* See hunk #26 in `rst_el-emacs_V23_1_patch1_1_2` vs. `emacs_V23_1`
  for some ideas

Completion for directive options
================================

* Imagine ::

    .. list-table::
       :@

  with the cursor at the asterisk

* There should be a command which offers all the possible options for
  this particular directive as completion

* May be `skeleton.el` can also be useful

Completion for directives
=========================

* Imagine ::

    .. @

* There should be a command which offers all directives as completion

* May be this should work for other keywords as well

* May be this could work even at the beginning of the line

* Completion must be bound to M-TAB

  * Already existing binding must be chained

  * May be `expand.el` can help (look in package finder)?

  * May be `hippie` is good here

  * Check `(info)autotype`

Completion for user-defined elements
====================================

* Imagine ::

    |@

  or ::

    [@

  or ::

    _@

* There should be a command which offers all defined substitutions /
  footnotes / links as completion

Insertion of link alias
=======================

* Imagine ::

    Aspect of something
    ===================

    This is about the `aspect of something`_@

* There should be a command which asks you for an alias for the link,
  add the alias and change the link ::

    .. _aspects of something:

    Aspect of something
    ===================

    This is about the `aspects of something`_@

Smart use of `iimage-mode`
==========================

* There is `iimage-mode` which shows ``.. image::``\s in Emacs

* May be we can add a binding to toggle it

TOC in speedbar
===============

* If the TOC is displayed in the speedbar this could be used for
  permanent navigation

toc-mode without markup
=======================

* The markup which may be contained in a section title is not useful
  in toc-mode and should be suppressed

Sophisticated navigation in sections
====================================

* Navigation in sections similar to navigation in other structured data

  * Like XML, Lisp

  * C-M-u für Up

  * C-M-d für Down

  * C-M-f / C-M-b für Forward / Backward

Display of current location
===========================

* Display the "section path" to the current point

* Like in XML: In which element is the point?

toc-mode only to a certain level
================================

* If a TOC buffer is created a prefix argument should limit the depth
  of the listing to the given level

Imenu support
=============

* Imenu could be supported

  * See `(elisp)Imenu`

Outline support
===============

* Support for `outline-mode' / `allout-mode' would be nice

  * Should consider section titles

* May be folding is also possible

  * For item lists

Sophisticated filling
=====================

* These things must be filled special:

  * Definitions

  * Filling of ::

      * VeryLongWordSuchAsAnURLVeryLongWordSuchAsAnURLVeryLongWordSuchAsAnURLVeryLongWordSuchAsAnURLVeryLongWordSuchAsAnURL

    should work as expected by *not* breaking the line

* These things may not be filled at all

  * Literal blocks

  * Tables

  * Section headers

  * Link definitions

Sophisticated indentation
=========================

* It should be generally possible to shift one more to the right

  * This makes indentation for quotes possible

  * But not for literal blocks

* For item lists the best tab should be on the same level as the last
  item::

    * bla

    @

  * The second best tab should be where text starts::

      * bla

	@

* <backtab> should be used to indent in the other direction

  * Or may be C-u <tab> but this has a different meaning

* <tab> could obsolete C-c C-r <tab>

  * For this the indentation needs to be determined at the start
    instead of per line

    * <tab> over list works::

	Text

	  * GGGGGG
	  * SSSSSSSSSSSSSSS
	  * TTTTTTTT
	  * ZZZZZZZZ

    * <tab> over list doesn't work::

	Text

	* GGGGGG
	* SSSSSSSSSSSSSSS
	* TTTTTTTT
	* ZZZZZZZZ

List to sections
================

* A command would be nice which

  * transforms the first level of a nested list in a region into a
    header

  * removes one level of indentation from the rest of the list

Change section level by more than one step
==========================================

* It would be nice if <C-h> `rst-adjust` could rotate a section
  adornment more than one level

* A modification of the semantic of the prefix arguments could do this

  * Non-zero numeric prefix arg n rotates n step in the given direction

  * Prefix arg 0 toggles overline / underline

    * This would be different from current setup

Compiling for syntax check
==========================

* Compiling with results going to `/dev/null` would be useful

  * This would just do a syntax check with no files lying around

* Toolset choice for <C-c C-c C-c> `rst-compile` must be by
  customizable if at all necessary

  * Customization group must be used

Renumber an exisiting enumeration
=================================

* Renumbering an exisiting enumeration is not possible yet

Command to move across blocks
=============================

* A command moving forward / backward across the content blocks of the
  current block would be nice

  * For instance: Move across all blocks contained in an item or field

  * This would move to the start of the sibling of the current block

  * Would allow to jump to the next item on the same level in a list

* <C-M-f> `forward-sexp` could be a nice binding

rst-toc-insert features
=======================

* The `contents::` options could be parsed to figure out how deep to
  render the inserted TOC

* On load, detect any existing TOCs and set the properties for links

* TOC insertion should have an option to add empty lines

* TOC insertion should deal with multiple lines

* Automatically detect if we have a `section-numbering::` in the
  corresponding section, to render the toc.

Automatic handling of `.txt` files
----------------------------------

It would be nice to differentiate between text files using
reStructuredText and other general text files. If we had a function to
automatically guess whether a `.txt` file is following the
reStructuredText conventions, we could trigger `rst-mode` without
having to hard-code this in every text file, nor forcing the user to
add a local mode variable at the top of the file. We could perform
this guessing by searching for a valid adornment at the top of the
document or searching for reStructuredText directives further on.
