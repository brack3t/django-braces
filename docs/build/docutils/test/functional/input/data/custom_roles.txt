Custom Roles
------------

* A role based on an existing role.

  .. role:: custom(literal)

  :custom:`one` :custom:`two` :custom:`three`

* A new role.

  .. role:: customnew

  :customnew:`one two three`

* A role with class attribute.

  .. role:: customclass
     :class: special

  :customclass:`interpreted text`

* A language-switching role:

  .. role:: language-de

  Let's count in German :language-de:`eins zwei drei`.

* A role with multiple class attributes, styled with raw directives:

  .. role:: customx
     :class: green sc language-en-GB

  .. raw:: latex

     \newcommand{\DUrolegreen}[1]{\textcolor{green}{#1}}
     \newcommand{\DUrolesc}[1]{\textsc{#1}}

  The following works in most browsers but does not validate
  (``<style>`` is only allowed in the document head)::

    .. raw:: html

      <style type="text/css"><!--
       .green {color: green;}
       .sc {font-variant: small-caps;}
       --></style>

  :customx:`British colourful text in small-caps`.
