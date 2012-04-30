Custom Roles in LaTeX
---------------------

* Role names and class arguments are converted to conform to the
  regular expression ``[a-z][-a-z0-9]*`` (letters are downcased,
  accents and similar decoration is stripped, non-conforming
  characters are replaced by a hyphen).

  Class arguments may contain numbers and hyphens, which need special
  treatment in LaTeX command names.

  .. role:: custom4
     :class: large custom4 small_caps custom.role custom\role

  :custom4:`Interpreted Text`

* With LaTeX, roles can be styled within the document using the `raw`
  directive.

  .. raw:: latex

    \newcommand{\DUrolelarge}[1]{{\large #1}}
    \makeatletter
    \@namedef{DUrolesmall-caps}{\textsc}
    \@namedef{DUrolecustom4}{\textbf}
    \makeatother

  :custom4:`Interpreted Text` in large, bold, small-caps.

* Custom roles can be based on standard roles:

  .. role:: custom-emphasis(emphasis)

  This is a :custom-emphasis:`customized emphasis text role`

  .. role:: custom-literal(literal)

  This is a :custom-literal:`customized literal text role`

  .. role:: custom-strong(strong)

  This is a :custom-strong:`customized strong text role`

  .. role:: custom-subscript(subscript)

  This is a :custom-subscript:`customized subscript text role`

  .. role:: custom-superscript(superscript)

  This is a :custom-superscript:`customized superscript text role`

  .. role:: custom-title-reference(title-reference)

  This is a :custom-title-reference:`customized title-reference text role`
