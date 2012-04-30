Non-ASCII characters
--------------------

Punctuation and footnote symbols

= ===================================
– en-dash
— em-dash
‘ single turned comma quotation mark
’ single comma quotation mark
‚ low single comma quotation mark
“ double turned comma quotation mark
” double comma quotation mark
„ low double comma quotation mark
† dagger
‡ double dagger
♦ black diamond suit
♥ black heart suit
♠ black spade suit
♣ black club suit
… ellipsis
™ trade mark sign
⇔ left-right double arrow
= ===================================


The `Latin-1 extended` Unicode block

===  =  =  =  =  =  =  =  =  =  =
 ..  0  1  2  3  4  5  6  7  8  9
---  -  -  -  -  -  -  -  -  -  -
160     ¡  ¢  £     ¥  ¦  §  ¨  ©
170  ª  «  ¬  ­  ®  ¯  °  ±  ²  ³
180  ´  µ  ¶  ·  ¸  ¹  º  »  ¼  ½
190  ¾  ¿  À  Á  Â  Ã  Ä  Å  Æ  Ç
200  È  É  Ê  Ë  Ì  Í  Î  Ï  Ð  Ñ
210  Ò  Ó  Ô  Õ  Ö  ×  Ø  Ù  Ú  Û
220  Ü  Ý  Þ  ß  à  á  â  ã  ä  å
230  æ  ç  è  é  ê  ë  ì  í  î  ï
240  ð  ñ  ò  ó  ô  õ  ö  ÷  ø  ù
250  ú  û  ü  ý  þ  ÿ
===  =  =  =  =  =  =  =  =  =  =

* The following line should not be wrapped, because it uses
  no-break spaces (\\u00a0):

  X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X

* Line wrapping with/without breakpoints marked by soft hyphens
  (\\u00ad):

  pdn­derd­mdtd­ri­schpdn­derd­mdtd­ri­schpdn­derd­mdtd­ri­schpdn­derd­mdtd­ri­schpdn­derd­mdtd­ri­sch

  pdnderdmdtdrischpdnderdmdtdrischpdnderdmdtdrischpdnderdmdtdrischpdnderdmdtdrisch

* The currency sign (\\u00a4) is not supported by all fonts
  (some have an Euro sign at its place). You might see an error
  like::

    ! Package textcomp Error: Symbol \textcurrency not provided by
    (textcomp)                font family ptm in TS1 encoding.
    (textcomp)                Default family used instead.

  (which in case of font family ptm is a false positive). Add either

  :warn: turn the error in a warning, use the default symbol (bitmap), or
  :force,almostfull: use the symbol provided by the font at the users
  		     risk,

  to the document options or use a different font package.
