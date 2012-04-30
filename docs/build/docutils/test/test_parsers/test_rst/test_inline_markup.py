#! /usr/bin/env python

# $Id: test_inline_markup.py 5642 2008-09-05 18:18:28Z goodger $
# Author: David Goodger <goodger@python.org>
# Copyright: This module has been placed in the public domain.

"""
Tests for inline markup in docutils/parsers/rst/states.py.
Interpreted text tests are in a separate module, test_interpreted.py.
"""

from __init__ import DocutilsTestSupport

def suite():
    s = DocutilsTestSupport.ParserTestSuite()
    s.generateTests(totest)
    return s

totest = {}

totest['emphasis'] = [
["""\
*emphasis*
""",
"""\
<document source="test data">
    <paragraph>
        <emphasis>
            emphasis
"""],
[u"""\
l'*emphasis* and l\u2019*emphasis* with apostrophe
""",
u"""\
<document source="test data">
    <paragraph>
        l'
        <emphasis>
            emphasis
         and l\u2019
        <emphasis>
            emphasis
         with apostrophe
"""],
["""\
*emphasized sentence
across lines*
""",
"""\
<document source="test data">
    <paragraph>
        <emphasis>
            emphasized sentence
            across lines
"""],
["""\
*emphasis without closing asterisk
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="id2" refid="id1">
            *
        emphasis without closing asterisk
    <system_message backrefs="id2" ids="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline emphasis start-string without end-string.
"""],
["""\
'*emphasis*' and 1/*emphasis*/2 and 3-*emphasis*-4 and 5:*emphasis*:6
but not '*' or '"*"' or  x*2* or 2*x* or \\*args or *
or *the\\* *stars\\\\\\* *inside*

(however, '*args' will trigger a warning and may be problematic)

what about *this**?
""",
"""\
<document source="test data">
    <paragraph>
        '
        <emphasis>
            emphasis
        ' and 1/
        <emphasis>
            emphasis
        /2 and 3-
        <emphasis>
            emphasis
        -4 and 5:
        <emphasis>
            emphasis
        :6
        but not '*' or '"*"' or  x*2* or 2*x* or *args or *
        or \n\
        <emphasis>
            the* *stars\* *inside
    <paragraph>
        (however, '
        <problematic ids="id2" refid="id1">
            *
        args' will trigger a warning and may be problematic)
    <system_message backrefs="id2" ids="id1" level="2" line="5" source="test data" type="WARNING">
        <paragraph>
            Inline emphasis start-string without end-string.
    <paragraph>
        what about \n\
        <emphasis>
            this*
        ?
"""],
["""\
Emphasized asterisk: *\\**

Emphasized double asterisk: *\\***
""",
"""\
<document source="test data">
    <paragraph>
        Emphasized asterisk: \n\
        <emphasis>
            *
    <paragraph>
        Emphasized double asterisk: \n\
        <emphasis>
            **
"""],
]

totest['strong'] = [
["""\
**strong**
""",
"""\
<document source="test data">
    <paragraph>
        <strong>
            strong
"""],
[u"""\
l'**strong** and l\u2019**strong** with apostrophe
""",
u"""\
<document source="test data">
    <paragraph>
        l'
        <strong>
            strong
         and l\u2019
        <strong>
            strong
         with apostrophe
"""],
[u"""\
quoted '**strong**', quoted "**strong**",
quoted \u2018**strong**\u2019, quoted \u201c**strong**\u201d,
quoted \xab**strong**\xbb
""",
u"""\
<document source="test data">
    <paragraph>
        quoted '
        <strong>
            strong
        ', quoted "
        <strong>
            strong
        ",
        quoted \u2018
        <strong>
            strong
        \u2019, quoted \u201c
        <strong>
            strong
        \u201d,
        quoted \xab
        <strong>
            strong
        \xbb
"""],
["""\
(**strong**) but not (**) or '(** ' or x**2 or \\**kwargs or **

(however, '**kwargs' will trigger a warning and may be problematic)
""",
"""\
<document source="test data">
    <paragraph>
        (
        <strong>
            strong
        ) but not (**) or '(** ' or x**2 or **kwargs or **
    <paragraph>
        (however, '
        <problematic ids="id2" refid="id1">
            **
        kwargs' will trigger a warning and may be problematic)
    <system_message backrefs="id2" ids="id1" level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Inline strong start-string without end-string.
"""],
["""\
Strong asterisk: *****

Strong double asterisk: ******
""",
"""\
<document source="test data">
    <paragraph>
        Strong asterisk: \n\
        <strong>
            *
    <paragraph>
        Strong double asterisk: \n\
        <strong>
            **
"""],
["""\
**strong without closing asterisks
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="id2" refid="id1">
            **
        strong without closing asterisks
    <system_message backrefs="id2" ids="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline strong start-string without end-string.
"""],
]

totest['literal'] = [
["""\
``literal``
""",
"""\
<document source="test data">
    <paragraph>
        <literal>
            literal
"""],
["""\
``\\literal``
""",
"""\
<document source="test data">
    <paragraph>
        <literal>
            \\literal
"""],
["""\
``lite\\ral``
""",
"""\
<document source="test data">
    <paragraph>
        <literal>
            lite\\ral
"""],
["""\
``literal\\``
""",
"""\
<document source="test data">
    <paragraph>
        <literal>
            literal\\
"""],
[u"""\
l'``literal`` and l\u2019``literal`` with apostrophe
""",
u"""\
<document source="test data">
    <paragraph>
        l'
        <literal>
            literal
         and l\u2019
        <literal>
            literal
         with apostrophe
"""],
[u"""\
quoted '``literal``', quoted "``literal``",
quoted \u2018``literal``\u2019, quoted \u201c``literal``\u201d,
quoted \xab``literal``\xbb
""",
u"""\
<document source="test data">
    <paragraph>
        quoted '
        <literal>
            literal
        ', quoted "
        <literal>
            literal
        ",
        quoted \u2018
        <literal>
            literal
        \u2019, quoted \u201c
        <literal>
            literal
        \u201d,
        quoted \xab
        <literal>
            literal
        \xbb
"""],
[u"""\
``'literal'`` with quotes, ``"literal"`` with quotes,
``\u2018literal\u2019`` with quotes, ``\u201cliteral\u201d`` with quotes,
``\xabliteral\xbb`` with quotes
""",
u"""\
<document source="test data">
    <paragraph>
        <literal>
            'literal'
         with quotes, 
        <literal>
            "literal"
         with quotes,
        <literal>
            \u2018literal\u2019
         with quotes, 
        <literal>
            \u201cliteral\u201d
         with quotes,
        <literal>
            \xabliteral\xbb
         with quotes
"""],
["""\
``literal ``TeX quotes'' & \\backslash`` but not "``" or ``

(however, ``standalone TeX quotes'' will trigger a warning
and may be problematic)
""",
"""\
<document source="test data">
    <paragraph>
        <literal>
            literal ``TeX quotes'' & \\backslash
         but not "``" or ``
    <paragraph>
        (however, \n\
        <problematic ids="id2" refid="id1">
            ``
        standalone TeX quotes'' will trigger a warning
        and may be problematic)
    <system_message backrefs="id2" ids="id1" level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Inline literal start-string without end-string.
"""],
["""\
Find the ```interpreted text``` in this paragraph!
""",
"""\
<document source="test data">
    <paragraph>
        Find the \n\
        <literal>
            `interpreted text`
         in this paragraph!
"""],
["""\
``literal without closing backquotes
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="id2" refid="id1">
            ``
        literal without closing backquotes
    <system_message backrefs="id2" ids="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline literal start-string without end-string.
"""],
["""\
Python ``list``\\s use square bracket syntax.
""",
"""\
<document source="test data">
    <paragraph>
        Python \n\
        <literal>
            list
        s use square bracket syntax.
"""],
]

totest['references'] = [
["""\
ref_
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="ref" refname="ref">
            ref
"""],
[u"""\
l'ref_ and l\u2019ref_ with apostrophe
""",
u"""\
<document source="test data">
    <paragraph>
        l'
        <reference name="ref" refname="ref">
            ref
         and l\u2019
        <reference name="ref" refname="ref">
            ref
         with apostrophe
"""],
[u"""\
quoted 'ref_', quoted "ref_",
quoted \u2018ref_\u2019, quoted \u201cref_\u201d,
quoted \xabref_\xbb,
but not 'ref ref'_, "ref ref"_, \u2018ref ref\u2019_,
\u201cref ref\u201d_, or \xabref ref\xbb_
""",
u"""\
<document source="test data">
    <paragraph>
        quoted '
        <reference name="ref" refname="ref">
            ref
        ', quoted "
        <reference name="ref" refname="ref">
            ref
        ",
        quoted \u2018
        <reference name="ref" refname="ref">
            ref
        \u2019, quoted \u201c
        <reference name="ref" refname="ref">
            ref
        \u201d,
        quoted \xab
        <reference name="ref" refname="ref">
            ref
        \xbb,
        but not 'ref ref'_, "ref ref"_, \u2018ref ref\u2019_,
        \u201cref ref\u201d_, or \xabref ref\xbb_
"""],
["""\
ref__
""",
"""\
<document source="test data">
    <paragraph>
        <reference anonymous="1" name="ref">
            ref
"""],
[u"""\
l'ref__ and l\u2019ref__ with apostrophe
""",
u"""\
<document source="test data">
    <paragraph>
        l'
        <reference anonymous="1" name="ref">
            ref
         and l\u2019
        <reference anonymous="1" name="ref">
            ref
         with apostrophe
"""],
[u"""\
quoted 'ref__', quoted "ref__",
quoted \u2018ref__\u2019, quoted \u201cref__\u201d,
quoted \xabref__\xbb,
but not 'ref ref'__, "ref ref"__, \u2018ref ref\u2019__,
\u201cref ref\u201d__, or \xabref ref\xbb__
""",
u"""\
<document source="test data">
    <paragraph>
        quoted '
        <reference anonymous="1" name="ref">
            ref
        ', quoted "
        <reference anonymous="1" name="ref">
            ref
        ",
        quoted \u2018
        <reference anonymous="1" name="ref">
            ref
        \u2019, quoted \u201c
        <reference anonymous="1" name="ref">
            ref
        \u201d,
        quoted \xab
        <reference anonymous="1" name="ref">
            ref
        \xbb,
        but not 'ref ref'__, "ref ref"__, \u2018ref ref\u2019__,
        \u201cref ref\u201d__, or \xabref ref\xbb__
"""],
["""\
ref_, r_, r_e-f_, -ref_, and anonymousref__,
but not _ref_ or __attr__ or object.__attr__
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="ref" refname="ref">
            ref
        , \n\
        <reference name="r" refname="r">
            r
        , \n\
        <reference name="r_e-f" refname="r_e-f">
            r_e-f
        , -
        <reference name="ref" refname="ref">
            ref
        , and \n\
        <reference anonymous="1" name="anonymousref">
            anonymousref
        ,
        but not _ref_ or __attr__ or object.__attr__
"""],
]

totest['phrase_references'] = [
["""\
`phrase reference`_
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="phrase reference" refname="phrase reference">
            phrase reference
"""],
[u"""\
l'`phrase reference`_ and l\u2019`phrase reference`_ with apostrophe
""",
u"""\
<document source="test data">
    <paragraph>
        l'
        <reference name="phrase reference" refname="phrase reference">
            phrase reference
         and l\u2019
        <reference name="phrase reference" refname="phrase reference">
            phrase reference
         with apostrophe
"""],
[u"""\
quoted '`phrase reference`_', quoted "`phrase reference`_",
quoted \u2018`phrase reference`_\u2019,
quoted \u201c`phrase reference`_\u201d,
quoted \xab`phrase reference`_\xbb
""",
u"""\
<document source="test data">
    <paragraph>
        quoted '
        <reference name="phrase reference" refname="phrase reference">
            phrase reference
        ', quoted "
        <reference name="phrase reference" refname="phrase reference">
            phrase reference
        ",
        quoted \u2018
        <reference name="phrase reference" refname="phrase reference">
            phrase reference
        \u2019,
        quoted \u201c
        <reference name="phrase reference" refname="phrase reference">
            phrase reference
        \u201d,
        quoted \xab
        <reference name="phrase reference" refname="phrase reference">
            phrase reference
        \xbb
"""],
[u"""\
`'phrase reference'`_ with quotes, `"phrase reference"`_ with quotes,
`\u2018phrase reference\u2019`_ with quotes,
`\u201cphrase reference\u201d`_ with quotes,
`\xabphrase reference\xbb`_ with quotes
""",
u"""\
<document source="test data">
    <paragraph>
        <reference name="'phrase reference'" refname="'phrase reference'">
            'phrase reference'
         with quotes, 
        <reference name=""phrase reference"" refname=""phrase reference"">
            "phrase reference"
         with quotes,
        <reference name="\u2018phrase reference\u2019" refname="\u2018phrase reference\u2019">
            \u2018phrase reference\u2019
         with quotes,
        <reference name="\u201cphrase reference\u201d" refname="\u201cphrase reference\u201d">
            \u201cphrase reference\u201d
         with quotes,
        <reference name="\xabphrase reference\xbb" refname="\xabphrase reference\xbb">
            \xabphrase reference\xbb
         with quotes
"""],
["""\
`anonymous reference`__
""",
"""\
<document source="test data">
    <paragraph>
        <reference anonymous="1" name="anonymous reference">
            anonymous reference
"""],
[u"""\
l'`anonymous reference`__ and l\u2019`anonymous reference`__ with apostrophe
""",
u"""\
<document source="test data">
    <paragraph>
        l'
        <reference anonymous="1" name="anonymous reference">
            anonymous reference
         and l\u2019
        <reference anonymous="1" name="anonymous reference">
            anonymous reference
         with apostrophe
"""],
[u"""\
quoted '`anonymous reference`__', quoted "`anonymous reference`__",
quoted \u2018`anonymous reference`__\u2019,
quoted \u201c`anonymous reference`__\u201d,
quoted \xab`anonymous reference`__\xbb
""",
u"""\
<document source="test data">
    <paragraph>
        quoted '
        <reference anonymous="1" name="anonymous reference">
            anonymous reference
        ', quoted "
        <reference anonymous="1" name="anonymous reference">
            anonymous reference
        ",
        quoted \u2018
        <reference anonymous="1" name="anonymous reference">
            anonymous reference
        \u2019,
        quoted \u201c
        <reference anonymous="1" name="anonymous reference">
            anonymous reference
        \u201d,
        quoted \xab
        <reference anonymous="1" name="anonymous reference">
            anonymous reference
        \xbb
"""],
[u"""\
`'anonymous reference'`__ with quotes, `"anonymous reference"`__ with quotes,
`\u2018anonymous reference\u2019`__ with quotes,
`\u201canonymous reference\u201d`__ with quotes,
`\xabanonymous reference\xbb`__ with quotes
""",
u"""\
<document source="test data">
    <paragraph>
        <reference anonymous="1" name="'anonymous reference'">
            'anonymous reference'
         with quotes, 
        <reference anonymous="1" name=""anonymous reference"">
            "anonymous reference"
         with quotes,
        <reference anonymous="1" name="\u2018anonymous reference\u2019">
            \u2018anonymous reference\u2019
         with quotes,
        <reference anonymous="1" name="\u201canonymous reference\u201d">
            \u201canonymous reference\u201d
         with quotes,
        <reference anonymous="1" name="\xabanonymous reference\xbb">
            \xabanonymous reference\xbb
         with quotes
"""],
["""\
`phrase reference
across lines`_
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="phrase reference across lines" refname="phrase reference across lines">
            phrase reference
            across lines
"""],
["""\
`phrase\`_ reference`_
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="phrase`_ reference" refname="phrase`_ reference">
            phrase`_ reference
"""],
["""\
Invalid phrase reference:

:role:`phrase reference`_
""",
"""\
<document source="test data">
    <paragraph>
        Invalid phrase reference:
    <paragraph>
        <problematic ids="id2" refid="id1">
            :role:`phrase reference`_
    <system_message backrefs="id2" ids="id1" level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Mismatch: both interpreted text role prefix and reference suffix.
"""],
["""\
Invalid phrase reference:

`phrase reference`:role:_
""",
"""\
<document source="test data">
    <paragraph>
        Invalid phrase reference:
    <paragraph>
        <problematic ids="id2" refid="id1">
            `phrase reference`:role:_
    <system_message backrefs="id2" ids="id1" level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Mismatch: both interpreted text role suffix and reference suffix.
"""],
["""\
`phrase reference_ without closing backquote
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="id2" refid="id1">
            `
        phrase \n\
        <reference name="reference" refname="reference">
            reference
         without closing backquote
    <system_message backrefs="id2" ids="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline interpreted text or phrase reference start-string without end-string.
"""],
["""\
`anonymous phrase reference__ without closing backquote
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="id2" refid="id1">
            `
        anonymous phrase \n\
        <reference anonymous="1" name="reference">
            reference
         without closing backquote
    <system_message backrefs="id2" ids="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline interpreted text or phrase reference start-string without end-string.
"""],
]

totest['embedded_URIs'] = [
["""\
`phrase reference <http://example.com>`_
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="phrase reference" refuri="http://example.com">
            phrase reference
        <target ids="phrase-reference" names="phrase\ reference" refuri="http://example.com">
"""],
["""\
`anonymous reference <http://example.com>`__
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="anonymous reference" refuri="http://example.com">
            anonymous reference
"""],
["""\
`embedded URI on next line
<http://example.com>`__
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="embedded URI on next line" refuri="http://example.com">
            embedded URI on next line
"""],
["""\
`embedded URI across lines <http://example.com/
long/path>`__
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="embedded URI across lines" refuri="http://example.com/long/path">
            embedded URI across lines
"""],
["""\
`embedded URI with whitespace <http://example.com/
long/path /and  /whitespace>`__
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="embedded URI with whitespace" refuri="http://example.com/long/path/and/whitespace">
            embedded URI with whitespace
"""],
["""\
`embedded email address <jdoe@example.com>`__

`embedded email address broken across lines <jdoe
@example.com>`__
""",
"""\
<document source="test data">
    <paragraph>
        <reference name="embedded email address" refuri="mailto:jdoe@example.com">
            embedded email address
    <paragraph>
        <reference name="embedded email address broken across lines" refuri="mailto:jdoe@example.com">
            embedded email address broken across lines
"""],
["""\
`embedded URI with too much whitespace < http://example.com/
long/path /and  /whitespace >`__

`embedded URI with too much whitespace at end <http://example.com/
long/path /and  /whitespace >`__

`embedded URI with no preceding whitespace<http://example.com>`__

`escaped URI \\<http://example.com>`__

See `HTML Anchors: \\<a>`_.
""",
"""\
<document source="test data">
    <paragraph>
        <reference anonymous="1" name="embedded URI with too much whitespace < http://example.com/ long/path /and /whitespace >">
            embedded URI with too much whitespace < http://example.com/
            long/path /and  /whitespace >
    <paragraph>
        <reference anonymous="1" name="embedded URI with too much whitespace at end <http://example.com/ long/path /and /whitespace >">
            embedded URI with too much whitespace at end <http://example.com/
            long/path /and  /whitespace >
    <paragraph>
        <reference anonymous="1" name="embedded URI with no preceding whitespace<http://example.com>">
            embedded URI with no preceding whitespace<http://example.com>
    <paragraph>
        <reference anonymous="1" name="escaped URI <http://example.com>">
            escaped URI <http://example.com>
    <paragraph>
        See \n\
        <reference name="HTML Anchors: <a>" refname="html anchors: <a>">
            HTML Anchors: <a>
        .
"""],
["""\
Relative URIs' reference text can be omitted:

`<reference>`_

`<anonymous>`__
""",
"""\
<document source="test data">
    <paragraph>
        Relative URIs' reference text can be omitted:
    <paragraph>
        <reference name="reference" refuri="reference">
            reference
        <target ids="reference" names="reference" refuri="reference">
    <paragraph>
        <reference name="anonymous" refuri="anonymous">
            anonymous
"""],
]

totest['inline_targets'] = [
["""\
_`target`

Here is _`another target` in some text. And _`yet
another target`, spanning lines.

_`Here is  a    TaRgeT` with case and spacial difficulties.
""",
"""\
<document source="test data">
    <paragraph>
        <target ids="target" names="target">
            target
    <paragraph>
        Here is \n\
        <target ids="another-target" names="another\ target">
            another target
         in some text. And \n\
        <target ids="yet-another-target" names="yet\ another\ target">
            yet
            another target
        , spanning lines.
    <paragraph>
        <target ids="here-is-a-target" names="here\ is\ a\ target">
            Here is  a    TaRgeT
         with case and spacial difficulties.
"""],
[u"""\
l'_`target1` and l\u2019_`target2` with apostrophe
""",
u"""\
<document source="test data">
    <paragraph>
        l'
        <target ids="target1" names="target1">
            target1
         and l\u2019
        <target ids="target2" names="target2">
            target2
         with apostrophe
"""],
[u"""\
quoted '_`target1`', quoted "_`target2`",
quoted \u2018_`target3`\u2019, quoted \u201c_`target4`\u201d,
quoted \xab_`target5`\xbb
""",
u"""\
<document source="test data">
    <paragraph>
        quoted '
        <target ids="target1" names="target1">
            target1
        ', quoted "
        <target ids="target2" names="target2">
            target2
        ",
        quoted \u2018
        <target ids="target3" names="target3">
            target3
        \u2019, quoted \u201c
        <target ids="target4" names="target4">
            target4
        \u201d,
        quoted \xab
        <target ids="target5" names="target5">
            target5
        \xbb
"""],
[u"""\
_`'target1'` with quotes, _`"target2"` with quotes,
_`\u2018target3\u2019` with quotes, _`\u201ctarget4\u201d` with quotes,
_`\xabtarget5\xbb` with quotes
""",
u"""\
<document source="test data">
    <paragraph>
        <target ids="target1" names="'target1'">
            'target1'
         with quotes, 
        <target ids="target2" names=""target2"">
            "target2"
         with quotes,
        <target ids="target3" names="\u2018target3\u2019">
            \u2018target3\u2019
         with quotes, 
        <target ids="target4" names="\u201ctarget4\u201d">
            \u201ctarget4\u201d
         with quotes,
        <target ids="target5" names="\xabtarget5\xbb">
            \xabtarget5\xbb
         with quotes
"""],
["""\
But this isn't a _target; targets require backquotes.

And _`this`_ is just plain confusing.
""",
"""\
<document source="test data">
    <paragraph>
        But this isn't a _target; targets require backquotes.
    <paragraph>
        And \n\
        <problematic ids="id2" refid="id1">
            _`
        this`_ is just plain confusing.
    <system_message backrefs="id2" ids="id1" level="2" line="3" source="test data" type="WARNING">
        <paragraph>
            Inline target start-string without end-string.
"""],
["""\
_`inline target without closing backquote
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="id2" refid="id1">
            _`
        inline target without closing backquote
    <system_message backrefs="id2" ids="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline target start-string without end-string.
"""],
]

totest['footnote_reference'] = [
["""\
[1]_
""",
"""\
<document source="test data">
    <paragraph>
        <footnote_reference ids="id1" refname="1">
            1
"""],
["""\
[#]_
""",
"""\
<document source="test data">
    <paragraph>
        <footnote_reference auto="1" ids="id1">
"""],
["""\
[#label]_
""",
"""\
<document source="test data">
    <paragraph>
        <footnote_reference auto="1" ids="id1" refname="label">
"""],
["""\
[*]_
""",
"""\
<document source="test data">
    <paragraph>
        <footnote_reference auto="*" ids="id1">
"""],
["""\
Adjacent footnote refs are not possible: [*]_[#label]_ [#]_[2]_ [1]_[*]_
""",
"""\
<document source="test data">
    <paragraph>
        Adjacent footnote refs are not possible: [*]_[#label]_ [#]_[2]_ [1]_[*]_
"""],
]

totest['citation_reference'] = [
["""\
[citation]_
""",
"""\
<document source="test data">
    <paragraph>
        <citation_reference ids="id1" refname="citation">
            citation
"""],
["""\
[citation]_ and [cit-ation]_ and [cit.ation]_ and [CIT1]_ but not [CIT 1]_
""",
"""\
<document source="test data">
    <paragraph>
        <citation_reference ids="id1" refname="citation">
            citation
         and \n\
        <citation_reference ids="id2" refname="cit-ation">
            cit-ation
         and \n\
        <citation_reference ids="id3" refname="cit.ation">
            cit.ation
         and \n\
        <citation_reference ids="id4" refname="cit1">
            CIT1
         but not [CIT 1]_
"""],
["""\
Adjacent citation refs are not possible: [citation]_[CIT1]_
""",
"""\
<document source="test data">
    <paragraph>
        Adjacent citation refs are not possible: [citation]_[CIT1]_
"""],
]

totest['substitution_references'] = [
["""\
|subref|
""",
"""\
<document source="test data">
    <paragraph>
        <substitution_reference refname="subref">
            subref
"""],
["""\
|subref|_ and |subref|__
""",
"""\
<document source="test data">
    <paragraph>
        <reference refname="subref">
            <substitution_reference refname="subref">
                subref
         and \n\
        <reference anonymous="1">
            <substitution_reference refname="subref">
                subref
"""],
["""\
|substitution reference|
""",
"""\
<document source="test data">
    <paragraph>
        <substitution_reference refname="substitution reference">
            substitution reference
"""],
["""\
|substitution
reference|
""",
"""\
<document source="test data">
    <paragraph>
        <substitution_reference refname="substitution reference">
            substitution
            reference
"""],
["""\
|substitution reference without closing verbar
""",
"""\
<document source="test data">
    <paragraph>
        <problematic ids="id2" refid="id1">
            |
        substitution reference without closing verbar
    <system_message backrefs="id2" ids="id1" level="2" line="1" source="test data" type="WARNING">
        <paragraph>
            Inline substitution_reference start-string without end-string.
"""],
["""\
first | then || and finally |||
""",
"""\
<document source="test data">
    <paragraph>
        first | then || and finally |||
"""],
]

totest['standalone_hyperlink'] = [
["""\
http://www.standalone.hyperlink.com

http:/one-slash-only.absolute.path

[http://example.com]

(http://example.com)

<http://example.com>

http://[1080:0:0:0:8:800:200C:417A]/IPv6address.html

http://[3ffe:2a00:100:7031::1] (the final "]" is ambiguous in text)

http://[3ffe:2a00:100:7031::1]/

mailto:someone@somewhere.com

news:comp.lang.python

An email address in a sentence: someone@somewhere.com.

ftp://ends.with.a.period.

(a.question.mark@end?)
""",
"""\
<document source="test data">
    <paragraph>
        <reference refuri="http://www.standalone.hyperlink.com">
            http://www.standalone.hyperlink.com
    <paragraph>
        <reference refuri="http:/one-slash-only.absolute.path">
            http:/one-slash-only.absolute.path
    <paragraph>
        [
        <reference refuri="http://example.com">
            http://example.com
        ]
    <paragraph>
        (
        <reference refuri="http://example.com">
            http://example.com
        )
    <paragraph>
        <
        <reference refuri="http://example.com">
            http://example.com
        >
    <paragraph>
        <reference refuri="http://[1080:0:0:0:8:800:200C:417A]/IPv6address.html">
            http://[1080:0:0:0:8:800:200C:417A]/IPv6address.html
    <paragraph>
        <reference refuri="http://[3ffe:2a00:100:7031::1">
            http://[3ffe:2a00:100:7031::1
        ] (the final "]" is ambiguous in text)
    <paragraph>
        <reference refuri="http://[3ffe:2a00:100:7031::1]/">
            http://[3ffe:2a00:100:7031::1]/
    <paragraph>
        <reference refuri="mailto:someone@somewhere.com">
            mailto:someone@somewhere.com
    <paragraph>
        <reference refuri="news:comp.lang.python">
            news:comp.lang.python
    <paragraph>
        An email address in a sentence: \n\
        <reference refuri="mailto:someone@somewhere.com">
            someone@somewhere.com
        .
    <paragraph>
        <reference refuri="ftp://ends.with.a.period">
            ftp://ends.with.a.period
        .
    <paragraph>
        (
        <reference refuri="mailto:a.question.mark@end">
            a.question.mark@end
        ?)
"""],
["""\
Valid URLs with escaped markup characters:

http://example.com/\\*content\\*/whatever

http://example.com/\\*content*/whatever
""",
"""\
<document source="test data">
    <paragraph>
        Valid URLs with escaped markup characters:
    <paragraph>
        <reference refuri="http://example.com/*content*/whatever">
            http://example.com/*content*/whatever
    <paragraph>
        <reference refuri="http://example.com/*content*/whatever">
            http://example.com/*content*/whatever
"""],
["""\
Valid URLs may end with punctuation inside "<>":

<http://example.org/ends-with-dot.>
""",
"""\
<document source="test data">
    <paragraph>
        Valid URLs may end with punctuation inside "<>":
    <paragraph>
        <
        <reference refuri="http://example.org/ends-with-dot.">
            http://example.org/ends-with-dot.
        >
"""],
["""\
Valid URLs with interesting endings:

http://example.org/ends-with-pluses++
""",
"""\
<document source="test data">
    <paragraph>
        Valid URLs with interesting endings:
    <paragraph>
        <reference refuri="http://example.org/ends-with-pluses++">
            http://example.org/ends-with-pluses++
"""],
["""\
None of these are standalone hyperlinks (their "schemes"
are not recognized): signal:noise, a:b.
""",
"""\
<document source="test data">
    <paragraph>
        None of these are standalone hyperlinks (their "schemes"
        are not recognized): signal:noise, a:b.
"""],
["""\
Escaped email addresses are not recognized: test\@example.org
""",
"""\
<document source="test data">
    <paragraph>
        Escaped email addresses are not recognized: test@example.org
"""],
]

totest['miscellaneous'] = [
["""\
__This__ should be left alone.
""",
"""\
<document source="test data">
    <paragraph>
        __This__ should be left alone.
"""],
[r"""
Character-level m\ *a*\ **r**\ ``k``\ `u`:title:\p
with backslash-escaped whitespace, including new\
lines.
""",
"""\
<document source="test data">
    <paragraph>
        Character-level m
        <emphasis>
            a
        <strong>
            r
        <literal>
            k
        <title_reference>
            u
        p
        with backslash-escaped whitespace, including newlines.
"""],
[u"""\
quoted '*emphasis*', quoted "*emphasis*",
quoted \u2018*emphasis*\u2019, quoted \u201c*emphasis*\u201d,
quoted \xab*emphasis*\xbb
""",
u"""\
<document source="test data">
    <paragraph>
        quoted '
        <emphasis>
            emphasis
        ', quoted "
        <emphasis>
            emphasis
        ",
        quoted \u2018
        <emphasis>
            emphasis
        \u2019, quoted \u201c
        <emphasis>
            emphasis
        \u201d,
        quoted \xab
        <emphasis>
            emphasis
        \xbb
"""],
[u"""\
text-*separated*\u2010*by*\u2011*various*\u2012*dashes*\u2013*and*\u2014*hyphens*.
\u00bf*punctuation*? \u00a1*examples*!\u00a0*too*.
""",
u"""\
<document source="test data">
    <paragraph>
        text-
        <emphasis>
            separated
        \u2010
        <emphasis>
            by
        \u2011
        <emphasis>
            various
        \u2012
        <emphasis>
            dashes
        \u2013
        <emphasis>
            and
        \u2014
        <emphasis>
            hyphens
        .
        \xbf
        <emphasis>
            punctuation
        ? \xa1
        <emphasis>
            examples
        !\xa0
        <emphasis>
            too
        .
"""],
[u"""\
None of these should be markup (matched openers & closers):

\u2018*\u2019 \u201c*\u201d \xab*\xbb \u00bf*? \u00a1*!

But this should:

l\u2019*exception*.
""",
u"""\
<document source="test data">
    <paragraph>
        None of these should be markup (matched openers & closers):
    <paragraph>
        \u2018*\u2019 \u201c*\u201d \xab*\xbb \xbf*? \xa1*!
    <paragraph>
        But this should:
    <paragraph>
        l\u2019
        <emphasis>
            exception
        .
"""],
]


if __name__ == '__main__':
    import unittest
    unittest.main(defaultTest='suite')
