Hyperlinks and -targets
-----------------------

In LaTeX, we must set an explicit anchor (``\phantomsection``) for a
_`hypertarget in plain text` or in a figure but not in a longtable or
caption:

.. _`table label`:

.. table:: Table with _`hypertarget in table title`.

   =====  ===== =====
   False  True  None
   =====  ===== =====

.. _`figure label`:

.. figure:: ../../../docs/user/rst/images/biohazard.png

   Figure with _`hypertarget in figure caption`.

   Legend with _`hypertarget in figure legend`.

.. _`image label`:

.. image::  ../../../docs/user/rst/images/biohazard.png

See `hypertarget in plain text`_,
`table label`_, `hypertarget in table title`_,
`figure label`_, `hypertarget in figure caption`_,
`hypertarget in figure legend`_, and
`image label`_.
