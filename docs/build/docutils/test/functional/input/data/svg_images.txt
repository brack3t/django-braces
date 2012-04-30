SVG Images
----------

.. image:: ../../../docs/user/rst/images/biohazard.svg
   :width: 48 px
   :height: 48 px

Scalable vector graphics (SVG) images are not supported by all backends.
Rendering depends partially on the backend, especially if the size is
not explicitely given.

.. image:: ../../../docs/user/rst/images/title-scaling.svg
   :width: 50%
   :align: left

A scaling image occupying 50% of the line width (scales with the
browser window).

Whether an SVG image is scaled or clipped/padded cannot be set in the
containing HTML. It depends on the viewport declaration inside its
root <svg> element.

.. |inline-svg| image:: ../../../docs/user/rst/images/biohazard-scaling.svg
   :height: 0.8 em

An inline SVG image |inline-svg| scaled to a height of 0.8 em.

.. image:: ../../../docs/user/rst/images/title-scaling.svg
   :width: 50 %
   :height: 1.2 em
   :align: right
   
A scaling image occupying 50% of the line width and 1.2 em high,
right aligned (this SVG image keeps the aspect ratio):

.. image:: ../../../docs/user/rst/images/biohazard-scaling.svg
   :height: 1 em
   :align: left

A scaling image 1 em high, left aligned.

A scaling image 5 mm x 5 mm, centered, with hyperlink reference:

.. image:: ../../../docs/user/rst/images/biohazard-scaling.svg
   :target: Directives_
   :width: 5 mm
   :height: 5 mm
   :align: center

.. image:: ../../../docs/user/rst/images/biohazard.svg
   :width: 4 cm
   :height: 2 em
   :align: left

A fixed-size image in a  4 cm x 2 em box.

.. image:: ../../../docs/user/rst/images/title.svg
   :width: 50%
   :height: 15 px
   :align: left

A fixed-size image in a box 50% the line width and 15 pixle high.

.. figure:: ../../../docs/user/rst/images/title.svg
   :alt: reStructuredText, the markup syntax
   :width:  290 px
   :height:  28 px

   SVG image in a figure.
