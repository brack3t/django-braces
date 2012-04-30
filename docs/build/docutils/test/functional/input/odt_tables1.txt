================================================================================
Grid test
================================================================================

Test 1
=======

+------------------------+------------+----------+----------+
| Header row, column 1   | Header 2   | Header 3 | Header 4 |
| (header rows optional) |            |          |          |
+========================+============+==========+==========+
| body row 1, column 1   | column 2   | column 3 | column 4 |
+------------------------+------------+----------+----------+
| body row 2             | Cells may span columns.          |
+------------------------+------------+---------------------+
| body row 3             | Cells may  | - Table cells       |
+------------------------+ span rows. + - contain           +
| body row 4             | aaa        | - body elements.    |
+------------------------+------------+---------------------+

Test 2
=======

+------------------------+------------+----------+----------+
| Header row, column 1   | Header 2   | Header 3 | Header 4 |
| (header rows optional) |            |          |          |
+========================+============+==========+==========+
| body row 1, column 1   | column 2   | column 3 | column 4 |
+                        +------------+----------+----------+
| body row 2             | column 2   | column 3 | column 4 |
+------------------------+            +----------+----------+
| body row 3             | may span   | - Table cells       |
+------------------------+------------+ - contain           +
| body row 4             | column 2   | - body elements.    |
+------------------------+------------+---------------------+


Test 3
=======

+------------------------+------------+----------+----------+
| Header row, column 1   | Header 2   | Header 3 | Header 4 |
| (header rows optional) |            |          |          |
+========================+============+==========+==========+
| body row 1, column 1   | column 2   | column 3 | column 4 |
+                        +------------+----------+          +
| body row 2             | column 2   | column 3 | may span |
+------------------------+------------+          +----------+
| body row 3             | column 2   | may span | column 4 |
+------------------------+            +----------+----------+
| body row 4             | may span   | column 3 | column 4 |
+------------------------+------------+----------+----------+


Test 4
=======

+------------------------+------------+----------+----------+
| Header row, column 1   | Header 2   | Header 3 | Header 4 |
| (header rows optional) |            |          |          |
+========================+============+==========+==========+
| body row 1, column 1   | column 2   | column 3 | column 4 |
+                        +------------+----------+          +
| body row 2             | column 2 and column 3 | may span |
+------------------------+                       +----------+
| body row 3             |  may             span | column 4 |
+                        +------------+----------+----------+
| may span               | may span   | column 3 | column 4 |
+------------------------+------------+----------+----------+

Test 4a
=======

+------------------------+------------+----------+----------+
| Header row, column 1   | Header 2   | Header 3 | Header 4 |
| (header rows optional) |            |          |          |
+========================+============+==========+==========+
| body row 1, column 1   | column 2   | column 3 | column 4 |
+                        +------------+----------+          +
| body row 2             | column 2 and column 3 | may span |
+------------------------+                       +----------+
| body row 3             |  may             span | column 4 |
+                        +------------+----------+----------+
| may span               | may span   | column 3 | column 4 |
+------------------------+------------+----------+----------+
| body row 4             | column 2   | column 3 | column 4 |
+------------------------+------------+----------+----------+








Test 5
=======

+------------------------+------------+----------+----------+
| Header row, column 1   | Header 2   | Header 3 | Header 4 |
| (header rows optional) |            |          |          |
+========================+============+==========+==========+
| body row 1, column 1   | column 2   | column 3 | column 4 |
+------------------------+------------+----------+----------+
| body row 2             | Cells may span columns.          |
+------------------------+------------+---------------------+
| body row 3             | Cells may  | - Table cells       |
+------------------------+ span rows. | - contain           |
| body row 4             |            | - body elements.    |
+------------------------+------------+---------------------+


Test 6
=======

Some care must be taken with grid tables to avoid undesired
interactions with cell text in rare cases. For example, the
following table contains a cell in row 2 spanning from column 2 to
column 4:

+--------------+----------+-----------+-----------+
| row 1, col 1 | column 2 | column 3  | column 4  |
+--------------+----------+-----------+-----------+
| row 2        |                                  |
+--------------+----------+-----------+-----------+
| row 3        |          |           |           |
+--------------+----------+-----------+-----------+

Test 7
=======

If a vertical bar is used in the text of that cell, it could have
unintended effects if accidentally aligned with column boundaries:

+--------------+----------+-----------+-----------+
| row 1, col 1 | column 2 | column 3  | column 4  |
+--------------+----------+-----------+-----------+
| row 2        | Use the command ``ls \| more``.  |
+--------------+----------+-----------+-----------+
| row 3        |          |           |           |
+--------------+----------+-----------+-----------+

Test 8
=======

Several solutions are possible. All that is needed is to break the
continuity of the cell outline rectangle. One possibility is to
shift the text by adding an extra space before:


+--------------+----------+-----------+-----------+
| row 1, col 1 | column 2 | column 3  | column 4  |
+--------------+----------+-----------+-----------+
| row 2        |  Use the command ``ls | more``.  |
+--------------+----------+-----------+-----------+
| row 3        |          |           |           |
+--------------+----------+-----------+-----------+

Test 9
=======

Another possibility is to add an extra line to row 2:

+--------------+----------+-----------+-----------+
| row 1, col 1 | column 2 | column 3  | column 4  |
+--------------+----------+-----------+-----------+
| row 2        | Use the command ``ls | more``.   |
|              |                                  |
+--------------+----------+-----------+-----------+
| row 3        |          |           |           |
+--------------+----------+-----------+-----------+
