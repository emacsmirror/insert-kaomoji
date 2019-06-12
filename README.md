`insert-kaomoji.el`
==================

This packages collects and categorises eastern emoticons, called
"kaomojis", as to make it easy to insert them into text within Emacs.

All the kaomojis are stored in a secondary file, which should be placed
in the same directory as `insert-kaomoji.el`. The file format itself is
described below.

How to use
----------

Using MELPA and `use-package`, a minimal setup might look something like
this:

	(use-package insert-kaomoji
	  :bind ("C-x 8 k" . insert-kaomoji))

`KAOMOJIS` format
-----------------

The `KAOMOJIS` file has to use a slightly peculiar format, since usual
delimiters such as newlines, tabs, commas, etc. all can appear in a
emoticon. For that reason, the format instead uses special [ASCII
control characters][ascii delim], code 29 to 31.

These designate:

- **29 (group separator):** A category of emoticons ("Happy", "Sad",
  etc.)
- **30 (record separator):** Divide a category into labels (first part)
  and content (latter part). Only one record separator is allowed per
  group.
- **31 (unit separator):** Within the labels part, it separates
  synonyms, and in the content the single emoticons. Note that all
  synonyms are stripped of surrounding white spaces, _allowing_ new
  categories to start on new lines.

Copying
-------

`bang.el` is distributed under the [CC0 1.0 Universal (CC0 1.0) Public
Domain Dedication][cc0] license.

[ascii delim]: https://en.wikipedia.org/wiki/Delimiter#ASCII_delimited_text
[cc0]: https://creativecommons.org/publicdomain/zero/1.0/deed
