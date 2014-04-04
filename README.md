# a2ps-multibyte.el

This package allows printing Emacs buffers using `a2ps`, even when they use an UTF-8 charset or another multibyte character coding system (such encodings are otherwise not supported by `a2ps`).

This only works if the buffer contents could have been encoded using a supported 8-bit coding system (e.g. a French text stored as UTF-8, but featuring only ISO-8859-15 characters).


## Installation

```lisp
(add-to-list 'load-path "/path/to/a2ps-multibyte-package")
(require 'a2ps-multibyte)
```

You can optionally bind the `a2ps` commands to keys:

```lisp
(global-set-key (kbd "<print>")   'a2ps-buffer)
(global-set-key (kbd "S-<print>") 'a2ps-region)
```


## Basic usage

`a2ps-multibyte` reimplements the same commands as the Emacs' standard `a2ps` package. You can thus use the usual interface:

- `M-x a2ps-buffer`
- `M-x a2ps-region`

Giving these commands a prefix argument allows specifying additional switches to be used in the `a2ps` command line.

If all characters in the buffer/region cannot be coded using the coding system specified by `a2ps-encoding`, you will be proposed to provide another.


## Customization

This package can be customized _via_ Emacs' standard interface: `M-x customize-group a2ps`

Here are the relevant variables:

- `a2ps-command`: path to the `a2ps` utility.
- `a2ps-switches`: list of additional command-line switches, e.g.

   ```lisp
   (setq a2ps-switches '("-l" "100"))
   ```

- `a2ps-encoding`: 8-bit encoding to try by default when printing files. If you mostly work with documents in a language supported by an 8-bit encoding, this is the one you should specify here. E.g.

  ```lisp
  (setq a2ps-encoding 'iso-8859-15-unix) ;; Latin-1 charset
  ```

  Try running `M-x list-coding-systems` to see the full list of supported encodings.

- `a2ps-filter-functions`: list of functions to run before printing the buffer. These functions should be callable without argument, and should modify the contents of the current buffer. They will be called in the context of a temporary buffer, so that the original buffer is left untouched.


## Contributing

If you make improvements to this code or have suggestions, please do not hesitate to fork the repository or submit bug reports on [github](https://github.com/ffevotte/a2ps-multibyte.el). The repository's URL is:

    https://github.com/ffevotte/a2ps-multibyte.el.git


## License

Copyright (C) 2012 François Févotte.

Large parts of this code come from `a2ps-print.el` in the [a2ps package](http://www.gnu.org/software/a2ps/) and are (C) Bruce Ingalls.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.
