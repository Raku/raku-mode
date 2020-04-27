# Raku Mode

[![License GPL 3][badge-license]][copying]
[![MELPA Status](http://melpa.org/packages/perl6-mode-badge.svg)](http://melpa.org/#/perl6-mode)
[![travis][badge-travis]][travis]

Raku mode lets you edit Raku code with [GNU Emacs][] 24.

This mode needs GNU Emacs 24.4.

## Features

* Basic syntax highlighting
* Basic indentation
* Identifier index menu (variables, subs, classes, etc.)
* REPL interaction

#### Planned

* Complete syntax highlighting
* Better indentation support (uses Emacs SMIE grammar, see the Emacs manual)
* Help system
* ETags support
* `find-file-at-point` for module names
* Electricity (`electric-pair-mode` needs some context-sensitive help)
* Unicode character map
* Better HEREDOC support (currently not very stable)

#### Not planned

* Syntax checking (use [flycheck-raku][])

## Installation

With [`use-package`][use-package] in your init file:

```el
(use-package raku-mode
  :ensure t
  :defer t)
```

Or in your [`Cask`][cask] file:

```el
(source melpa)

(depends-on "raku-mode")
```
Or manually from [MELPA][] with <kbd>M-x package-refresh-contents</kbd>
and <kbd>M-x package-install RET raku-mode</kbd>.

## Usage

Just visit Raku files.

The major mode will be autoloaded whenever a Raku file is visited.
This includes any file with `raku` in the shebang, as well as any file
with a `.p6`, `.pm6`, or `.pl6` extension. It also applies to any `.pm`,
`.pl`, and `.t` files whose first line of code looks like Raku.

Start the REPL with <kbd>M-x run-raku RET</kbd>. The following
keybindings are available to interact with the REPL:

* <kbd>C-c C-c</kbd>: Send the current line to the REPL
* <kbd>C-c C-r</kbd>: Send the selected region to the REPL
* <kbd>C-c C-h</kbd>: Send the whole buffer to the REPL

The REPL will start if needed with this keybindings.

Use <kbd>M-x customize-group RET raku</kbd> to customize Raku Mode.

## Template skeletons

Included are two skeletons (file templates) that can be auto-inserted with
auto-insert-mode:

* `raku-script-skeleton`, and
* `raku-module-skeleton`.

To use them, add them to your auto-insert-alist (`M-x customize-option RET
auto-insert-alist`) with the conditions of your choice.

To insert them when you create a new file with the `.raku` or `.rakumod`
extension, use the following matching regular expressions:

* For `raku-script-skeleton`: `\.raku\'`.
* For `raku-module-skeleton`: `\.rakumod\`.

Alternatively you can add them in your .emacs using `define-auto-insert`:

```Emacs Lisp
(define-auto-insert
	'("\\.rakumod\\'" . "Raku module skeleton")
	'raku-module-skeleton)
(define-auto-insert
	'("\\.raku\\'" . "Raku script skeleton")
	'raku-script-skeleton)
```

The full path to the Raku executable for the shebang, as well as the default
auth information for a module can be defined in the Raku Skeleton customization
group, `M-x customize-group RET raku-skeleton`.

## Contribute

Pull requests are welcome.

You might want to install [`cask`][cask] so you can run the test suite
(with `make test`).

## Original Code

The original version of this code can be found at
[https://github.com/hinrik/perl6-mode](https://github.com/hinrik/perl6-mode)

## License

Raku Mode is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Raku Mode is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
Public License for more details.

See [`COPYING`][copying] for the complete license.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[COPYING]: https://github.com/hinrik/perl6-mode/blob/master/COPYING
[travis]: https://travis-ci.org/hinrik/perl6-mode
[badge-travis]: https://travis-ci.org/hinrik/perl6-mode.svg?branch=master
[GNU Emacs]: https://www.gnu.org/software/emacs/
[flycheck-raku]: https://github.com/widefox/flycheck-raku
[MELPA]: http://melpa.milkbox.net/
[use-package]: https://github.com/jwiegley/use-package
[Cask]: http://cask.github.io/
[Issue tracker]: https://github.com/hinrik/perl6-mode/issues
