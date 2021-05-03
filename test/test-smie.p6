#!/usr/bin/env raku
qq:to/HERE/;
    This is text that should not
  have its indention changed
    by emacs. (Except maybe to have left overhanging lines
    adjusted.)
    HERE

my %var =
   A => 1,
   :b<2>,
   :3c,
   D => 4;
