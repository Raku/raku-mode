# file: test-imenu.p6

# Raku syntax file for testing raku-mode with imenu support, which is located at:
#
#   https://github.com/tbrowder/raku-mode [branch: "my-branch"]

my $a;
my @b;
our %c;

my $a-a;
my $a'a_3-z;

 sub a(){my @ze}
 multi sub x() {}
method d() {}
my multi method z() {}
multi c() {}

proto xx() {}

class My-class1 {}
class My-class2{
    class doit () {}
