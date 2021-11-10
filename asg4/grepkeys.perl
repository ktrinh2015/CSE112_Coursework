#!/usr/bin/perl
while (<>) {
   $keys{$&} = 1 while s/{[A-Z]+}//;
}
print "$_\n" for sort keys %keys;
