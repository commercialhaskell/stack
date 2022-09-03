<div class="hidden-warning"><a href="https://docs.haskellstack.org/"><img src="https://cdn.jsdelivr.net/gh/commercialhaskell/stack/doc/img/hidden-warning.svg"></a></div>

# Signing key

Each released Stack executable is signed with either:

* the GPG key with ID 0x575159689BEFB442; or
* the GPG key of a person that has been authorised by the GPG key with ID
  0x575159689BEFB442.

The signature is in an `*.asc` file. For example:

~~~text
stack-2.7.5-linux-x86_64-bin
stack-2.7.5-linux-x86_64-bin.asc
~~~

The signature can be verified with GPG, as follows:

~~~text
# Receive the public key from a keyserver
gpg --keyserver keyserver.ubuntu.com --recv-keys 0x575159689BEFB442
# Get information about the key
gpg --keyid-format long --list-keys 0x575159689BEFB442
pub   rsa2048/575159689BEFB442 2015-06-02 [SC]
      C5705533DA4F78D8664B5DC0575159689BEFB442
uid                 [ unknown] FPComplete <dev@fpcomplete.com>
sub   rsa2048/85A738994664AB89 2015-06-02 [E]

# Attempt to verify the file using the signature file. The public key has not
# yet been certified with a trusted signature.
gpg --verify stack-2.7.5-linux-x86_64-bin.asc stack-2.7.5-linux-x86_64-bin
gpg: Signature made 06/03/2022 15:15:21 GMT Standard Time
gpg:                using RSA key C5705533DA4F78D8664B5DC0575159689BEFB442
gpg: Good signature from "FPComplete <dev@fpcomplete.com>" [unknown]
gpg: WARNING: This key is not certified with a trusted signature!
gpg:          There is no indication that the signature belongs to the owner.
Primary key fingerprint: C570 5533 DA4F 78D8 664B  5DC0 5751 5968 9BEF B442
~~~

The GPG key with ID 0x575159689BEFB442, and keys it has signed, have been
uploaded to the
[Ubuntu Keyserver](https://keyserver.ubuntu.com/pks/lookup?search=0x575159689BEFB442&fingerprint=on&op=index).

This is the public key block for GPG key ID 0x575159689BEFB442:

~~~text
-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: GnuPG v1

mQENBFVs+cMBCAC5IsLWTikd1V70Ur1FPJMn14Sc/C2fbXc0zRcPuWX+JaXgrIJQ
74A3UGBpa07wJDZiQLLz4AasDQj++9gXdiM9MlK/xWt8BQpgQqSMgkktFVajSWX2
rSXPjqLtsl5dLsc8ziBkd/AARXoeITmXX+n6oRTy6QfdMv2Tacnq7r9M9J6bAz6/
7UsKkyZVwsbUPea4SuD/s7jkXAuly15APaYDmF5mMlpoRWp442lJFpA0h52mREX1
s5FDbuKRQW7OpZdLcmOgoknJBDSpKHuHEoUhdG7Y3WDUGYFZcTtta1qSVHrm3nYa
7q5yOzPW4/VpftkBs1KzIxx0nQ5INT5W5+oTABEBAAG0H0ZQQ29tcGxldGUgPGRl
dkBmcGNvbXBsZXRlLmNvbT6JATcEEwEKACEFAlVs+cMCGwMFCwkIBwMFFQoJCAsF
FgMCAQACHgECF4AACgkQV1FZaJvvtEIP8gf/S/k4C3lp/BFb0K9DHHSt6EaGQPwy
g+O8d+JvL7ghkvMjlQ+UxDw+LfRKANTpl8a4vHtEQLHEy1tPJfrnMA8DNci8HLVx
rK3lIqMfv5t85VST9rz3X8huSw7qwFyxsmIqFtJC/BBQfsOXC+Q5Z2nbResXHMeA
5ZvDopZnqKPdmMOngabPGZd89hOKn6r8k7+yvZ/mXmrGOB8q5ZGbOXUbCshst7lc
yZWmoK3VJdErQjGHCdF4MC9KFBQsYYUy9b1q0OUv9QLtq/TeKxfpvYk9zMWAoafk
M8QBE/qqOpqkBRoKbQHCDQgx7AXJMKnOA0jPx1At57hWl7PuEH4rK38UtLkBDQRV
bPnDAQgAx1+4ENyaMk8XznQQ4l+nl8qw4UedZhnR5Xxr6z2kcMO/0VdwmIDCpxaM
spurOF+yExfY/Chbex7fThWTwVgfsItUc/QLLv9jkvpveMUDuPyh/4QrAQBYoW09
jMJcOTFQU+f4CtKaN/1PNoTSU2YkVpbhvtV3Jn2LPFjUSPb7z2NZ9NKe10M0/yN+
l0CuPlqu6GZR5L3pA5i8PZ0Nh47j0Ux5KIjrjCGne4p+J8qqeRhUf04yHAYfDLgE
aLAG4v4pYbb1jNPUm1Kbk0lo2c3dxx0IU201uAQ6LNLdF/WW/ZF7w3iHn7kbbzXO
jhbq2rvZEn3K9xDr7homVnnj21/LSQARAQABiQEfBBgBCgAJBQJVbPnDAhsMAAoJ
EFdRWWib77RC3ukH/R9jQ4q6LpXynQPJJ9QKwstglKfoKNpGeAYVTEn0e7NB0HV5
BC+Da5SzBowboxC2YCD1wTAjBjLLQfAYNyR+tHpJBaBmruafj87nBCDhSWwWDXwx
OUDpNOwKUkrwZDRlM7n4byoMRl7Vh/7CXxaTqkyao1c5v3mHh/DremiTvOJ4OXgJ
77NHaPXezHkCFZC8/sX6aY0DJxF+LIE84CoLI1LYBatH+NKxoICKA+yeF3RIVw0/
F3mtEFEtmJ6ljSks5tECxfJFvQlkpILBbGvHfuljKMeaj+iN+bsHmV4em/ELB1ku
N9Obs/bFDBMmQklIdLP7dOunDjY4FwwcFcXdNyg=
=YUsC
-----END PGP PUBLIC KEY BLOCK-----
~~~
