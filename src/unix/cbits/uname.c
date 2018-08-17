#include <sys/utsname.h>

int haskell_uname(struct utsname *name)
{
    return uname(name);
}
