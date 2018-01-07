#include <time.h>

void rio_inittime(void)
{
}


double rio_getcputime(void)
{
    struct timespec ts;

    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);

    return ts.tv_sec + ts.tv_nsec * 1e-9;
}
