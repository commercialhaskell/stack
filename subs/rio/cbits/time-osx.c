#include <mach/mach.h>
#include <mach/mach_time.h>

static mach_timebase_info_data_t timebase_info;
static double timebase_recip;

void rio_inittime(void)
{
    if (timebase_recip == 0) {
	mach_timebase_info(&timebase_info);
	timebase_recip = (timebase_info.denom / timebase_info.numer) / 1e9;
    }
}

static double to_double(time_value_t time)
{
    return time.seconds + time.microseconds / 1e6;
}

double rio_getcputime(void)
{
    struct task_thread_times_info thread_info_data;
    mach_msg_type_number_t thread_info_count = TASK_THREAD_TIMES_INFO_COUNT;
    kern_return_t kr = task_info(mach_task_self(),
				 TASK_THREAD_TIMES_INFO,
				 (task_info_t) &thread_info_data,
				 &thread_info_count);
    return (to_double(thread_info_data.user_time) +
	    to_double(thread_info_data.system_time));
}
