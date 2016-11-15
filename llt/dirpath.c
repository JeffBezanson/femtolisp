#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "dtypes.h"

#ifdef WIN32
#include <malloc.h>
#include <sys/timeb.h>
#include <windows.h>
#undef NO_ERROR
#undef MOD_SHIFT
#undef TRUE
#undef FALSE
#undef VOID
#else
#include <sys/time.h>
#include <sys/poll.h>
#include <unistd.h>
#endif

#include "dirpath.h"

void get_cwd(char *buf, size_t size)
{
#ifndef WIN32
    getcwd(buf, size);
#else
    GetCurrentDirectory(size, buf);
#endif
}

int set_cwd(char *buf)
{
#ifndef WIN32
    if (chdir(buf) == -1)
        return 1;
#else
    if (SetCurrentDirectory(buf) == 0)
        return 1;
#endif
    return 0;
}

// destructively convert path to directory part
void path_to_dirname(char *path)
{
    char *sep = strrchr(path, PATHSEP);
    if (sep != NULL) {
        *sep = '\0';
    }
    else {
        path[0] = '\0';
    }
}

#ifdef LINUX
char *get_exename(char *buf, size_t size)
{
    char linkname[64]; /* /proc/<pid>/exe */
    pid_t pid;
    ssize_t ret;

    /* Get our PID and build the name of the link in /proc */
    pid = getpid();

    if (snprintf(linkname, sizeof(linkname), "/proc/%i/exe", pid) < 0)
        return NULL;

    /* Now read the symbolic link */
    ret = readlink(linkname, buf, size);

    /* In case of an error, leave the handling up to the caller */
    if (ret == -1)
        return NULL;

    /* Report insufficient buffer size */
    if ((size_t)ret >= size)
        return NULL;

    /* Ensure proper NUL termination */
    buf[ret] = 0;

    return buf;
}
#elif defined(OPENBSD)
#include <sys/param.h>
#include <sys/sysctl.h>

char *get_exename(char *buf, size_t size)
{
    int mib[4];
    pid_t pid;
    size_t len, plen;
    char  **argv, **argv2;
    char *p, *path, *pathcpy, filename[PATH_MAX];
    struct stat sbuf;

    pid = getpid();

    mib[0] = CTL_KERN;
    mib[1] = KERN_PROC_ARGS;
    mib[2] = pid;
    mib[3] = KERN_PROC_ARGV;

    buf = NULL;
    argv = NULL;
    len = 128;

    // Now, play The Guessing Game with sysctl(3) as to how big argv
    // is supposed to be. (It's loads of fun for the whole family!)

    while (len < SIZE_MAX / 2) {
        len *= 2;
        if ((argv2 = realloc(argv, len)) == NULL)
            break;
        argv = argv2;
        if (sysctl(mib, 4, argv, &len, NULL, 0) == -1) {
            if (errno == ENOMEM)
                continue; // Go back and realloc more memory.
            break; // Bail for some other error in sysctl(3).
        }
        // If you made it here, congrats! You guessed right!
        if (*argv != NULL)
            buf = strdup(*argv);
        break;
    }
    free(argv);

    // If no error occurred in the sysctl(3) KERN_PROC_ARGV call
    // above, then buf at this point contains some kind of pathname.

    if (buf != NULL) {
	if (strchr(buf, '/') == NULL) {
	    // buf contains a `basename`-style pathname (i.e. "foo",
	    // as opposed to "../foo" or "/usr/bin/foo"); search the
	    // PATH for its location. (BTW the setgid(2), setuid(2)
	    // calls are a pre-condition for the access(2) call
	    // later.)

	    if ( (path = getenv("PATH")) != NULL &&
		 !setgid(getegid()) && !setuid(geteuid()) ) {

		// The strdup(3) call below, if successful, will
		// allocate memory for the PATH string returned by
		// getenv(3) above.  This is necessary because the man
		// page of getenv(3) says that its return value
		// "should be considered read-only"; however, the
		// strsep(3) call below is going to be destructively
		// modifying that value. ("Hulk smash!")

		if ((path = strdup(path)) != NULL) {
		    pathcpy = path;
		    len = strlen(buf);
		    while ((p = strsep(&pathcpy, ":")) != NULL) {
			if (*p == '\0') p = ".";
			plen = strlen(p);

			// strip trailing '/'
			while (p[plen-1] == '/') p[--plen] = '\0';

			if (plen + 1 + len < sizeof(filename)) {
			    snprintf(filename, sizeof(filename), "%s/%s", p, buf);
			    if ( (stat(filename, &sbuf) == 0) &&
				 S_ISREG(sbuf.st_mode) &&
				 access(filename, X_OK) == 0 ) {
				buf = strdup(filename);
				break;
			    }
			}
		    }
		    free(path); // free the strdup(3) memory allocation.
		}
	    }
	    else buf = NULL; // call to getenv(3) or [sg]ete?[ug]id(2) failed.
	}
	if ( buf != NULL && *buf != '/' ) {
	    // buf contains a relative pathname (e.g. "../foo");
	    // resolve this to an absolute pathname.
	    if ( strlcpy(filename, buf, sizeof(filename)) >= sizeof(filename) ||
		 realpath(filename, buf) == NULL )
		buf = NULL; 
	}
    }

    return buf;
}
#elif defined(FREEBSD)
#include <sys/types.h>
#include <sys/sysctl.h>

char *get_exename(char *buf, size_t size)
{
  int mib[4];
  mib[0] = CTL_KERN;
  mib[1] = KERN_PROC;
  mib[2] = KERN_PROC_PATHNAME;
  mib[3] = -1;
  sysctl(mib, 4, buf, &size, NULL, 0);
 
  return buf;
}
#elif defined(WIN32)
char *get_exename(char *buf, size_t size)
{
    if (GetModuleFileName(NULL, buf, size) == 0)
        return NULL;

    return buf;
}
#elif defined(MACOSX)
#include <mach-o/dyld.h>
char *get_exename(char *buf, size_t size)
{
    uint32_t bufsize = (uint32_t)size;
    if (_NSGetExecutablePath(buf, &bufsize))
	return NULL;
    return buf;
}
#endif
