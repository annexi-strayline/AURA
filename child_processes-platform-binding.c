/*****************************************************************************
**                                                                          **
**                     Ada User Repository Annex (AURA)                     **
**                ANNEXI-STRAYLINE Reference Implementation                 **
**                                                                          **
** ************************************************************************ **
**                                                                          **
**  Copyright (C) 2020, ANNEXI-STRAYLINE Trans-Human Ltd.                   **
**  All rights reserved.                                                    **
**                                                                          **
**  Original Contributors:                                                  **
**  * Richard Wai (ANNEXI-STRAYLINE)                                        **
**                                                                          **
**  Redistribution and use in source and binary forms, with or without      **
**  modification, are permitted provided that the following conditions are  **
**  met:                                                                    **
**                                                                          **
**      * Redistributions of source code must retain the above copyright    **
**        notice, this list of conditions and the following disclaimer.     **
**                                                                          **
**      * Redistributions in binary form must reproduce the above copyright **
**        notice, this list of conditions and the following disclaimer in   **
**        the documentation and/or other materials provided with the        **
**        distribution.                                                     **
**                                                                          **
**      * Neither the name of the copyright holder nor the names of its     **
**        contributors may be used to endorse or promote products derived   **
**        from this software without specific prior written permission.     **
**                                                                          **
**  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     **
**  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       **
**  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A **
**  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT      **
**  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   **
**  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT        **
**  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,   **
**  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY   **
**  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT     **
**  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE   **
**  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.    **
**                                                                          **
*****************************************************************************/

/* This is the POSIX-y verion. POSIX-y because it uses the dup2 syscalls    */
/* which is not POSIX, but is identical on Linux, BSD, and Solaris          */

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <time.h>
#include <sys/types.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <errno.h>

#include "platform_info.h"

/* For pipes, and readability                              */
/* Many modern systems only have bi-directional pipes, but */
/* sometimes tradition is harmless                         */
#define READ_END  0
#define WRITE_END 1


/* procedure fork_and_exec (path: in     char_array;       */
/*                          args: in     chars_ptr_array;  */
/*                          wdir: in     char_array;       */
/*                                                         */
/*                          pid :    out Process_ID;       */
/*                          stdin:   out Stream_Handle;    */
/*                          stdout:  out Stream_Handle;    */
/*                          stderr:  out Stream_Handle);   */
/* with                                                    */
/*   Import        => True,                                */
/*   Convention    => C,                                   */
/*   External_Name => "__chldproc_platform_fork_and_exec"; */

extern int __chldproc_platform_fork_and_exec
(char  * path, char ** args, char * wdir,
 pid_t * pid,  int  * stdin, int  * stdout, int * stderr)
{
     pid_t event_horizon;
     
     int stdin_pipe[2];
     int stdout_pipe[2];
     int stderr_pipe[2];

     int modflags;
     int retval;
     
     /* Set up new pipes to talk to the child */
     pipe (stdin_pipe);
     pipe (stdout_pipe);
     pipe (stderr_pipe);

     /* Here we go */
     event_horizon = fork ();

     if (event_horizon < 0)
     {
          /* Fork failed */
          return (-1);
     }
     else if (event_horizon > 0)
     {
          /* We are the parent */

          /* Close the other ends */
          close (stdin_pipe[READ_END]  );
          close (stdout_pipe[WRITE_END]);
          close (stderr_pipe[WRITE_END]);
          
          *pid = event_horizon;
          
          /* Child's stdin, so we take the write-end     */
          *stdin  = stdin_pipe[WRITE_END];

          /* Child's stdout/err, so we take the read-end */
          *stdout = stdout_pipe[READ_END];
          *stderr = stderr_pipe[READ_END];

          /* Set non-blocking on the read pipes */
          modflags = fcntl ( *stdin, F_GETFL );
          fcntl ( *stdin, F_SETFL, modflags | O_NONBLOCK );
          
          modflags = fcntl ( *stdout, F_GETFL );
          fcntl ( *stdout, F_SETFL, modflags | O_NONBLOCK );

          modflags = fcntl ( *stderr, F_GETFL );
          fcntl ( *stderr, F_SETFL, modflags | O_NONBLOCK );

          return (0);
     }

     /* We are the child */

     /* Close the other ends */
     close (stdin_pipe[WRITE_END]);
     close (stdout_pipe[READ_END]);
     close (stderr_pipe[READ_END]);
     
     /* Bring the pipes down to our stdio */
     dup2 ( stdin_pipe[READ_END],   STDIN_FILENO  );
     dup2 ( stdout_pipe[WRITE_END], STDOUT_FILENO );
     dup2 ( stderr_pipe[WRITE_END], STDERR_FILENO );

     /* Set-up working directory */
     if (chdir (wdir))
     {
          perror ("chdir failed after fork: ");
          _exit (-1);
     }

     /* Do exec! */
     execv ( path, args );

     /* If we get here, the exec failed */
     perror ("exec failed after fork: ");
     _exit (-1);
     
}


/* function wait_pid_terminate (pid: Process_ID) return int with  */
/*   Import => True,                                              */
/*   Convention => C,                                             */
/*   External_Name => "__chldproc_platform_wait_pid_terminate";   */

extern int __chldproc_platform_wait_pid_terminate (pid_t pid)
{
     int status;
     pid_t retval;

     /* We will intercept failures of waitpid to indicate that the pid */
     /* in question is already terminated */

     retval = waitpid ( pid, &status, 0 );

     if (retval != pid)
          return (-1);
     else
          return ((int)WEXITSTATUS(status));
}


/* procedure sigterm_kill (pid: in Process_ID) with       */
/*   Import        => True,                               */
/*   Convention    => C,                                  */
/*   External_Name => "__chldproc_platform_sigterm_kill"; */

extern void __chldproc_platform_sigterm_kill (pid_t pid)
{
     kill ( pid, SIGTERM );
}

/* procedure sigkill_kill (pid: in Process_ID) with       */
/*   Import        => True,                               */
/*   Convention    => C,                                  */
/*   External_Name => "__chldproc_platform_sigkill_kill"; */

extern void __chldproc_platform_sigkill_kill (pid_t pid)
{
     kill ( pid, SIGKILL );
}

/* function wait_read (fd: in Stream_Handle; wait: access timeval)  */
/*                    return int                                    */
/* with                                                             */
/*   Import        => True,                                         */
/*   Convention    => C,                                            */
/*   External_Name => "__chldproc_platform_wait_read";              */

extern int __chldproc_platform_wait_read (int fd, struct timeval * wait)
{
     fd_set set;
     
     FD_ZERO (&set);
     FD_SET  (fd, &set);
     return (select  (fd + 1, &set, NULL, NULL, wait));
}

/* function wait_write (fd: in Stream_Handle; wait: access timeval)  */
/*                     return int                                    */
/* with                                                              */
/*   Import        => True,                                          */
/*   Convention    => C,                                             */
/*   External_Name => "__chldproc_platform_wait_write";              */

extern int __chldproc_platform_wait_write (int fd, struct timeval * wait)
{
     fd_set set;
     
     FD_ZERO (&set);
     FD_SET  (fd, &set);
     return (select  (fd + 1, NULL, &set, NULL, wait));
}

