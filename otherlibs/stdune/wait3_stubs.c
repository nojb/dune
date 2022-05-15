#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

#define TAG_WEXITED 0
#define TAG_WSIGNALED 1
#define TAG_WSTOPPED 2

#ifdef _WIN32

#include <Windows.h>

void dune_wait3(value flags) {
  caml_failwith("wait3: not supported on windows");
}

value dune_WaitForMultipleObjects(value v) {
  CAMLparam1(v);
  CAMLlocal2(vstatus, vres);
  HANDLE *lpHandles, hProcess;
  DWORD res, nCount, exitCode;
  value x;

  for (nCount = 0, x = v; Is_block(x); nCount ++, x = Field(x, 1))
    ;
  lpHandles = caml_stat_alloc(sizeof(HANDLE) * nCount);
  for (nCount = 0, x = v; Is_block(x); nCount ++, x = Field(x, 1)) {
    lpHandles[nCount] = (HANDLE)Long_val(Field(x, 0));
  }

  caml_enter_blocking_section();
  res = WaitForMultipleObjects(nCount, lpHandles, FALSE, INFINITE);
  caml_leave_blocking_section();

  if (res == WAIT_FAILED) {
    res = GetLastError();
    win32_maperr(res);
    caml_stat_free(lpHandles);
    uerror("WaitForMultipleObjects", Nothing);
  }

  hProcess = lpHandles[res - WAIT_OBJECT_0];
  caml_stat_free(lpHandles);

  if (GetExitCodeProcess(hProcess, &exitCode) == 0) {
    res = GetLastError();
    win32_maperr(res);
    CloseHandle(hProcess);
    uerror("GetExitCodeProcess", Nothing);
  }

  CloseHandle(hProcess);

  vstatus = caml_alloc(1, TAG_WEXITED);
  Store_field(vstatus, 0, Val_long(exitCode));

  vres = caml_alloc_tuple(2);
  Store_field(vres, 0, Val_long(hProcess));
  Store_field(vres, 1, vstatus);

  CAMLreturn(vres);
}

#else

#include <caml/signals.h>

#include <sys/resource.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>

CAMLextern int caml_convert_signal_number(int);
CAMLextern int caml_rev_convert_signal_number(int);

static value alloc_process_status(int status) {
  value st;

  if (WIFEXITED(status)) {
    st = caml_alloc_small(1, TAG_WEXITED);
    Field(st, 0) = Val_int(WEXITSTATUS(status));
  } else if (WIFSTOPPED(status)) {
    st = caml_alloc_small(1, TAG_WSTOPPED);
    Field(st, 0) = Val_int(caml_rev_convert_signal_number(WSTOPSIG(status)));
  } else {
    st = caml_alloc_small(1, TAG_WSIGNALED);
    Field(st, 0) = Val_int(caml_rev_convert_signal_number(WTERMSIG(status)));
  }
  return st;
}

static int wait_flag_table[] = {WNOHANG, WUNTRACED};

value dune_wait3(value flags) {
  CAMLparam1(flags);
  CAMLlocal2(times, res);

  int pid, status, cv_flags;
  struct timeval tp;
  cv_flags = caml_convert_flag_list(flags, wait_flag_table);

  struct rusage ru;

  caml_enter_blocking_section();
  pid = wait3(&status, cv_flags, &ru);
  gettimeofday(&tp, NULL);
  caml_leave_blocking_section();
  if (pid == -1)
    uerror("wait3", Nothing);

  times = caml_alloc_small(2 * Double_wosize, Double_array_tag);
  Store_double_field(times, 0, ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1e6);
  Store_double_field(times, 1, ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1e6);

  res = caml_alloc_tuple(4);
  Store_field(res, 0, Val_int(pid));
  Store_field(res, 1, alloc_process_status(status));
  Store_field(res, 2, caml_copy_double(((double) tp.tv_sec + (double) tp.tv_usec / 1e6)));
  Store_field(res, 3, times);
  CAMLreturn(res);
}

value dune_WaitForMultipleObjects(value v) {
  caml_failwith("WaitForMultipleObjects: not supported on unix");
}

#endif
