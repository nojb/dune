#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>

#ifdef _WIN32

#include <Windows.h>

typedef struct dune_rdcw_t {
  HANDLE hDirectory;
  value v_callback;
} dune_rdcw_t;

value dune_rdcw_create(value v_path, value v_callback)
{
  CAMLparam2(v_path, v_callback);
  HANDLE hDirectory;
  dune_rdcw_t *t;
  int res;
  WCHAR *path;

  res = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, String_val(v_path), -1, NULL, 0);

  if (res == 0) {
    caml_sys_error("MultiByteToWideChar");
  }

  path = caml_stat_alloc(res * sizeof(WCHAR));

  res = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, String_val(v_path), -1, path, res);

  if (res == 0) {
    caml_stat_free(path);
    caml_sys_error("MultiByteToWideChar");
  }

  hDirectory = CreateFileW(path,
                           FILE_LIST_DIRECTORY,
                           FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                           NULL,
                           OPEN_EXISTING,
                           FILE_FLAG_BACKUP_SEMANTICS,
                           NULL);

  caml_stat_free(path);

  if (hDirectory == INVALID_HANDLE_VALUE) {
    caml_sys_error("CreateFile");
  }

  t = caml_stat_alloc(sizeof(dune_rdcw_t));

  t->hDirectory = hDirectory;
  t->v_callback = v_callback;

  caml_register_generational_global_root(&t->v_callback);

  CAMLreturn(caml_copy_nativeint((intnat)t));
}

#define BUFFER_SIZE 16384

value dune_rdcw_loop(value v_t)
{
  CAMLparam1(v_t);
  CAMLlocal3(v_res, v_path, v_act);
  dune_rdcw_t *t;
  char fileInfoBuffer[BUFFER_SIZE];
  BOOL res;
  DWORD dwBytesReturned, dwPathLen;
  FILE_NOTIFY_INFORMATION *event;
  int tag;

  t = (dune_rdcw_t *)Nativeint_val(v_t);

  while (1) {
    caml_release_runtime_system();
    res = ReadDirectoryChangesW(t->hDirectory,
                                (FILE_NOTIFY_INFORMATION *)fileInfoBuffer,
                                sizeof(fileInfoBuffer),
                                TRUE,
                                FILE_NOTIFY_CHANGE_LAST_WRITE |
                                FILE_NOTIFY_CHANGE_CREATION |
                                FILE_NOTIFY_CHANGE_FILE_NAME,
                                &dwBytesReturned,
                                NULL,
                                NULL);
    caml_acquire_runtime_system();

    if (res == FALSE) {
      caml_sys_error("ReadDirectoryChangesW");
    }

    if (dwBytesReturned == 0) {
      caml_callback2(t->v_callback, v_t, Val_int(0));
      continue;
    }

    event = (FILE_NOTIFY_INFORMATION *)fileInfoBuffer;

    while (1) {
      dwPathLen = WideCharToMultiByte(CP_UTF8,
                                      WC_ERR_INVALID_CHARS,
                                      event->FileName,
                                      event->FileNameLength / sizeof(WCHAR),
                                      NULL,
                                      0,
                                      NULL,
                                      NULL);

      if (dwPathLen == 0) {
        caml_sys_error("WideCharToMultiByte");
      }

      v_path = caml_alloc_string(dwPathLen);

      dwPathLen = WideCharToMultiByte(CP_UTF8,
                                      WC_ERR_INVALID_CHARS,
                                      event->FileName,
                                      event->FileNameLength / sizeof(WCHAR),
                                      Bytes_val(v_path),
                                      dwPathLen,
                                      NULL,
                                      NULL);

      if (dwPathLen == 0) {
        caml_sys_error("WideCharToMultiByte");
      }

      switch(event->Action) {
        case FILE_ACTION_ADDED:
          tag = 0;
          break;
        case FILE_ACTION_REMOVED:
          tag = 1;
          break;
        case FILE_ACTION_MODIFIED:
          tag = 2;
          break;
        case FILE_ACTION_RENAMED_OLD_NAME:
          tag = 3;
          break;
        case FILE_ACTION_RENAMED_NEW_NAME:
          tag = 4;
          break;
        default:
          CAMLassert(0);
          break;
      }

      v_act = caml_alloc(1, tag);
      Store_field(v_act, 0, v_path);
      v_res = caml_callback2(t->v_callback, v_t, v_act);

      if (event->NextEntryOffset == 0)
        break;

      *(char **)&fileInfoBuffer += event->NextEntryOffset;
    }
  }

  CAMLreturn(Val_unit);
}

value dune_rdcw_destroy(value v_t)
{
  CAMLparam1(v_t);
  dune_rdcw_t *t;

  t = (dune_rdcw_t *)Nativeint_val(v_t);

  CloseHandle(t->hDirectory);
  caml_remove_generational_global_root(&t->v_callback);
  caml_stat_free(t);

  CAMLreturn(Val_unit);
}

#else

#endif
