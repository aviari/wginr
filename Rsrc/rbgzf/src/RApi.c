//
// R API
//
#include <R.h>
#include "RMemory.h"
#include "bgzf.h"

void bgzf_index_destroy(BGZF *fp);

//
// Api
//

// check BGZF format

void C_bgzf_is_bgzf(char **path, int *res) {
  *res = bgzf_is_bgzf(*path);
}


// open bgzf file

void C_bgzf_open(char **path, char **mode, char**res) {
  BGZF *bgzf = bgzf_open(*path, *mode);
  r_ptr2str(bgzf, *res);
}

// close bgzf file

void C_bgzf_close(char **bgzf, int *res) {
  Ptr p = r_str2ptr(*bgzf);
  int status = 1;
  if (r_check(p)) {
    status = bgzf_close((BGZF*) p);
  } else {
    warning("invalid pointer : 0x%lx", (unsigned long) p);
  }
  *res = status;
}

// read bytes from file

void C_bgzf_read(char **bgzf, double *nbytes, unsigned char *buffer, double *nread) {
  Ptr p = r_str2ptr(*bgzf);
  if (r_check(p)) {
    ssize_t sread = bgzf_read((BGZF*) p, buffer, (size_t) *nbytes);
    *nread = (double) sread;
  } else {
    warning("invalid pointer : 0x%lx", (unsigned long) p);
    *nread = 0;
  }
}

// write bytes to file

void C_bgzf_write(char **bgzf, unsigned char *buffer, double *nbytes, double *nwrite) {
  Ptr p = r_str2ptr(*bgzf);
  if (r_check(p)) {
    ssize_t swrite = bgzf_write((BGZF*) p, buffer, (size_t) *nbytes);
    *nwrite = (double) swrite;
  } else {
    warning("invalid pointer : 0x%lx", (unsigned long) p);
    *nwrite = 0;
  }
}

// flush write

void C_bgzf_flush(char **bgzf, int *res) {
  Ptr p = r_str2ptr(*bgzf);
  int status = 1;
  if (r_check(p)) {
    status = bgzf_flush((BGZF*) p);
  } else {
    warning("invalid pointer : 0x%lx", (unsigned long) p);
  }
  *res = status;
}

// get position in uncompressed file

void C_bgzf_utell(char **bgzf, double *res) {
  Ptr p = r_str2ptr(*bgzf);
  long offset = -1;
  if (r_check(p)) {
    offset = bgzf_utell((BGZF*) p);
  } else {
    warning("invalid pointer : 0x%lx", (unsigned long) p);
  }
  *res = (double) offset;
}

// set position in uncompressed file

void C_bgzf_useek(char **bgzf, double *offset, int *res) {
  Ptr p = r_str2ptr(*bgzf);
  int status = -1;
  if (r_check(p)) {
    status = bgzf_useek((BGZF*) p, (long) *offset, SEEK_SET);
  } else {
    warning("invalid pointer : 0x%lx", (unsigned long) p);
  }
  *res = status;
}

// init index

void C_bgzf_index_build_init(char **bgzf, int *res) {
  Ptr p = r_str2ptr(*bgzf);
  int status = -1;
  if (r_check(p)) {
    status = bgzf_index_build_init((BGZF*) p);
  } else {
    warning("invalid pointer : 0x%lx", (unsigned long) p);
  }
  *res = status;
}

// check index

void C_bgzf_has_index(char **bgzf, int *res) {
  Ptr p = r_str2ptr(*bgzf);
  int status = 0;
  if (r_check(p)) {
    status = ((BGZF*) p)->idx != NULL;
  } else {
    warning("invalid pointer : 0x%lx", (unsigned long) p);
  }
  *res = status;
}

// remove index

void C_bgzf_index_remove(char **bgzf, int *res) {
  Ptr p = r_str2ptr(*bgzf);
  int status = -1;
  if (r_check(p)) {
    bgzf_index_destroy((BGZF*) p);
    status = 0;
  } else {
    warning("invalid pointer : 0x%lx", (unsigned long) p);
  }
  *res = status;
}

// reindex all file

void C_bgzf_reindex(char **bgzf, int *res) {
  Ptr p = r_str2ptr(*bgzf);
  int status = 1;
  if (r_check(p)) {
    BGZF *fp = (BGZF*) p;
    int64_t pos = bgzf_tell(fp), prev = -1L;
    // clear previous index
    if (fp->idx) bgzf_index_destroy(fp);
    // init new index
    if (bgzf_index_build_init(fp) != 0)
      error("cannot init index");
    // rewind file
    if (bgzf_seek(fp, 0L, SEEK_SET) != 0)
      error("cannot rewind file");
    // read by blocks - this will force indexing on the fly
    if (bgzf_read_block(fp) != 0)
      warning("cannot read block");
    while (bgzf_read_block(fp) == 0) {
      if (fp->block_address == prev) break;
      prev = fp->block_address;
    }
    // reposition file
    if (bgzf_seek(fp, pos, SEEK_SET) != 0)
      error("cannot reposition file");
    // success
    status = 0;
  } else {
    warning("invalid pointer : 0x%lx", (unsigned long) p);
  }
  *res = status;
}

// load index

void C_bgzf_index_load(char **bgzf, char **path, int *res) {
  Ptr p = r_str2ptr(*bgzf);
  int status = 1;
  if (r_check(p)) {
    status = bgzf_index_load((BGZF*) p, *path, NULL);
  } else {
    warning("invalid pointer : 0x%lx", (unsigned long) p);
  }
  *res = status;
}

// dump index

void C_bgzf_index_dump(char **bgzf, char **path, int *res) {
  Ptr p = r_str2ptr(*bgzf);
  int status = 1;
  if (r_check(p)) {
    status = bgzf_index_dump((BGZF*) p, *path, NULL);
  } else {
    warning("invalid pointer : 0x%lx", (unsigned long) p);
  }
  *res = status;
}
