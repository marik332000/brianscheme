/* pool.c - Allocation Pool Library
 * Copyright (C) 2007 Christopher Wellons <mosquitopsu@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/mman.h>
#include <bits/mman.h>
#include "pool.h"
#include "gc.h"

static const size_t hdr = sizeof(void *) + sizeof(size_t);

size_t default_pool_size = 1048576;
int miss_limit = 8;
int pool_scale = 2;
size_t init_freed_stack = 256;

void* new_mmap(size_t size) {
  void *p = mmap(NULL, size, PROT_READ | PROT_WRITE,
		 MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  fflush(stdout);
  return p;
}

/* Used internally to allocate more pool space. */
static subpool_t *create_subpool_node (size_t size);

pool_t *create_pool (size_t init_size, size_t init_alloc, void **init)
{
  /* if init_size == 0, user didn't want to choose one */
  if (init_size == 0)
    {
      init_size = default_pool_size;
    }
  /* Make sure it aligns as a page. */
  size_t ps = sysconf(_SC_PAGE_SIZE);
  init_size = (init_size / ps) * ps;

  /* allocate first subpool and use it for the pool */
  subpool_t *first = create_subpool_node (init_size);
  pool_t *new_pool = first->free_start;
  first->free_start += sizeof (pool_t);
  new_pool->pools = first;
  new_pool->first = new_pool->pools;

  if (init_alloc > 0 && init != NULL)
    *init = pool_alloc(new_pool, init_alloc);

  return new_pool;
}

/* Returns a pointer to the allocated size bytes from the given
 * pool. */
void *pool_alloc (pool_t * source_pool, size_t size)
{
  subpool_t *cur, *last;
  void *chunk = NULL;
  size_t s = sizeof(size_t);

  cur = source_pool->first;
  if (cur->misses > miss_limit)
    {
      /* this pool doesn't seem to be any good anymore */
      source_pool->first = source_pool->first->next;
    }

  do
    {
      /* Check this pool's free list. */
      if (cur->freedb != NULL) {
	freed_t *p = cur->freedp - 1;
	while (p >= cur->freedb)
	  {
	    if (p->size >= size)
	      {
		cur->misses = 0;
		void *chunk = p->p;
		memmove(p, p + 1, (cur->freedp - p - 1) * sizeof(freed_t));
		cur->freedp--;
		return chunk;
	      }
	    p--;
	  }
      }

      if ((size + s) <= (size_t) (cur->free_end - cur->free_start))
	{
	  /* cut off a chunk and return it */
	  chunk = cur->free_start;
	  cur->free_start += size + s;
	  cur->misses = 0;
	}
      else
	{
	  /* current pool is too small */
	  cur->misses++;
	}

      last = cur;
      cur = cur->next;
    }
  while (cur != NULL && chunk == NULL);

  /* No existing pools had enough room. Make a new one. */
  if (chunk == NULL)
    {
      /* double the size of the last one */
      size_t new_size = last->size * pool_scale;
      if (new_size <= (size + s))
	{
	  /* square requested size if its much bigger */
	  new_size = (size + s) * pool_scale * pool_scale;
	}

      /* create new subpool */
      last->next = create_subpool_node (new_size);
      cur = last->next;

      if (cur == NULL)		/* failed to allocate subpool */
	return NULL;

      /* chop off requested amount */
      chunk = cur->free_start;
      cur->free_start += size + s;
    }
  ((size_t *) chunk)[0] = size;
  return chunk + s;
}

/* Reallocate pool memory at location. */
void *pool_realloc (pool_t * source_pool, void * p, size_t new)
{
  size_t old = *(((size_t *) p) - 1);
  if (old >= new)
    return p;
  void *np = pool_alloc (source_pool, new);
  pool_free (source_pool, p);
  memcpy(np, p, old);
  return np;
}

/* Return memory to the pool. */
void pool_free (pool_t * source_pool, void *p)
{
  subpool_t *cur, *next;

  /* Locate the right pool. */
  cur = source_pool->first;
  next = cur->next;
  while (next != NULL)
    {
      if (p >= ((void *) cur) && p < ((void *) next))
	break;
      cur = next;
      next = cur->next;
    }

  /* Add to the free list. */
  if (cur->freedb == NULL) {
    cur->freedb = pool_alloc(source_pool, init_freed_stack * sizeof(freed_t));
    cur->freedp = cur->freedb;
    cur->freed_size = init_freed_stack;
  }
  if (((size_t) (cur->freedp - cur->freedb))
      > ((size_t) (cur->freed_size - 3))) {
    cur->freed_size *= pool_scale;
    fflush(stdout);
    freed_t *stack = pool_realloc(source_pool, cur->freedb,
				  cur->freed_size * sizeof(freed_t));
    /* Old stack may end up on itself here. */
    fflush(stdout);
    size_t diff = cur->freedp - cur->freedb;
    cur->freedb = stack;
    cur->freedp = stack + diff;
  }
  size_t size = *(((size_t *) p) - 1);
  cur->freedp->size = size;
  cur->freedp->p = p;
  cur->freedp++;
}

subpool_t *create_subpool_node (size_t size)
{
  /* allocate subpool memory */
  void *block = new_mmap (size);
  subpool_t *new_subpool = block + hdr;
  new_subpool->mem_block = block;

  /* initialize data */
  ((size_t *) new_subpool->mem_block)[0] = size;
  ((void **) (sizeof(size_t *) + new_subpool->mem_block))[0]
    = new_subpool->mem_block;
  new_subpool->free_start = new_subpool->mem_block + hdr + sizeof(subpool_t);
  new_subpool->free_end = new_subpool->mem_block + size;
  new_subpool->size = size;
  new_subpool->misses = 0;
  new_subpool->next = NULL;

  /* freed stack (create later) */
  new_subpool->freedb = NULL;
  new_subpool->freedp = NULL;
  new_subpool->freed_size = 0;

  return new_subpool;
}

/* Dump entire pool to file that can be read back in later to the same
 * place in memory. */
int pool_dump (pool_t * pool, char *file)
{
  FILE *f = fopen(file, "w");
  if (f == NULL) return -1;
  subpool_t *cur = pool->pools;
  while (cur != NULL)
    {
      fwrite(cur->mem_block, cur->size, 1, f);
      cur = cur->next;
    }
  fclose(f);
  return 0;
}

/* Read the pool from the given file into memory. */
void* pool_load (char * file)
{
  int fd = open(file, O_RDONLY);
  if (fd < 0) {
    fprintf(stderr, "error: failed to open %s: %s\n", file, strerror(errno));
    return NULL;
  }
  off_t loc = 0;
  void *first = NULL;
  while (1) {
    void *address;
    size_t size, r;
    r = read(fd, &size, sizeof(size_t));
    if (r == 0) break;
    r = read(fd, &address, sizeof(void *));
    if (r == 0) break;
    if (first == NULL)
      first = address;
    void *p = mmap(address, size, PROT_READ | PROT_WRITE,
		   MAP_PRIVATE | MAP_FIXED, fd, loc);
    if (p == MAP_FAILED) {
      fprintf(stderr, "error: failed to mmap() %s: %s\n",
	      file, strerror(errno));
      return NULL;
    }
    loc += size;
    lseek(fd, size - hdr, SEEK_CUR);
  }
  if (first == NULL) {
    fprintf(stderr, "error: empty image file: %s\n", file);
    return NULL;
  }
  return first + hdr + sizeof(subpool_t) + sizeof(pool_t) + sizeof(size_t);
}