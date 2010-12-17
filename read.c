#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include <editline.h>

#include "types.h"
#include "symbols.h"
#include "read.h"
#include "gc.h"
/*temp*/
#include "interp.h"

char *prompt = "> ";

typedef struct {
  FILE *stream;
  char *buffer, *p;
  int ungetc;
  char *history;
} read_buffer;

object *throw_read(char * msg, ...) {
  va_list args;
  va_start(args, msg);
  vfprintf(stderr, msg, args);
  va_end(args);
  exit(1);
  return NULL;
}

char *xstrdup(char *str) {
  char *newstr = MALLOC (strlen (str) + 1);
  strncpy (newstr, str, strlen(str) + 1);
  return newstr;
}

/* Append the current buffer to the history. */
void grow_history(read_buffer *in) {
  if (in->buffer == NULL || (strlen(in->buffer) == 0))
    return;
  if (in->history == NULL) {
    in->history = xstrdup(in->buffer);
    return;
  }
  size_t histlen = strlen(in->history);
  size_t bufflen = strlen(in->buffer);
  in->history = realloc(in->history, histlen + bufflen + 2);
  in->history[histlen] = '\n';
  strncpy(in->history + histlen + 1, in->buffer, bufflen + 1);
}

/* Wraps calls to getc() to work on our buffer. */
int read_getc(read_buffer *in) {
  if (in->stream != stdin) {
    return getc(in->stream);
  }
  if (in->ungetc != -1) {
    char c = in->ungetc;
    in->ungetc = -1;
    return c;
  }
  if (in->p != NULL && *in->p == '\0') {
    in->p = NULL;
    return '\n';
  }
  if (in->p == NULL) {
    free(in->buffer);
    in->buffer = in->p = readline(in->p == NULL ? prompt : "");
    grow_history(in);
    if (in->buffer == NULL)
      return EOF;
  }
  return *in->p++;
}

/* Wraps calls to ungetc() to work on our buffer. */
void read_ungetc(char c, read_buffer *in) {
  if (in->stream != stdin) {
    ungetc(c, in->stream);
  } else {
    in->ungetc = c;
  }
}

char is_delimiter(int c) {
  return isspace(c) || c == EOF ||
    c == '(' || c == ')' ||
    c == '"' || c ==';';
}

char is_initial(char c) {
  return isalpha((int)c) || c == '*' || c == '/' ||
    c == '>' || c == '<' || c == '=' || c == '?' ||
    c == '?' || c == '!' || c == '&' || c == '.';
}

int peek(read_buffer *in) {
  int c;
  c = read_getc(in);
  read_ungetc(c, in);
  return c;
}

int eat_whitespace(read_buffer *in) {
  int c;

  while ((c = read_getc(in)) != EOF) {
    if(isspace(c)) {
      continue;
    }
    else if(c == ';') {
      /* eat until eol */
      while (((c = read_getc(in)) != EOF) && (c != '\n')) {
	continue;
      }
      if (c == EOF)
	return c;
      return eat_whitespace(in);
    }
    read_ungetc(c, in);
    break;
  }
  return c;
}

void eat_expected_string(read_buffer *in, char *str) {
  int c;
  while(*str != '\0') {
    c = read_getc(in);
    if(c != *str) {
      throw_read("unexpected character '%c'\n", c);
    }
    str++;
  }
}

void peek_expected_delimiter(read_buffer *in) {
  if(!is_delimiter(peek(in))) {
    throw_read("character not followed by delimiter\n");
  }
}

object *lisp_read(read_buffer *in);

object *read_character(read_buffer *in) {
  int c;

  c = read_getc(in);
  switch(c) {
  case EOF:
    return throw_read("incomplete character literal\n");
  case 's':
    if(peek(in) == 'p') {
      eat_expected_string(in, "pace");
      peek_expected_delimiter(in);
      return make_character(' ');
    }
    break;
  case 'n':
    if(peek(in) == 'e') {
      eat_expected_string(in, "ewline");
      peek_expected_delimiter(in);
      return make_character('\n');
    }
    break;
  }
  peek_expected_delimiter(in);
  return make_character(c);
}

object *read_pair(read_buffer *in) {
  int c;
  object *car_obj;
  object *cdr_obj;

  if (eat_whitespace(in) == EOF)
    return throw_read("unexpected EOF\n");

  c = read_getc(in);
  if(c == ')') {
    return the_empty_list;
  }
  read_ungetc(c, in);

  car_obj = lisp_read(in);
  push_root(&car_obj);

  if (eat_whitespace(in) == EOF)
    return throw_read("unexpected EOF\n");

  c = read_getc(in);
  if(c == '.') {
    peek_expected_delimiter(in);

    cdr_obj = lisp_read(in);
    push_root(&cdr_obj);

    if (eat_whitespace(in) == EOF)
      return throw_read("unexpected EOF\n");
    c = read_getc(in);
    if(c != ')') {
      return throw_read("improper list missing trailing paren\n");
    }

    object *result = cons(car_obj, cdr_obj);
    pop_root(&cdr_obj);
    pop_root(&car_obj);

    return result;
  } else {
    read_ungetc(c, in);

    cdr_obj = read_pair(in);
    push_root(&cdr_obj);

    object *result = cons(car_obj, cdr_obj);
    pop_root(&cdr_obj);
    pop_root(&car_obj);
    return result;
  }
}

object *read_vector(read_buffer *in) {
  int c;
  object *list_head;
  object *list_tail;
  object *current;

  if (eat_whitespace(in) == EOF)
    return throw_read("unexpected EOF\n");

  c = read_getc(in);
  if(c == ')') {
    return the_empty_vector;
  }
  read_ungetc(c, in);

  current = cons(lisp_read(in), the_empty_list);
  push_root(&current);
  list_head = current;
  push_root(&list_head);

  /* this is protected by the head */
  list_tail = list_head;

  if (eat_whitespace(in) == EOF)
    return throw_read("unexpected EOF\n");
  c = read_getc(in);
  while(c != ')') {
    read_ungetc(c, in);

    current = lisp_read(in);
    set_cdr(list_tail, cons(current, the_empty_list));
    list_tail = cdr(list_tail);

    if (eat_whitespace(in) == EOF)
      return throw_read("unexpected EOF\n");
    c = read_getc(in);
  }

  object *vector = list_to_vector(list_head);
  pop_root(&list_head);
  pop_root(&current);

  return vector;
}

object *obj_read(FILE *in) {
  read_buffer r;
  r.stream = in;
  r.buffer = r.p = r.history = NULL;
  r.ungetc = -1;
  object *obj = lisp_read(&r);
  free(r.buffer);
  add_history(r.history);
  return obj;
}

object *lisp_read(read_buffer *in) {
  int c;
  short sign = 1;
  int i;
  long num = 0;
#define BUFFER_MAX 1000
  char buffer[BUFFER_MAX];

  if (eat_whitespace(in) == EOF)
    return NULL;
  c = read_getc(in);
  if(c == '#') {
    c = read_getc(in);
    switch(c) {
    case 't':
      return true;
    case 'f':
      return false;
    case '\\':
      return read_character(in);
    case '(':
      return read_vector(in);
    default:
      return throw_read("unknown boolean or character literal\n");
    }
  }
  else if(isdigit(c) || (c == '-' && (isdigit(peek(in))))) {
    if(c == '-') {
      sign = -1;
    } else {
      sign = 1;
      read_ungetc(c, in);
    }

    while(isdigit(c = read_getc(in))) {
      num = (num * 10) + (c - '0');
    }
    num *= sign;
    if(is_delimiter(c)) {
      read_ungetc(c, in);
      return make_fixnum(num);
    } else {
      return throw_read("number was not followed by delimiter");
    }
  }
  else if(is_initial(c) ||
	  ((c == '+' || c == '-') &&
	   is_delimiter(peek(in)))) {
    i = 0;
    while(is_initial(c) || isdigit(c) ||
	  c == '+' || c == '-') {
      if(i < BUFFER_MAX - 1) {
	buffer[i++] = c;
      } else {
	return throw_read("symbol exceeded %d chars", BUFFER_MAX);
      }
      c = read_getc(in);
      if (c == EOF) {
	return throw_read("unexpected EOF\n");
      }
    }
    if(is_delimiter(c)) {
      buffer[i] = '\0';
      read_ungetc(c, in);
      return make_symbol(buffer);
    } else {
      return throw_read("symbol not followed by delimiter. found %c\n",
			c);
    }
  }
  else if(c == '"') {
    i = 0;
    while((c = read_getc(in)) != '"') {
      if(c == '\\') {
	c = read_getc(in);
	if(c == 'n') {
	  c = '\n';
	}
	else if(c == '"') {
	  c = '"';
	}
      }
      if(c == EOF) {
	return throw_read("string literal not terminated\n");
      }
      if(i < BUFFER_MAX - 1) {
	buffer[i++] = c;
      }
      else {
	return throw_read("string exceeded buffer length %d", BUFFER_MAX);
      }
    }
    buffer[i] = '\0';
    return make_string(buffer);
  }
  else if(c == '(') {
    return read_pair(in);
  }
  else if(c == '\'') {
    object *quoted = lisp_read(in);
    push_root(&quoted);
    quoted = cons(quoted, the_empty_list);
    quoted = cons(quote_symbol, quoted);
    pop_root(&quoted);
    return quoted;
  }
  else if(c == ',') {
    object *unquoted = lisp_read(in);
    push_root(&unquoted);
    unquoted = cons(unquoted, the_empty_list);
    unquoted = cons(unquote_symbol, unquoted);
    pop_root(&unquoted);
    return unquoted;
  }
  else if(c == '`') {
    object *qquoted = lisp_read(in);
    push_root(&qquoted);
    qquoted = cons(qquoted, the_empty_list);
    qquoted = cons(quasiquote_symbol, qquoted);
    pop_root(&qquoted);
    return qquoted;
  }
  else if(c == EOF) {
    return NULL;
  }
  else {
    return throw_read("bad input. Unexpected '%c'\n", c);
  }
}
