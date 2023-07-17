#include "parser.h"

#include <errno.h>
#include <stdbool.h>
#include <stdio.h>

#include "../machine/utils.h"

#define LIST_BUF_SIZE 1024

bool is_separator(char c) {
  if (c == ' ' || c == '\n') return true;
  return false;
}

char* skip_separators(char* s) {
  while (1) {
    if (is_separator(*s)) {
      s++;
      continue;
    }

    break;
  }

  return s;
}

// comment
char* skip_comment(char* s) {
  if (*s != ';') return s;
  s++;

  while (1) {
    if (*s != '\n' && *s != '\0') {
      s++;
      continue;
    }

    break;
  }

  return s;
}

// number
bool is_numeric_char(char c) {
  if ('0' <= c && c <= '9') return true;
  if (c == '.') return true;
  return false;
}
char* parse_number(char* s, lisp_value_t** result) {
  char* next = NULL;
  errno = 0;
  double d = strtod(s, &next);
  if (errno != 0) {
    fprintf(stderr, "number: parse error\n");
    exit(1);
  }

  *result = make_number(d);

  return next;
}

// quote
char* parse_quote(char* s, lisp_value_t** result) {
  s++;

  lisp_value_t* content = NULL;
  s = parse_lisp_value(s, &content);
  if (content == NULL) {
    fprintf(stderr, "quote: no content\n");
    exit(1);
  }

  *result = cons(make_symbol("quote"), cons(content, make_null()));

  return s;
}

// list: ([separators][lisp value][separators][lisp value]...[separators])
char* parse_list(char* s, lisp_value_t** result) {
  // "([separators]" を消費
  s++;
  s = skip_separators(s);

  // "[lisp value][separators]..." をパース
  lisp_value_t* contents[LIST_BUF_SIZE] = {};
  int content_num = 0;
  while (1) {
    if (*s == ')') break;

    lisp_value_t* content = NULL;
    s = parse_lisp_value(s, &content);
    s = skip_separators(s);

    if (*s == '\0') {
      fprintf(stderr, "list: expected ')'\n");
      exit(1);
    }
    if (content != NULL) {
      contents[content_num] = content;
      content_num++;
    }
  }

  // ")" を消費
  s++;
  *result = list_from_values(contents);

  return s;
}

// symbol
bool is_symbol_char(char c) {
  if (is_separator(c)) return false;
  if (c == '(' || c == ')') return false;
  if (c == '.') return false;
  if (c == ';') return false;
  if (c == '\'') return false;
  return true;
}
char* parse_symbol(char* s, lisp_value_t** result) {
  char* next = s;
  while (1) {
    if (!is_symbol_char(*next)) break;
    next++;
  }

  *result = make_symbol_with_len(s, next - s);

  return next;
}

// main
char* parse_lisp_value(char* s, lisp_value_t** result) {
  s = skip_separators(s);

  if (*s == '\0') return s;

  // comment
  if (*s == ';') {
    return skip_comment(s);
  }

  // number
  if (is_numeric_char(*s)) {
    return parse_number(s, result);
  }

  // quote
  if (*s == '\'') {
    return parse_quote(s, result);
  }

  // list
  if (*s == '(') {
    return parse_list(s, result);
  }

  // symbol
  return parse_symbol(s, result);
}
