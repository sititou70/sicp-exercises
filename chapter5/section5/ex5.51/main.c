#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "eceval/eval-apply.h"
#include "io/parser.h"
#include "io/printer.h"
#include "machine/primitives.h"

#define BUF_SIZE 1024

int main(int argc, char* argv[]) {
  if (argc >= 2 && strcmp(argv[1], "-h") == 0) {
    printf("usage:\n");
    printf("  repl: %s\n", argv[0]);
    printf("  execute file: %s [file path]\n", argv[0]);
    exit(0);
  }

  // read from file
  if (argc >= 2) {
    struct stat statBuf;
    stat(argv[1], &statBuf);

    FILE* file = fopen(argv[1], "r");

    char* input_buffer = malloc(sizeof(char) * (statBuf.st_size + 1));
    input_buffer[statBuf.st_size] = '\0';
    fread(input_buffer, sizeof(char), statBuf.st_size, file);

    lisp_value_t* parse_result = NULL;
    char* input = input_buffer;

    // racketの#langショートハンドを飛ばす
    while (*input != '\n') {
      input++;
    }
    input++;

    while (1) {
      parse_result = NULL;
      char* next = parse_lisp_value(input, &parse_result);
      if (next == input || *next == '\0') break;

      if (parse_result != NULL) eval(parse_result);

      input = next;
    }

    fclose(file);
    free(input_buffer);
    exit(0);
  }

  // repl
  char input_buffer[BUF_SIZE] = {};
  lisp_value_t* parse_result = NULL;
  while (1) {
    printf("> ");
    fflush(stdout);
    int scaned_num = scanf("%1023[^\n]%*c", input_buffer);

    if (scaned_num == 0) {
      scanf("%*c");
      continue;
    };

    if (scaned_num == EOF) break;

    parse_lisp_value(input_buffer, &parse_result);
    printf_lisp_value(eval(parse_result));
  }

  return 0;
}
