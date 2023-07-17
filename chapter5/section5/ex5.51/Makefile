MAIN = main
TEST = test
SRCDIR = $(realpath .)
SRCS = $(filter-out $(TEST).c, $(filter-out $(MAIN).c, $(wildcard $(SRCDIR)/**/*.c)))
OBJS = $(SRCS:.c=.o)
HEADERS = $(SRCS:.c=.h)

CC := gcc
CFLAGS := -g -W -Wall -Wno-unused-parameter

.PHONY: default test clean
default: $(MAIN)

$(MAIN): $(MAIN).c $(OBJS)
	$(CC) $(CFLAGS) $@.c -o $@ $(OBJS)

$(TEST): $(TEST).c $(OBJS)
	$(CC) $(CFLAGS) $@.c -o $@ $(OBJS) -lcunit 
	./test

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf main test **/*.o 
