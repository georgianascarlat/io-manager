.PHONY: all clean

OBJECTS = simple-example

all: $(OBJECTS)

simple-example: loader.scm simple-example.scm io-manager.scm
  mzc --exe simple-example loader.scm

clean:
	$(RM) $(OBJECTS)
