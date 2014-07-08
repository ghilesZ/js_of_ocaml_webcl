WITHSTR =str.cma -cclib -lstr
CC=ocamlfind ocamlc $(WITHSTR)
JSCC=js_of_ocaml
TARGET=vector_add_webcl
JSOPT=-linkpkg -package 'js_of_ocaml','js_of_ocaml.syntax' -syntax camlp4o

all: $(TARGET).js

$(TARGET).js: $(TARGET).byte
	$(JSCC) $<

$(TARGET).byte: webCL.cmo vector_add_webcl.ml
	$(CC) $(JSOPT) -o $@ $^

%.cmo : %.ml
	$(CC) $(JSOPT) -c $<

clean:
	rm -f *.cm[oi] *~ $(TARGET).js $(TARGET).byte
