# Define the target executable and intermediate file
TARGET = node

# Define source file patterns
HS_SOURCES = $(wildcard *.hs)
C_SOURCES = $(wildcard *.c)
H_SOURCES = $(wildcard *.h)

node: LUS_step.c $(C_SOURCES) $(H_SOURCES)
	@echo "Compiling C code..."
	gcc -o2 main.c -o node

Frustre: $(HS_SOURCES)
	@echo "Compiling Haskell..."
	ghc -O2 --make Frustre

LUS_step.c: ./Frustre
	@echo "Running Frustre to generate LUS_step.c..."

# Clean rule to remove generated files
.PHONY: clean
clean:
	rm -f LUS_step.c $(TARGET)
