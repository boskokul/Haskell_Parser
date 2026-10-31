TARGET=compiler
FILES=Main.hs HsParser.hs AstTypes.hs TypeInfer.hs Desugar.hs

all: $(TARGET)

$(TARGET): $(FILES)
	ghc -O0 -o $(TARGET) $(FILES)

verify: $(TARGET)
	./verify.sh

clean:
	rm -f $(TARGET) *.json
	rm -rf examples/*.json

.PHONY: all verify clean
