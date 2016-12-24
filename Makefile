install:
	python configure.py
	raco make src/compiler.rkt

clean:
	rm src/chlorophyll src/chlorophyll-test src/output*.tmp
	rm -r examples/*/output-* src/compiled


