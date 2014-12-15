install:
	python configure.py
	raco make src/compiler.rkt
	raco exe src/chlorophyll.rkt
	raco exe src/chlorophyll-test.rkt

clean:
	rm src/chlorophyll src/chlorophyll-test src/output*.tmp
	rm -r examples/*/output-* src/compiled


