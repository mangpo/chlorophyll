install:
	python configure.py
	raco exe src/chlorophyll.rkt

clean:
	rm -r examples/*/output-*


