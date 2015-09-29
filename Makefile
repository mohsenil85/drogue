all: manifest
	buildapp \
		--manifest-file manifest \
		--load-system drogue \
		--output out \
		--entry drogue:main \

manifest:
	sbcl --eval '(ql:write-asdf-manifest-file "manifest" )' \
	--eval '(quit)'

clean:
	rm *.fasl out manifest
