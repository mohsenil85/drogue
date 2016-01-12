all: manifest
	buildapp \
		--manifest-file manifest \
		--load-system swank \
		--load-system drogue \
		--output out \
		--entry drogue:main \
		--compress-core \

manifest:
	sbcl --eval '(ql:write-asdf-manifest-file "manifest" )' \
	--eval '(quit)'

clean:
	rm *.fasl out manifest
