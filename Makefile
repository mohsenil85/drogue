all:
	buildapp \
		--manifest-file manifest.txt \
		--load-system drogue \
		--output out \
		--entry drogue:main\
		--compress-core
	
