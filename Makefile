all:
	buildapp \
		--manifest-file manifest.txt \
		--load-system drogue \
		--output output \
		--entry drogue:main\
		--compress-core
	
