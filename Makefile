# Kostra grafu
# Funkcionalni a logicke programovani
# Nikola Valesova, xvales02

all:
	swipl -q -g start -o flp18-log -c xvales02.pl

test: all
	python SpanningTreeTest.py

time: all
	python SpanningTreeTest.py --times

clean:
	rm flp18-log
