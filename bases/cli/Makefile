run:
	clojure -M -m fset.cli.core scheduler.mch

jar:
	clojure -X:uberjar

image: jar
	native-image -jar fset.jar -H:Name=fset --no-fallback

clean:
	rm -f fset.jar fset
