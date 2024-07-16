clj -X:build:prod uberjar :build/jar-name "target/app.jar"
java -cp target/app.jar clojure.main -m prod
