exec erl -pa ebin deps/*/ebin -config nasoc.config \
    -sname nasoc_dev +K true \
    -s nasoc_app \
    -setcookie blah 