exec erl -pa ebin deps/*/ebin -boot start_sasl \
    -sname nasoc_dev +K true \
    -s nasoc_app \
    -setcookie blah 