Simple HTTP Web Server used when testing other things I'm working on.

HTTP Endpoints

/increase_sleep
Every time it is hit the process serving the request will sleep and increment a sleep value thus increasing response times

/node
Returns the name of the Erlang node

/echo?value=${value}
Returns a value specified in the value query parameter as text/plain
