swarm - Fast acceptor pool for Erlang

swarm is a fast and simple TCP and SSL connection acceptor pool library.

* Overview

swarm is based loosely on ranch and much credit is due to Loïc Hoguin for
basic interface design. The swarm interface resembles ranch's, but under
the hood things are a bit different.

In particular, swarm passes transport options straight through, so all
options are available, and it provides a more complete transport interface
on top of gen_tcp and ssl (e.g. DN parsing for SSL client certs). 

The swarm acceptor design is simple, but "backwards" from other designs,
in that the accepting process becomes the working process, and the listener
spawns another acceptor (based on a replacement strategy) to replace it.
This design can be tuned to optimize for low-latency burst handling or
for one-for-one acceptor replacement. It also removes the need to
synchronize over the passing of socket control to a child process.

* Motivation

swarm is a key ingredient in middleman, a scalable and modular TCP, SSL,
and HTTP reverse proxy. Middleman requires a number of features not
provided by ranch, and the simplest path was to build a new acceptor 
library.

swarm is intended to be fast and simple and to get out of the way, so it
is unlikely to receive significant new features.

* Using swarm

** Add swarm as a rebar dependency:

#+begin_src erlang
  {swarm, ".*", {git, "file:///Users/jbarrett/dev/swarm", {branch, "master"}}}
#+end_src

** Start the application

#+begin_src erlang
  swarm:start()
#+end_src

** Start a listener

#+begin_src erlang
  {ok, Pid} = swarm:start_listener(my_app, 100, swarm_tcp, [{port, 8080}], {my_app_handler, handle, []})
#+end_src

