#!/bin/bash
docker run --network draw-server_default -it --rm cassandra:4.1.4 cqlsh cassandra
