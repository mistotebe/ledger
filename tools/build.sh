#!/bin/sh

time (                                                          \
    cd ~/src/ledger ;                                           \
    PATH=/usr/local/bin:/opt/local/bin:$PATH                    \
      nice -n 20                                                \
        ./acprep --debug --python --http --doxygen "$@"         \
)