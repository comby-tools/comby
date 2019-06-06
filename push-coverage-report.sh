#!/bin/bash

bisect-ppx-report \
    -I _build/default/ \
    -coveralls coverage.json \
    -service-name travis-ci \
    -service-job-id $TRAVIS_JOB_ID \
    `find . -name 'bisect*.out'`
curl -L -F json_file=@./coverage.json https://coveralls.io/api/v1/jobs
