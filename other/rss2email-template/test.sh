#!/bin/bash

_dir="$(pwd)"
cd test-toolkit
PYTHONPATH="$_dir":. ./test.py ./bbc-chinese/1.config

