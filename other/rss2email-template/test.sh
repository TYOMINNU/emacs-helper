#!/bin/bash

_dir="$(pwd)"
cd test-toolkit
PYTHONPATH="$_dir":. ./test.py ./bbc-chinese/add_pinyin.config

