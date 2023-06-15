#!/bin/bash
zig build test;
zig build;
export PATH=$PATH:`pwd`/zig-out/bin;
