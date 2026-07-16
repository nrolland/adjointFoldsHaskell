#!/usr/bin/env bash
set -euo pipefail

ghci -ignore-dot-ghci -v0 < tests/Regression.ghci
