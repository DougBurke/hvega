#!/usr/bin/env python

"""

Run:

  ./checkfile.py schema file

Aim:

Check the file against the JSON schema. This was written to
provide more-helpful error messages than I was getting from
jsonschema.

"""

from collections import deque
import json
import os
import sys

from jsonschema import validate
from jsonschema.exceptions import ValidationError


def read_json(infile):
    try:
        with open(infile, 'r') as fh:
            cts = fh.read()
    except IOError as ie:
        sys.stderr.write("ERROR: Unable to read from {}\n".format(infile))
        sys.stderr.write("       {}\n".format(ie))
        sys.exit(1)

    try:
        js = json.loads(cts)
    except json.JSONDecodeError as je:
        sys.stderr.write("ERROR: Not a JSON file {}\n".format(infile))
        sys.stderr.write("       {}\n".format(je))
        sys.exit(1)

    return js


# files that are known to fail due to unsupported projections
projectionFails = [
    'ConicEqualArea.vl',
    'airy.vl',
    'aitoff.vl',
    'armadillo.vl',
    'august.vl',
    'baker.vl',
    'berghaus.vl',
    'bertin1953.vl',
    'boggs.vl',
    'bonne.vl',
    'bottomley.vl',
    'collignon.vl',
    'craig.vl',
    'craster.vl',
    'cylindricalstereographic.vl',
    'cylindricalequalarea.vl',
    'eckert1.vl',
    'eckert2.vl',
    'eckert3.vl',
    'eckert4.vl',
    'eckert5.vl',
    'eckert6.vl',
    'eisenlohr.vl',
    'fahey.vl',
    'foucaut.vl',
    'gingery.vl',
    'winkel3.vl',
    ]

# color fails on interpolation type
colorFails = [
    'interp1.vl',
    'interp2.vl',
    'interp3.vl',
    'interp4.vl',
    'interp5.vl'
    ]

def compare(schema, instance):

    try:
        validate(instance=instance, schema=schema)
        return

    except ValidationError as ve:

        sys.stderr.write("Failed\n")

        msg = "    {}".format(ve.absolute_path)
        sys.stderr.write(msg + "\n")

        msg = "    {}".format(ve.validator)
        sys.stderr.write(msg + "\n")

        msg = "    {}".format(ve.validator_value)
        sys.stderr.write(msg + "\n")

        msg = "    {}".format(ve.message)
        sys.stderr.write(msg + "\n")

    sys.exit(1)


if __name__ == "__main__":

    if len(sys.argv) != 3:
        sys.stderr.write("Usage: {} schema file\n".format(sys.argv[0]))
        sys.exit(1)

    schema = read_json(sys.argv[1])
    instance = read_json(sys.argv[2])
    compare(schema, instance)
