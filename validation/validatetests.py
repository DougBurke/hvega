#!/usr/bin/env python

"""

Run:

  ./validatetests.py schema

Aim:

Check all the Vega-Lite test files (*.vl) against the given schema.
The script must be run in the hvega/ directory (i.e. have the tests/
subdirectory in it).

This requires access to the Python jsonschema package [1]_.

Expected failures (i.e. "known" or "expected" differences)
are noted but not reported; only "new" failures are reported.

References:

  .. [1] https://github.com/Julian/jsonschema

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

# invalid labelOffset VL specification (string, not number) in VL4.4.0
# - should go away soon
labelOffsetFails = ['axis{}c.vl'.format(n) for n in range(1, 9)]

def process(schema):

    nfound = 0
    nfailed = 0
    nexpected = 0
    for root, dirs, infiles in os.walk('tests'):

        for infile in infiles:
            if not infile.endswith('.vl'):
                continue

            # The assumption here is that we can whitelist "acceptable"
            # failures by pointing out what element in the specification
            # was invalid. This is not 100% watertight but should be
            # okay for our needs.
            #
            allowed = None
            if (root == 'tests/specs/projection') and \
                (infile in projectionFails):
                allowed = deque(['projection', 'type'])
            elif (root == 'tests/specs/color') and \
                (infile in colorFails):
                allowed = deque(['encoding', 'color', 'scale', 'interpolate', 'type'])
            elif root == 'tests/specs/fillstroke':
                if infile in ['combined1.vl', 'fill1.vl']:
                    allowed = deque(['encoding', 'fill'])
                elif infile == 'stroke1.vl':
                    allowed = deque(['encoding', 'stroke'])

            elif root == 'tests/specs/axis':
                # Should be fixed in next release after 4.0.0
                if infile in labelOffsetFails:
                    allowed = deque(['config', 'axisQuantitative', 'labelOffset'])

            fullname = os.path.join(root, infile)
            js = read_json(fullname)
            nfound += 1

            try:
                validate(instance=js, schema=schema)
            except ValidationError as ve:
                nfailed += 1

                if ve.absolute_path == allowed:
                    nexpected += 1
                    continue

                msg = "{:<2d} {:40s} {}".format(nfailed, root, infile)
                sys.stderr.write(msg + "\n")

                msg = "    {}".format(ve.absolute_path)
                sys.stderr.write(msg + "\n")

                msg = "    {}".format(ve.validator)
                sys.stderr.write(msg + "\n")

                msg = "    {}".format(ve.validator_value)
                sys.stderr.write(msg + "\n")

                msg = "    {}".format(ve.message)
                sys.stderr.write(msg + "\n")

    if nfound == 0:
        sys.stderr.write("ERROR: No files found matching tests/[*]/*.vl\n")
        sys.exit(1)

    if nfailed == 0:
        print("All passed. That was unexpected\n")
        return

    nunexpected = nfailed - nexpected
    if nunexpected == 0:
        print("There were only expected 'failures' ({}).\n".format(nexpected))
        return

    sys.stderr.write("\n")
    sys.stderr.write("Of {} failures, {} were unexpected\n".format(nfailed,
                                                                   nunexpected))
    sys.exit(1)


if __name__ == "__main__":

    if len(sys.argv) != 2:
        sys.stderr.write("Usage: {} schema\n".format(sys.argv[0]))
        sys.exit(1)

    schema = read_json(sys.argv[1])
    process(schema)
