# Validate the test output

The `validate.py` script tests the hvega test output - that is the
files in `hvega/tests/` - against a JSON schema and reports if
any fail. The script must be run in the `hvega/` directory, and
be given the path to the JSON schema file to use.

At the time of writing, the tests are known to include some that do
not fully conform to versin 3.4.0 of the Vega-Lite schema (mainly
projection types, but several other cases). These "known failures"
are filtered out of the report. Ideally I should fix the tests.

The test script requires the jsonschema Python package is available;
fortunately this is easily installed via conda (and presumably pip,
but I haven't tested this).

## Example

```
% python ../validation/validate.py ~/visualization/vega-lite-3.4.0.json 
There were only expected 'failures'.

```
