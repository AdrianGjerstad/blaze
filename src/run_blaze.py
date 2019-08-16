import blaze
import sys

if len(sys.argv) < 2:
  raise Exception('Fatal error: No file given.')

f = open(sys.argv[1], "r")

result, error = blaze.run('<stdin>', f.read())

if error:
    print(error.as_string())
elif result:
    print("fetch < " + (result.__repr__()))