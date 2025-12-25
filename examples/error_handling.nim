## Error Handling Example
##
## This example demonstrates how to properly handle parsing errors
## and work with invalid KDL syntax.

import ../src/kdl

# Helper to safely parse KDL with error handling
proc safeParse(content: string): bool =
  try:
    let doc = parseKdl(content)
    echo "✓ Parsed successfully: ", doc.len, " nodes"
    return true
  except KdlError as e:
    echo "✗ Parse error: ", e.msg
    return false

echo "=== Valid KDL ==="
discard safeParse("""
  node "value" key=123
""")

echo "\n=== Missing quotes ==="
discard safeParse("""
  node value
""")

echo "\n=== Invalid keyword usage ==="
echo "Keywords like 'true' must use #true or be quoted:"
discard safeParse("""
  node true
""")

echo "\n=== Correct keyword usage ==="
discard safeParse("""
  node #true
  other "true"
""")

echo "\n=== Unclosed children block ==="
discard safeParse("""
  node {
    child
""")

echo "\n=== Invalid property key ==="
echo "Keywords cannot be property keys:"
discard safeParse("""
  node true=value
""")

echo "\n=== Multiple children blocks ==="
echo "A node can only have one children block:"
discard safeParse("""
  node { child1 } { child2 }
""")

echo "\n=== Working with files ==="
# You can also read from files
# try:
#   let doc = parseKdl(readFile("config.kdl"))
#   echo "Config loaded successfully"
# except IOError as e:
#   echo "Could not read file: ", e.msg
# except KdlError as e:
#   echo "Invalid KDL syntax: ", e.msg

echo "\nDone!"
