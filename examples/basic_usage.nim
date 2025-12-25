## Basic Usage Example
##
## This example demonstrates the fundamental operations:
## - Parsing KDL from a string
## - Accessing nodes, arguments, and properties
## - Working with different value types
## - Pretty printing

import ../src/kdl

# Parse a KDL document
let doc = parseKdl("""
  // Configuration example
  server {
    host "localhost"
    port 8080
    ssl #true
  }

  database {
    connection "postgresql://localhost/mydb"
    pool-size 10
    timeout 30.5
  }

  (log-level)"info"
  admin-users "alice" "bob"
""")

# Access nodes by index
echo "First node name: ", doc[0].name  # "server"

# Access arguments (ordered values)
echo "Admin users: ", doc[3].args[0].getString(), ", ", doc[3].args[1].getString()

# Access properties (key-value pairs)
echo "Server host: ", doc[0].children[0].args[0].getString()
echo "Server port: ", doc[0].children[1].args[0].getInt()

# Access boolean values (KDL 2.0 uses #true/#false)
echo "SSL enabled: ", doc[0].children[2].args[0].getBool()

# Access type annotations (tags)
if doc[2].tag.isSome:
  echo "Log level tag: ", doc[2].tag.get

# Pretty print the document
echo "\nPretty printed:"
echo doc.pretty()
