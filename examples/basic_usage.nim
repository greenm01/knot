## Basic Usage Example
##
## This example demonstrates the fundamental operations:
## - Parsing KDL from a string
## - Accessing nodes using navigation helpers
## - Working with different value types
## - Using defaults for optional values
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

# ============================================================================
# Navigation helpers (recommended for config files)
# ============================================================================

echo "=== Using navigation helpers ==="

# findNode returns Option[KdlNode]
let serverOpt = doc.findNode("server")
if serverOpt.isSome:
  let server = serverOpt.get
  
  # childString/childInt/childBool return value or default
  echo "Host: ", server.childString("host", "0.0.0.0")
  echo "Port: ", server.childInt("port", 3000)
  echo "SSL: ", server.childBool("ssl", false)
  echo "Timeout: ", server.childInt("timeout", 30), " (default, not in config)"

# findChild for nested access
let dbOpt = doc.findNode("database")
if dbOpt.isSome:
  let db = dbOpt.get
  echo "\nDatabase connection: ", db.childString("connection", "")
  echo "Pool size: ", db.childInt("pool-size", 5)
  echo "Timeout: ", db.childFloat("timeout", 10.0)

# hasChild for conditional logic
if serverOpt.isSome:
  let server = serverOpt.get
  if server.hasChild("ssl"):
    echo "\nSSL is configured"

# ============================================================================
# Direct access (for when you know the structure)
# ============================================================================

echo "\n=== Direct index access ==="

# Access nodes by index
echo "First node name: ", doc[0].name  # "server"

# Access arguments (ordered values)  
echo "Admin users: ", doc[3].args[0].kString(), ", ", doc[3].args[1].kString()

# Access type annotations (tags)
if doc[2].tag.isSome:
  echo "Log level tag: ", doc[2].tag.get

# ============================================================================
# childVal for raw value access
# ============================================================================

echo "\n=== Raw value access ==="

if dbOpt.isSome:
  let db = dbOpt.get
  
  # childVal returns Option[KdlVal]
  let poolVal = db.childVal("pool-size")
  if poolVal.isSome:
    echo "Pool value kind: ", poolVal.get.kind
    echo "Pool value: ", poolVal.get.kInt()

# ============================================================================
# Pretty printing
# ============================================================================

echo "\n=== Pretty printed ==="
echo doc.pretty()
