## Building KDL Documents
##
## This example shows how to create KDL documents programmatically
## using the toKdlDoc, toKdlNode, and toKdlVal macros.

import ../src/kdl

# Create a complete document using the toKdlDoc macro
let config = toKdlDoc:
  server(host = "localhost", port = 8080):
    ssl #true
    workers 4

  database(driver = "postgres"):
    host "db.example.com"
    port 5432
    credentials:
      username "admin"
      password "secret"

  (i32)timeout(30)
  users "alice" "bob" "charlie"

echo "Generated config:"
echo config.pretty()

# Create individual nodes
let serverNode = toKdlNode:
  api[v2]("https://api.example.com", timeout = 30.5)

echo "\nServer node:"
echo serverNode.pretty()

# Create typed values
let port = toKdlVal(8080[i32])
let enabled = toKdlVal #true
let name = toKdlVal("MyApp")

echo "\nTyped values:"
echo "  Port: ", port.pretty()
echo "  Enabled: ", enabled.pretty()
echo "  Name: ", name.pretty()

# Build a document with helper functions
proc makeConfigNode(name: string, values: seq[(string, KdlVal)]): KdlNode =
  var node = initKNode(name)
  for (key, val) in values:
    node.props[key] = val
  node

let dbConfig = makeConfigNode("database", @[
  ("host", initKVal("localhost")),
  ("port", initKVal(5432)),
  ("name", initKVal("mydb"))
])

echo "\nBuilt database node:"
echo dbConfig.pretty()

# Combine nodes into a document
let fullConfig = @[config[0], config[1], dbConfig]
echo "\nCombined document:"
echo fullConfig.pretty()
