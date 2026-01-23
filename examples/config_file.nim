## Config File Example
##
## This example shows how to read and work with a KDL configuration file.
## It demonstrates practical patterns for extracting configuration values.

import ../src/kdl, tables, options

# Example config content (normally read from a file)
let configContent = """
app {
  name "MyApp"
  version "1.0.0"
  debug #false
}

server {
  host "0.0.0.0"
  port 3000
  workers 4
}

database {
  driver "postgres"
  host "localhost"
  port 5432
  name "myapp_db"
  pool {
    min-connections 2
    max-connections 10
  }
}

logging {
  level "info"
  format "json"
  outputs "stdout" "file"
}
"""

# Parse the config
let config = parseKdl(configContent)

# Helper to find a node by name
proc findNode(doc: KdlDoc, name: string): Option[KdlNode] =
  for node in doc:
    if node.name == name:
      return some(node)
  none(KdlNode)

# Helper to get a child property value
proc childProp(node: KdlNode, childName: string): Option[KdlVal] =
  for child in node.children:
    if child.name == childName and child.args.len > 0:
      return some(child.args[0])
  none(KdlVal)

# Extract app configuration
if config.findNode("app").isSome:
  let app = config.findNode("app").get
  echo "Application: ", app.childProp("name").get.kString()
  echo "Version: ", app.childProp("version").get.kString()
  echo "Debug mode: ", app.childProp("debug").get.kBool()

# Extract server configuration
if config.findNode("server").isSome:
  let server = config.findNode("server").get
  let host = server.childProp("host").get.kString()
  let port = server.childProp("port").get.kInt()
  let workers = server.childProp("workers").get.kInt()
  echo "\nServer: ", host, ":", port, " (", workers, " workers)"

# Extract database configuration
if config.findNode("database").isSome:
  let db = config.findNode("database").get
  echo "\nDatabase:"
  echo "  Driver: ", db.childProp("driver").get.kString()
  echo "  Host: ", db.childProp("host").get.kString()
  echo "  Port: ", db.childProp("port").get.kInt()
  echo "  Name: ", db.childProp("name").get.kString()

  # Access nested configuration
  for child in db.children:
    if child.name == "pool":
      echo "  Pool settings:"
      echo "    Min: ", child.childProp("min-connections").get.kInt()
      echo "    Max: ", child.childProp("max-connections").get.kInt()

# Extract logging configuration with multiple arguments
if config.findNode("logging").isSome:
  let logging = config.findNode("logging").get
  echo "\nLogging:"
  echo "  Level: ", logging.childProp("level").get.kString()
  echo "  Format: ", logging.childProp("format").get.kString()

  # Multiple arguments
  for child in logging.children:
    if child.name == "outputs":
      echo "  Outputs: ", child.args.mapIt(it.kString()).join(", ")
