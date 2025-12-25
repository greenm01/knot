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
proc getChildProp(node: KdlNode, childName: string): Option[KdlVal] =
  for child in node.children:
    if child.name == childName and child.args.len > 0:
      return some(child.args[0])
  none(KdlVal)

# Extract app configuration
if config.findNode("app").isSome:
  let app = config.findNode("app").get
  echo "Application: ", app.getChildProp("name").get.getString()
  echo "Version: ", app.getChildProp("version").get.getString()
  echo "Debug mode: ", app.getChildProp("debug").get.getBool()

# Extract server configuration
if config.findNode("server").isSome:
  let server = config.findNode("server").get
  let host = server.getChildProp("host").get.getString()
  let port = server.getChildProp("port").get.getInt()
  let workers = server.getChildProp("workers").get.getInt()
  echo "\nServer: ", host, ":", port, " (", workers, " workers)"

# Extract database configuration
if config.findNode("database").isSome:
  let db = config.findNode("database").get
  echo "\nDatabase:"
  echo "  Driver: ", db.getChildProp("driver").get.getString()
  echo "  Host: ", db.getChildProp("host").get.getString()
  echo "  Port: ", db.getChildProp("port").get.getInt()
  echo "  Name: ", db.getChildProp("name").get.getString()

  # Access nested configuration
  for child in db.children:
    if child.name == "pool":
      echo "  Pool settings:"
      echo "    Min: ", child.getChildProp("min-connections").get.getInt()
      echo "    Max: ", child.getChildProp("max-connections").get.getInt()

# Extract logging configuration with multiple arguments
if config.findNode("logging").isSome:
  let logging = config.findNode("logging").get
  echo "\nLogging:"
  echo "  Level: ", logging.getChildProp("level").get.getString()
  echo "  Format: ", logging.getChildProp("format").get.getString()

  # Multiple arguments
  for child in logging.children:
    if child.name == "outputs":
      echo "  Outputs: ", child.args.mapIt(it.getString()).join(", ")
