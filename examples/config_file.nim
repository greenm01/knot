## Config File Example
##
## This example shows how to read and work with a KDL configuration file.
## It demonstrates the navigation helpers for extracting configuration values.

import ../src/kdl, sequtils, strutils

# Example config content (normally you'd use parseKdlFile("config.kdl"))
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

# ============================================================================
# Using the new navigation helpers
# ============================================================================

# Extract app configuration using findNode and childString/childBool
if config.findNode("app").isSome:
  let app = config.findNode("app").get
  
  # childString/childInt/childBool return the value or a default
  echo "Application: ", app.childString("name", "Unknown")
  echo "Version: ", app.childString("version", "0.0.0")
  echo "Debug mode: ", app.childBool("debug", false)

# Extract server configuration
if config.findNode("server").isSome:
  let server = config.findNode("server").get
  
  # Using defaults for optional values
  let host = server.childString("host", "127.0.0.1")
  let port = server.childInt("port", 8080)
  let workers = server.childInt("workers", 1)
  let timeout = server.childInt("timeout", 30)  # Not in config, uses default
  
  echo "\nServer: ", host, ":", port, " (", workers, " workers, timeout: ", timeout, "s)"

# Extract database configuration
if config.findNode("database").isSome:
  let db = config.findNode("database").get
  echo "\nDatabase:"
  echo "  Driver: ", db.childString("driver", "sqlite")
  echo "  Host: ", db.childString("host", "localhost")
  echo "  Port: ", db.childInt("port", 5432)
  echo "  Name: ", db.childString("name", "app_db")

  # Access nested configuration using findChild
  let poolOpt = db.findChild("pool")
  if poolOpt.isSome:
    let pool = poolOpt.get
    echo "  Pool settings:"
    echo "    Min: ", pool.childInt("min-connections", 1)
    echo "    Max: ", pool.childInt("max-connections", 5)

# Extract logging configuration
let loggingOpt = config.findNode("logging")
if loggingOpt.isSome:
  let logging = loggingOpt.get
  echo "\nLogging:"
  echo "  Level: ", logging.childString("level", "warn")
  echo "  Format: ", logging.childString("format", "text")

  # For nodes with multiple arguments, use findChild + args
  let outputsOpt = logging.findChild("outputs")
  if outputsOpt.isSome:
    echo "  Outputs: ", outputsOpt.get.args.mapIt(it.kString()).join(", ")

# ============================================================================
# Using hasChild for conditional logic
# ============================================================================

echo "\n--- Feature detection ---"
if config.findNode("server").isSome:
  let server = config.findNode("server").get
  
  if server.hasChild("ssl"):
    echo "SSL is configured"
  else:
    echo "SSL is not configured (using defaults)"
    
  if server.hasChild("workers"):
    echo "Custom worker count: ", server.childInt("workers", 1)

# ============================================================================
# Using childVal for raw KdlVal access
# ============================================================================

echo "\n--- Raw value access ---"
if config.findNode("app").isSome:
  let app = config.findNode("app").get
  
  # childVal returns Option[KdlVal] for when you need the raw value
  let nameVal = app.childVal("name")
  if nameVal.isSome:
    echo "Name value kind: ", nameVal.get.kind
    echo "Name value: ", nameVal.get.kString()
