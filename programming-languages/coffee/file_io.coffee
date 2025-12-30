# File I/O in CoffeeScript
# CoffeeScript uses Node.js fs module for file operations

# Note: This code requires Node.js environment
fs = require("fs") if typeof require isnt "undefined"
path = require("path") if typeof require isnt "undefined"

# Reading files
readFile = (filePath, encoding = "utf8") ->
  new Promise (resolve, reject) ->
    return reject(new Error("fs module not available")) unless fs?
    fs.readFile filePath, encoding, (err, data) ->
      return reject(err) if err
      resolve(data)

readFileSync = (filePath, encoding = "utf8") ->
  return fs.readFileSync(filePath, encoding) if fs?
  throw new Error("fs module not available")

# Writing files
writeFile = (filePath, data, encoding = "utf8") ->
  new Promise (resolve, reject) ->
    return reject(new Error("fs module not available")) unless fs?
    fs.writeFile filePath, data, encoding, (err) ->
      return reject(err) if err
      resolve(true)

writeFileSync = (filePath, data, encoding = "utf8") ->
  return fs.writeFileSync(filePath, data, encoding) if fs?
  throw new Error("fs module not available")

# Appending to files
appendFile = (filePath, data, encoding = "utf8") ->
  new Promise (resolve, reject) ->
    return reject(new Error("fs module not available")) unless fs?
    fs.appendFile filePath, data, encoding, (err) ->
      return reject(err) if err
      resolve(true)

# Directory operations
readDir = (dirPath) ->
  new Promise (resolve, reject) ->
    return reject(new Error("fs module not available")) unless fs?
    fs.readdir dirPath, (err, files) ->
      return reject(err) if err
      resolve(files)

# Check if file/directory exists
exists = (filePath) ->
  return new Promise (resolve) ->
    return resolve(false) unless fs?
    fs.exists filePath, resolve

existsSync = (filePath) ->
  return fs.existsSync(filePath) if fs?
  false

# Get file stats
stat = (filePath) ->
  new Promise (resolve, reject) ->
    return reject(new Error("fs module not available")) unless fs?
    fs.stat filePath, (err, stats) ->
      return reject(err) if err
      resolve(stats)

# Create directory
mkdir = (dirPath, recursive = true) ->
  new Promise (resolve, reject) ->
    return reject(new Error("fs module not available")) unless fs?
    fs.mkdir dirPath, { recursive }, (err) ->
      return reject(err) if err
      resolve(dirPath)

# Delete file
unlink = (filePath) ->
  new Promise (resolve, reject) ->
    return reject(new Error("fs module not available")) unless fs?
    fs.unlink filePath, (err) ->
      return reject(err) if err
      resolve(true)

# JSON file helper
readJSON = (filePath) ->
  readFile(filePath)
    .then (content) -> JSON.parse(content)

writeJSON = (filePath, data) ->
  writeFile(filePath, JSON.stringify(data, null, 2))

