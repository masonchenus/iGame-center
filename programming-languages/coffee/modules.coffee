# Modules in CoffeeScript
# CoffeeScript supports CommonJS-style modules (require/export)

# Creating a module with exports
# File: math_utils.coffee
MathUtils =
  PI: 3.14159
  
  square: (x) -> x * x
  
  circleArea: (radius) ->
    @PI * @square(radius)
  
  sum: (numbers...) ->
    numbers.reduce (acc, n) -> acc + n, 0
  
  average: (numbers...) ->
    return 0 if numbers.length is 0
    @sum(numbers...) / numbers.length

# Exporting (CommonJS style)
if typeof module isnt 'undefined' and module.exports
  module.exports = MathUtils

# Another module example
# File: string_utils.coffee
StringUtils =
  capitalize: (str) ->
    return "" unless str
    str.charAt(0).toUpperCase() + str[1...].toLowerCase()
  
  reverse: (str) ->
    str.split("").reverse().join("")
  
  truncate: (str, length = 50, suffix = "...") ->
    return str if str.length <= length
    str[0...length] + suffix
  
  slugify: (str) ->
    str.toLowerCase()
      .replace(/\s+/g, "-")
      .replace(/[^\w-]+/g, "")
      .replace(/--+/g, "-")
      .replace(/^-+|-+$/g, "")

if typeof module isnt 'undefined' and module.exports
  module.exports = StringUtils

# ES6 style module simulation
class UserManager
  constructor: ->
    @users = []
  
  addUser: (user) ->
    @users.push(user)
  
  getUsers: ->
    @users
  
  findByName: (name) ->
    @users.find (u) -> u.name is name

# Export as object
UserManagerModule =
  UserManager: UserManager
  createUserManager: -> new UserManager()

# Dynamic require example
loadModule = (modulePath) ->
  try
    require(modulePath) if typeof require isnt 'undefined'
  catch error
    console.error "Failed to load module: #{modulePath}", error
    null

