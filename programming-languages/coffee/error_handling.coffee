# Error Handling in CoffeeScript
# CoffeeScript provides try/catch/finally syntax

# Basic try-catch
parseJSON = (jsonString) ->
  try
    JSON.parse(jsonString)
  catch error
    console.error "Failed to parse JSON:", error.message
    null
  finally
    console.log "Parse attempt completed"

# Custom error class
class ValidationError extends Error
  constructor: (message, field) ->
    super(message)
    @name = "ValidationError"
    @field = field
    @timestamp = new Date()

# Function that throws custom error
validateUser = (user) ->
  unless user?
    throw new ValidationError("User object is required", "user")
  
  unless user.name and user.name.trim().length > 0
    throw new ValidationError("Name is required", "name")
  
  unless user.email and user.email.includes("@")
    throw new ValidationError("Invalid email format", "email")
  
  unless user.age >= 18
    throw new ValidationError("User must be 18 or older", "age")
  
  valid: true
  user: user

# Handling custom errors
processUser = (userData) ->
  try
    result = validateUser(userData)
    console.log "Validation successful:", result
    result
  catch error
    if error instanceof ValidationError
      console.warn "Validation failed for #{error.field}:", error.message
    else
      console.error "Unexpected error:", error
    null

# Try-catch in loops
processItems = (items) ->
  results = []
  for item, index in items
    try
      processed = validateItem(item)
      results.push({ index, success: true, data: processed })
    catch error
      results.push({ index, success: false, error: error.message })
  results

validateItem = (item) ->
  throw new Error("Invalid item") unless item?.id

# Async error handling
asyncOperation = (shouldFail = false) ->
  new Promise (resolve, reject) ->
    setTimeout ->
      if shouldFail
        reject(new Error("Operation failed"))
      else
        resolve({ success: true })
    , 500

# Async/await error handling
do async ->
  try
    result = await asyncOperation(false)
    console.log "Async result:", result
  catch error
    console.error "Async error:", error.message
  finally
    console.log "Async operation finished"

# Error boundary pattern
class ErrorBoundary
  constructor: ->
    @errors = []
  
  wrap: (fn) ->
    =>
      try
        fn.apply(this, arguments)
      catch error
        @handleError(error)
  
  handleError: (error) ->
    @errors.push(error)
    console.error "Caught error:", error.message
    null

# Fallback with try-catch
safeDivide = (a, b) ->
  try
    return "Division by zero" if b is 0
    a / b
  catch error
    "Error: #{error.message}"

