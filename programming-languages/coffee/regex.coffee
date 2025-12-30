# Regular Expressions in CoffeeScript
# CoffeeScript uses JavaScript RegExp syntax

# RegExp creation
emailPattern = /^[^\s@]+@[^\s@]+\.[^\s@]+$/
phonePattern = /^\+?[\d\s-]{10,}$/
urlPattern = /^https?:\/\/[\w.-]+(?:\/[\w.-]*)*$/

# Test method
isValidEmail = (email) -> emailPattern.test(email)

# String matching with regex
text = "The quick brown fox jumps over the lazy dog"
matches = text.match(/\w+/g)

# Replace with function
capitalizeWords = (str) ->
  str.replace(/\b\w/g, (match) -> match.toUpperCase())

# Extract parts with groups
parseDate = (dateString) ->
  pattern = /^(\d{4})-(\d{2})-(\d{2})$/
  match = dateString.match(pattern)
  return null unless match
  year: parseInt(match[1]), month: parseInt(match[2]), day: parseInt(match[3])

# Validation helpers
validators =
  email: (email) -> /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)
  
  phone: (phone) -> /^\+?[\d\s-]{10,}$/.test(phone)
  
  url: (url) -> /^https?:\/\/[\w.-]+(?:\/[\w.-]*)*$/.test(url)
  
  hexColor: (color) -> /^#?([a-fA-F0-9]{6}|[a-fA-F0-9]{3})$/.test(color)
  
  uuid: (id) -> /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(id)

# Parsing utilities
extractHashtags = (text) ->
  text.match(/#[a-zA-Z0-9_]+/g) or []

extractMentions = (text) ->
  text.match(/@[a-zA-Z0-9_]+/g) or []

extractEmails = (text) ->
  text.match(/[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}/g) or []

# Sanitization
sanitizeHTML = (str) ->
  str.replace(/[&<>"']/g, (match) ->
    switch match
      when "&" then "&amp;"
      when "<" then "<"
      when ">" then ">"
      when '"' then """
      when "'" then "&#39;"
  )

# Slugify
slugify = (text) ->
  text.toLowerCase()
    .replace(/[^\w\s-]/g, "")
    .replace(/[\s_-]+/g, "-")
    .replace(/^-+|-+$/g, "")

# Truncate with ellipsis
truncate = (str, length = 100) ->
  return str if str.length <= length
  str[0...length].replace(/\s+\S*$/, "") + "..."

# Count words
countWords = (text) ->
  text.trim().split(/\s+/).filter((w) -> w.length > 0).length

# Validate password strength
validatePassword = (password) ->
  checks = [
    [/.{8,}/, "at least 8 characters"]
    [/[a-z]/, "lowercase letter"]
    [/[A-Z]/, "uppercase letter"]
    [/\d/, "number"]
    [/[@$!%*?&]/, "special character"]
  ]
  
  errors = []
  for [pattern, message] in checks
    errors.push(message) unless pattern.test(password)
  
  valid: errors.length is 0
  errors: errors

