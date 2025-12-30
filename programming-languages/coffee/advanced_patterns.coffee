# Advanced CoffeeScript Patterns
# Exploring advanced features and design patterns

# === METAPROGRAMMING ===

# Dynamic property assignment
createDynamicObject = (prefix) ->
  handler =
    get: (target, prop) ->
      if prop of target
        target[prop]
      else
        "Property '#{prefix}_#{prop}' accessed"
  
  new Proxy({}, handler)

# Property interception with defineProperty
interceptProperty = (obj, prop, interceptor) ->
  value = obj[prop]
  Object.defineProperty obj, prop,
    get: -> interceptor(value)
    set: (newVal) -> value = interceptor(newVal)

# === DESIGN PATTERNS ===

# Singleton pattern
class Singleton
  instance = null
  
  constructor: ->
    unless Singleton.instance
      Singleton.instance = @
    Singleton.instance

# Factory pattern
class AnimalFactory
  createAnimal: (type, name) ->
    switch type
      when "dog" then new Dog(name)
      when "cat" then new Cat(name)
      when "bird" then new Bird(name)
      else throw new Error("Unknown animal type")

# Observer pattern
class Subject
  constructor: ->
    @observers = []
  
  subscribe: (observer) ->
    @observers.push(observer)
  
  unsubscribe: (observer) ->
    @observers = @observers.filter (o) -> o isnt observer
  
  notify: (data) ->
    observer.update?(data) for observer in @observers

# Strategy pattern
class PaymentProcessor
  constructor: ->
    @strategies = {}
  
  registerStrategy: (name, strategy) ->
    @strategies[name] = strategy
  
  process: (strategyName, amount) ->
    @strategies[strategyName]?.(amount)

# === ADVANCED FUNCTIONS ===

# Memoization with custom key generator
memoizeWith = (fn, keyGenerator = (args...) -> JSON.stringify(args)) ->
  cache = {}
  (args...) ->
    key = keyGenerator(args...)
    return cache[key] if key of cache
    result = fn.apply(this, args)
    cache[key] = result

# Partial application with placeholders
partialRight = (fn, args...) ->
  (more...) -> fn(more..., args...)

# Thunk creator
thunk = (fn) ->
  args = []
  context = this
  ->
    return fn.apply(context, args) if args.length
    args = [arguments...]

# === ADVANCED CLASS PATTERNS ===

# Decorator pattern for classes
decorator = (fn) ->
  ->
    result = fn.apply(this, arguments)
    # Add additional behavior
    result

# Chainable API pattern
class QueryBuilder
  constructor: ->
    @conditions = []
    @sort = []
    @limitCount = null
  
  where: (field, operator, value) ->
    @conditions.push({ field, operator, value })
    @
  
  orderBy: (field, direction = "asc") ->
    @sort.push({ field, direction })
    @
  
  take: (n) ->
    @limitCount = n
    @
  
  toQuery: ->
    query = "SELECT * FROM table"
    query += " WHERE #{c.field} #{c.operator} #{c.value}" for c in @conditions
    query += " ORDER BY #{s.field} #{s.direction}" for s in @sort
    query += " LIMIT #{@limitCount}" if @limitCount
    query

