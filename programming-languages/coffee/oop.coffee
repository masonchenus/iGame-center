# Object-Oriented Programming in CoffeeScript
# CoffeeScript provides class syntax and OOP features

# Base class with constructor and methods
class Vehicle
  constructor: (@make, @model, @year = 2020) ->
    @speed = 0
    @isRunning = false
  
  start: ->
    @isRunning = true
    "#{@make} #{@model} is now running"
  
  stop: ->
    @isRunning = false
    @speed = 0
    "#{@make} #{@model} is stopped"
  
  accelerate: (amount) ->
    return "Cannot accelerate - vehicle is off" unless @isRunning
    @speed += amount
    "Speed: #{@speed} km/h"
  
  brake: ->
    @speed = Math.max(0, @speed - 10)
    "Braking... Speed: #{@speed} km/h"

# Inheritance - Car class
class Car extends Vehicle
  constructor: (@make, @model, @year, @doors = 4) ->
    super(@make, @model, @year)
    @fuel = 100
  
  drive: ->
    if @isRunning
      @fuel = Math.max(0, @fuel - 5)
      "Driving with #{@fuel}% fuel"
    else
      "Start the car first"
  
  getInfo: ->
    "#{@year} #{@make} #{@model} with #{@doors} doors"

# Inheritance - Motorcycle class
class Motorcycle extends Vehicle
  constructor: (@make, @model, @year, @engineType = "V-Twin") ->
    super(@make, @model, @year)
    @leanAngle = 0
  
  lean: (angle) ->
    @leanAngle = angle
    "Leaning at #{angle} degrees"
  
  wheelie: ->
    return "Start the motorcycle first" unless @isRunning
    "Performing a wheelie!"

# Polymorphism - method overriding
class ElectricCar extends Car
  constructor: (@make, @model, @year) ->
    super(@make, @model, @year, 4)
    @battery = 100
    @fuel = 0
  
  drive: ->
    if @isRunning
      @battery = Math.max(0, @battery - 10)
      "Electric driving with #{@battery}% battery"
    else
      "Start the electric car first"
  
  charge: (amount = 100) ->
    @battery = Math.min(100, @battery + amount)
    "Charged to #{@battery}%"

# Composition pattern
class Engine
  constructor: (@type, @horsepower) ->
    @isRunning = false
  
  start: ->
    @isRunning = true
    "#{@type} engine started"
  
  stop: ->
    @isRunning = false
    "#{@type} engine stopped"

class Boat
  constructor: (@name, @engine) ->
  
  startEngine: ->
    @engine.start()
  
  stopEngine: ->
    @engine.stop()

# Mixin pattern
module =
  Serializable: ->
    toJSON: ->
      obj = {}
      for key, value of this
        continue if typeof value is 'function'
        obj[key] = value
      obj

# Using mixin
class Person
  constructor: (@name, @age) ->
  
  greet: -> "Hello, I'm #{@name}"

Object.assign(Person.prototype, Serializable)

