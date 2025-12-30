# Event Handling in CoffeeScript
# CoffeeScript provides clean event handling syntax

# Event listener helper
on = (element, eventType, handler) ->
  return unless element
  element.addEventListener(eventType, handler)
  -> element.removeEventListener(eventType, handler)

# One-time event listener
once = (element, eventType, handler) ->
  return unless element
  wrappedHandler = (e) ->
    handler(e)
    element.removeEventListener(eventType, wrappedHandler)
  element.addEventListener(eventType, wrappedHandler)

# Event delegation
delegate = (parent, eventType, selector, handler) ->
  parent.addEventListener eventType, (e) ->
    target = e.target.closest(selector)
    if target
      e.delegateTarget = target
      handler(e)

# Custom event emitter class
class EventEmitter
  constructor: ->
    @events = {}
  
  on: (event, handler) ->
    @events[event] ?= []
    @events[event].push(handler)
    @
  
  off: (event, handler) ->
    return @ unless @events[event]
    index = @events[event].indexOf(handler)
    @events[event].splice(index, 1) if index > -1
    @
  
  once: (event, handler) ->
    @on event, (...args) ->
      handler(...args)
      @off(event, handler)
    @
  
  emit: (event, data...) ->
    return @ unless @events[event]
    handler(data...) for handler in @events[event]
    @

# Debounce function
debounce = (fn, delay) ->
  timeout = null
  (...args) ->
    clearTimeout(timeout)
    timeout = setTimeout (=> fn.apply(this, args)), delay

# Throttle function
throttle = (fn, limit) ->
  inThrottle = false
  (...args) ->
    return if inThrottle
    fn.apply(this, args)
    inThrottle = true
    setTimeout (=> inThrottle = false), limit

# Practical example - Interactive component
class Accordion extends EventEmitter
  constructor: (@container) ->
    super()
    @panels = @container.querySelectorAll(".panel")
    @setupEvents()
  
  setupEvents: ->
    for panel in @panels
      header = panel.querySelector(".panel-header")
      header.addEventListener "click", =>
        @toggle(panel)
  
  toggle: (panel) ->
    isOpen = panel.classList.contains("open")
    @closeAll()
    unless isOpen
      panel.classList.add("open")
      @emit("expand", panel)
    else
      @emit("collapse", panel)
  
  closeAll: ->
    panel.classList.remove("open") for panel in @panels

