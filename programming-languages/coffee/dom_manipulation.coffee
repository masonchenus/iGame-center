# DOM Manipulation in CoffeeScript
# CoffeeScript works with browser DOM APIs

# Element selection
$ = (selector) -> document.querySelector(selector)
$$ = (selector) -> document.querySelectorAll(selector)

# Create element helper
createElement = (tag, attributes = {}, children = []) ->
  element = document.createElement(tag)
  
  for key, value of attributes
    if key is "className"
      element.className = value
    else if key is "style" and typeof value is "object"
      for prop, val of value
        element.style[prop] = val
    else if key.startsWith("data-")
      element.setAttribute(key, value)
    else if key of element
      element[key] = value
    else
      element.setAttribute(key, value)
  
  for child in children
    if typeof child is "string"
      element.appendChild(document.createTextNode(child))
    else if child instanceof Node
      element.appendChild(child)
  
  element

# Add class
addClass = (element, className) ->
  element.classList.add(className)

# Remove class
removeClass = (element, className) ->
  element.classList.remove(className)

# Toggle class
toggleClass = (element, className) ->
  element.classList.toggle(className)

# Has class
hasClass = (element, className) ->
  element.classList.contains(className)

# Event delegation
delegate = (parent, eventType, selector, handler) ->
  parent.addEventListener eventType, (e) ->
    target = e.target.closest(selector)
    if target
      e.delegateTarget = target
      handler(e)

# Animation helper
animate = (element, properties, duration = 300) ->
  for key, value of properties
    element.style.transition = "#{key} #{duration}ms ease"
    element.style[key] = value

# DOM ready handler
onDOMReady = (fn) ->
  if document.readyState is "loading"
    document.addEventListener "DOMContentLoaded", fn
  else
    fn()

# Safe element removal
removeElement = (element) ->
  element?.parentNode?.removeChild(element)

# Insert after
insertAfter = (newNode, referenceNode) ->
  referenceNode.parentNode.insertBefore(newNode, referenceNode.nextSibling)

# Clone element
cloneElement = (element, deep = true) ->
  element.cloneNode(deep)

