# Promises in CoffeeScript
# CoffeeScript works with JavaScript Promises

# Creating a Promise
fetchData = (url) ->
  new Promise (resolve, reject) ->
    # Simulate async operation
    setTimeout ->
      if url
        resolve({ url, data: "Sample data from #{url}" })
      else
        reject(new Error("URL is required"))
    , 1000

# Async/Await pattern (using generator functions)
sleep = (ms) ->
  new Promise (resolve) -> setTimeout resolve, ms

# Async function wrapper
asyncOperation = async ->
  await sleep(500)
  result: "Async result"
  timestamp: new Date()

# Promise chain
fetchUser = (id) ->
  new Promise (resolve, reject) ->
    setTimeout ->
      if id > 0
        resolve({ id, name: "User #{id}", email: "user#{id}@example.com" })
      else
        reject(new Error("Invalid user ID"))
    , 500

fetchPosts = (userId) ->
  new Promise (resolve) ->
    setTimeout ->
      resolve([
        { id: 1, userId, title: "First Post" }
        { id: 2, userId, title: "Second Post" }
      ])
    , 300

# Using Promises with CoffeeScript syntax
fetchData("https://api.example.com/data")
  .then (response) ->
    console.log "Data received:", response
    response
  .catch (error) ->
    console.error "Error:", error.message

# Promise.all for parallel operations
fetchMultiple = ->
  Promise.all [
    fetchUser(1)
    fetchUser(2)
    fetchUser(3)
  ]
  .then (users) ->
    console.log "All users:", users
    users
  .catch (error) ->
    console.error "Failed to fetch users:", error

# Promise.race for fastest response
fetchWithTimeout = (url, timeoutMs = 3000) ->
  Promise.race [
    fetchData(url)
    new Promise (_, reject) ->
      setTimeout ->
        reject(new Error("Timeout after #{timeoutMs}ms"))
      , timeoutMs
  ]

# Async/await in CoffeeScript (using do keyword)
do ->
  try
    user = await fetchUser(1)
    posts = await fetchPosts(user.id)
    console.log "User posts:", posts
  catch error
    console.error "Async error:", error.message

# Custom Promise-based utilities
delay = (ms) -> new Promise (resolve) -> setTimeout resolve, ms

sequence = (promises...) ->
  promises.reduce (
    (chain, promise) -> chain.then -> promise
  ), Promise.resolve()

parallel = (promises...) ->
  Promise.all promises

# Creating resolved/rejected Promises
resolvedPromise = Promise.resolve("Immediate value")
rejectedPromise = Promise.reject(new Error("Immediate error"))

# Handling settled Promises
Promise.allSettled([
  fetchUser(1)
  fetchUser(-1)
  fetchUser(2)
]).then (results) ->
  for result in results
    if result.status is "fulfilled"
      console.log "Success:", result.value
    else
      console.log "Failed:", result.reason

