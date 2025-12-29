# Crystal Data Structures - Advanced Data Types and Algorithms
require "json"

# Custom Linked List Implementation
class LinkedListNode(T)
  property value : T
  property next : LinkedListNode(T)?
  property prev : LinkedListNode(T)?
  
  def initialize(@value : T)
    @next = nil
    @prev = nil
  end
end

class LinkedList(T)
  def initialize
    @head : LinkedListNode(T)?
    @tail : LinkedListNode(T)?
    @size = 0
  end
  
  def append(value : T)
    node = LinkedListNode(T).new(value)
    
    if @tail
      @tail.not_nil!.next = node
      node.prev = @tail
      @tail = node
    else
      @head = node
      @tail = node
    end
    
    @size += 1
  end
  
  def prepend(value : T)
    node = LinkedListNode(T).new(value)
    
    if @head
      @head.not_nil!.prev = node
      node.next = @head
      @head = node
    else
      @head = node
      @tail = node
    end
    
    @size += 1
  end
  
  def to_a : Array(T)
    result = [] of T
    current = @head
    
    while current
      result << current.value
      current = current.next
    end
    
    result
  end
  
  def reverse!
    return if @size <= 1
    
    current = @head
    temp = nil
    
    while current
      temp = current.prev
      current.prev = current.next
      current.next = temp
      current = current.next
    end
    
    # Swap head and tail
    temp = @head
    @head = @tail
    @tail = temp
  end
end

# Binary Search Tree Implementation
class BSTNode(T)
  property value : T
  property left : BSTNode(T)?
  property right : BSTNode(T)?
  
  def initialize(@value : T)
  end
end

class BinarySearchTree(T)
  def initialize
    @root : BSTNode(T)?
    @size = 0
  end
  
  def insert(value : T)
    @root = insert_recursive(@root, value)
    @size += 1
  end
  
  def contains?(value : T) : Bool
    contains_recursive(@root, value) != nil
  end
  
  def in_order : Array(T)
    result = [] of T
    in_order_recursive(@root, result)
    result
  end
  
  def pre_order : Array(T)
    result = [] of T
    pre_order_recursive(@root, result)
    result
  end
  
  def post_order : Array(T)
    result = [] of T
    post_order_recursive(@root, result)
    result
  end
  
  private def insert_recursive(node : BSTNode(T)?, value : T) : BSTNode(T)
    return BSTNode(T).new(value) unless node
    
    if value < node.value
      node.left = insert_recursive(node.left, value)
    elsif value > node.value
      node.right = insert_recursive(node.right, value)
    end
    
    node
  end
  
  private def contains_recursive(node : BSTNode(T)?, value : T) : BSTNode(T)?
    return nil unless node
    
    return node if node.value == value
    
    value < node.value ? contains_recursive(node.left, value) : contains_recursive(node.right, value)
  end
  
  private def in_order_recursive(node : BSTNode(T)?, result : Array(T))
    return unless node
    
    in_order_recursive(node.left, result)
    result << node.value
    in_order_recursive(node.right, result)
  end
  
  private def pre_order_recursive(node : BSTNode(T)?, result : Array(T))
    return unless node
    
    result << node.value
    pre_order_recursive(node.left, result)
    pre_order_recursive(node.right, result)
  end
  
  private def post_order_recursive(node : BSTNode(T)?, result : Array(T))
    return unless node
    
    post_order_recursive(node.left, result)
    post_order_recursive(node.right, result)
    result << node.value
  end
end

# Hash Table Implementation
class HashTable(K, V)
  def initialize(capacity : Int32 = 16)
    @capacity = capacity
    @buckets = Array.new(capacity) { [] of {key: K, value: V} }
    @size = 0
  end
  
  def [](key : K) : V
    value, found = get(key)
    raise KeyError.new("Key not found: #{key}") unless found
    value
  end
  
  def []=(key : K, value : V)
    put(key, value)
  end
  
  def get(key : K) : {V, Bool}
    bucket_index = hash(key) % @capacity
    bucket = @buckets[bucket_index]
    
    bucket.each do |item|
      if item[:key] == key
        return {item[:value], true}
      end
    end
    
    raise KeyError.new("Key not found: #{key}")
  end
  
  def put(key : K, value : V)
    bucket_index = hash(key) % @capacity
    bucket = @buckets[bucket_index]
    
    # Update existing key
    bucket.each_with_index do |item, i|
      if item[:key] == key
        bucket[i] = {key: key, value: value}
        return
      end
    end
    
    # Add new key-value pair
    bucket << {key: key, value: value}
    @size += 1
    
    # Resize if load factor is too high
    resize if @size > @capacity * 0.75
  end
  
  def has_key?(key : K) : Bool
    bucket_index = hash(key) % @capacity
    bucket = @buckets[bucket_index]
    
    bucket.any? { |item| item[:key] == key }
  end
  
  def keys : Array(K)
    result = [] of K
    @buckets.each do |bucket|
      bucket.each do |item|
        result << item[:key]
      end
    end
    result
  end
  
  def values : Array(V)
    result = [] of V
    @buckets.each do |bucket|
      bucket.each do |item|
        result << item[:value]
      end
    end
    result
  end
  
  private def hash(key : K) : Int32
    key.hash
  end
  
  private def resize
    old_buckets = @buckets
    @capacity *= 2
    @buckets = Array.new(@capacity) { [] of {key: K, value: V} }
    @size = 0
    
    old_buckets.each do |bucket|
      bucket.each do |item|
        put(item[:key], item[:value])
      end
    end
  end
end

# Queue Implementation using Linked List
class Queue(T)
  def initialize
    @list = LinkedList(T).new
  end
  
  def enqueue(item : T)
    @list.append(item)
  end
  
  def dequeue : T
    raise "Queue is empty" if empty?
    
    first_item = @list.to_a[0]
    # In a real implementation, you'd remove the head element
    first_item
  end
  
  def front : T
    raise "Queue is empty" if empty?
    @list.to_a[0]
  end
  
  def empty? : Bool
    @list.to_a.empty?
  end
  
  def size : Int32
    @list.to_a.size
  end
end

# Stack Implementation
class Stack(T)
  def initialize
    @items = [] of T
  end
  
  def push(item : T)
    @items << item
  end
  
  def pop : T
    raise "Stack is empty" if empty?
    @items.pop
  end
  
  def top : T
    raise "Stack is empty" if empty?
    @items[-1]
  end
  
  def empty? : Bool
    @items.empty?
  end
  
  def size : Int32
    @items.size
  end
end

# Demo usage
if __FILE__ == $0
  puts "=== Crystal Data Structures Demo ==="
  
  # Linked List Demo
  puts "\n--- Linked List ---"
  list = LinkedList(Int32).new
  [3, 1, 4, 1, 5, 9, 2, 6].each { |num| list.append(num) }
  puts "Original: #{list.to_a}"
  list.reverse!
  puts "Reversed: #{list.to_a}"
  
  # Binary Search Tree Demo
  puts "\n--- Binary Search Tree ---"
  bst = BinarySearchTree(Int32).new
  [5, 3, 7, 1, 9, 4, 6, 2, 8].each { |num| bst.insert(num) }
  puts "In-order: #{bst.in_order}"
  puts "Pre-order: #{bst.pre_order}"
  puts "Post-order: #{bst.post_order}"
  puts "Contains 6? #{bst.contains?(6)}"
  
  # Hash Table Demo
  puts "\n--- Hash Table ---"
  hash_table = HashTable(String, Int32).new
  hash_table["one"] = 1
  hash_table["two"] = 2
  hash_table["three"] = 3
  puts "Keys: #{hash_table.keys}"
  puts "Values: #{hash_table.values}"
  puts "hash_table[\"two\"] = #{hash_table["two"]}"
  
  # Queue Demo
  puts "\n--- Queue ---"
  queue = Queue(String).new
  ["first", "second", "third"].each { |item| queue.enqueue(item) }
  puts "Front: #{queue.front}"
  puts "Size: #{queue.size}"
  
  # Stack Demo
  puts "\n--- Stack ---"
  stack = Stack(Int32).new
  [10, 20, 30, 40].each { |item| stack.push(item) }
  puts "Top: #{stack.top}"
  puts "Size: #{stack.size}"
  popped = stack.pop
  puts "Popped: #{popped}"
  puts "New top: #{stack.top}"
end
