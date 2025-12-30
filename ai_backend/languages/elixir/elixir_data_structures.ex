# Elixir Data Structures - Advanced Data Structure Implementations
defmodule ElixirDataStructures do
  # Singly Linked List Implementation
  defmodule SinglyLinkedList do
    defstruct head: nil, tail: nil, length: 0

    def new do
      %SinglyLinkedList{head: nil, tail: nil, length: 0}
    end

    def new([head | tail]) do
      elements = [head | tail]
      %{SinglyLinkedList.new() | head: head, tail: tail, length: length(elements)}
    end

    def append(list, element) when is_map(list) do
      %SinglyLinkedList{head: head, tail: tail, length: length} = list
      new_tail = if tail do
        tail ++ [element]
      else
        [element]
      end

      %{list | tail: new_tail, length: length + 1}
    end

    def prepend(list, element) when is_map(list) do
      %SinglyLinkedList{head: head, tail: tail, length: length} = list
      new_head = if head do
        [element | head]
      else
        [element]
      end

      %{list | head: new_head, length: length + 1}
    end

    def get(list, index) when is_map(list) and is_integer(index) and index >= 0 do
      %SinglyLinkedList{head: head} = list
      if head and index < Enum.count(head) do
        Enum.at(head, index)
      else
        nil
      end
    end

    def remove(list, index) when is_map(list) and is_integer(index) and index >= 0 do
      %SinglyLinkedList{head: head, length: length} = list
      if head and index < Enum.count(head) do
        new_head = List.delete_at(head, index)
        %{list | head: new_head, length: length - 1}
      else
        list
      end
    end

    def to_list(list) when is_map(list) do
      %SinglyLinkedList{head: head} = list
      head || []
    end

    def reverse(list) when is_map(list) do
      %SinglyLinkedList{head: head} = list
      if head, do: Enum.reverse(head), else: []
    end
  end

  # Binary Search Tree Implementation
  defmodule BinarySearchTree do
    defstruct root: nil, size: 0

    def new do
      %BinarySearchTree{root: nil, size: 0}
    end

    def insert(tree, value) when is_map(tree) do
      new_root = insert_node(tree.root, value)
      %{tree | root: new_root, size: tree.size + 1}
    end

    def search(tree, value) when is_map(tree) do
      search_node(tree.root, value) != nil
    end

    def find(tree, value) when is_map(tree) do
      search_node(tree.root, value)
    end

    def in_order_traversal(tree) when is_map(tree) do
      in_order(tree.root, [])
    end

    def pre_order_traversal(tree) when is_map(tree) do
      pre_order(tree.root, [])
    end

    def post_order_traversal(tree) when is_map(tree) do
      post_order(tree.root, [])
    end

    def minimum(tree) when is_map(tree) do
      minimum_node(tree.root)
    end

    def maximum(tree) when is_map(tree) do
      maximum_node(tree.root)
    end

    def height(tree) when is_map(tree) do
      height_node(tree.root)
    end

    def is_balanced?(tree) when is_map(tree) do
      balanced_height(tree.root) != -1
    end

    defp insert_node(nil, value) do
      %{value: value, left: nil, right: nil}
    end

    defp insert_node(node, value) do
      cond do
        value < node.value ->
          %{node | left: insert_node(node.left, value)}
        value > node.value ->
          %{node | right: insert_node(node.right, value)}
        true ->
          node  # Value already exists
      end
    end

    defp search_node(nil, _value), do: nil
    defp search_node(node, value) do
      cond do
        value < node.value -> search_node(node.left, value)
        value > node.value -> search_node(node.right, value)
        true -> node
      end
    end

    defp minimum_node(nil), do: nil
    defp minimum_node(node) do
      if node.left == nil do
        node
      else
        minimum_node(node.left)
      end
    end

    defp maximum_node(nil), do: nil
    defp maximum_node(node) do
      if node.right == nil do
        node
      else
        maximum_node(node.right)
      end
    end

    defp height_node(nil), do: 0
    defp height_node(node) do
      left_height = height_node(node.left)
      right_height = height_node(node.right)
      max(left_height, right_height) + 1
    end

    defp balanced_height(nil), do: 0
    defp balanced_height(node) do
      left_height = balanced_height(node.left)
      right_height = balanced_height(node.right)

      if left_height == -1 or right_height == -1 or abs(left_height - right_height) > 1 do
        -1  # Unbalanced
      else
        max(left_height, right_height) + 1
      end
    end

    defp in_order(nil, acc), do: acc
    defp in_order(node, acc) do
      acc = in_order(node.left, acc)
      acc = acc ++ [node.value]
      in_order(node.right, acc)
    end

    defp pre_order(nil, acc), do: acc
    defp pre_order(node, acc) do
      acc = acc ++ [node.value]
      acc = pre_order(node.left, acc)
      pre_order(node.right, acc)
    end

    defp post_order(nil, acc), do: acc
    defp post_order(node, acc) do
      acc = post_order(node.left, acc)
      acc = post_order(node.right, acc)
      acc ++ [node.value]
    end
  end

  # Hash Table Implementation
  defmodule HashTable do
    defstruct buckets: [], size: 0, capacity: 16, load_factor: 0.75

    def new(capacity \\ 16, load_factor \\ 0.75) do
      buckets = Enum.map(1..capacity, fn _ -> [] end)
      %HashTable{buckets: buckets, capacity: capacity, load_factor: load_factor}
    end

    def put(table, key, value) when is_map(table) do
      %HashTable{buckets: buckets, size: size, capacity: capacity, load_factor: load_factor} = table

      index = hash(key, capacity)
      bucket = Enum.at(buckets, index)

      # Check if key already exists
      new_bucket = case Enum.find(bucket, fn {k, _} -> k == key end) do
        nil -> bucket ++ [{key, value}]
        {existing_key, _} ->
          Enum.map(bucket, fn {k, v} ->
            if k == existing_key, do: {key, value}, else: {k, v}
          end)
      end

      new_buckets = List.replace_at(buckets, index, new_bucket)
      new_size = if Enum.find(bucket, fn {k, _} -> k == key end), do: size, else: size + 1

      # Check if we need to resize
      if new_size > capacity * load_factor do
        resized_table = resize(table)
        put(resized_table, key, value)
      else
        %{table | buckets: new_buckets, size: new_size}
      end
    end

    def get(table, key) when is_map(table) do
      %HashTable{buckets: buckets, capacity: capacity} = table
      index = hash(key, capacity)
      bucket = Enum.at(buckets, index)

      case Enum.find(bucket, fn {k, _} -> k == key end) do
        nil -> nil
        {_, value} -> value
      end
    end

    def has_key?(table, key) when is_map(table) do
      get(table, key) != nil
    end

    def remove(table, key) when is_map(table) do
      %HashTable{buckets: buckets, size: size, capacity: capacity} = table
      index = hash(key, capacity)
      bucket = Enum.at(buckets, index)

      new_bucket = Enum.reject(bucket, fn {k, _} -> k == key end)
      new_buckets = List.replace_at(buckets, index, new_bucket)

      %{table | buckets: new_buckets, size: size - 1}
    end

    def keys(table) when is_map(table) do
      %HashTable{buckets: buckets} = table
      Enum.flat_map(buckets, fn bucket ->
        Enum.map(bucket, fn {key, _} -> key end)
      end)
    end

    def values(table) when is_map(table) do
      %HashTable{buckets: buckets} = table
      Enum.flat_map(buckets, fn bucket ->
        Enum.map(bucket, fn {_, value} -> value end)
      end)
    end

    def entries(table) when is_map(table) do
      %HashTable{buckets: buckets} = table
      Enum.flat_map(buckets, fn bucket -> bucket end)
    end

    def size(table) when is_map(table), do: table.size
    def capacity(table) when is_map(table), do: table.capacity

    defp hash(key, capacity) do
      # Simple hash function
      key_hash = :erlang.phash2(key)
      abs(key_hash) |> rem(capacity)
    end

    defp resize(table) do
      %HashTable{buckets: buckets, capacity: capacity, load_factor: load_factor} = table
      new_capacity = capacity * 2
      new_table = new(new_capacity, load_factor)

      # Rehash all entries
      Enum.reduce(buckets, new_table, fn bucket, acc ->
        Enum.reduce(bucket, acc, fn {key, value}, table_acc ->
          put(table_acc, key, value)
        end)
      end)
    end
  end

  # Heap Implementation (Min Heap)
  defmodule MinHeap do
    defstruct heap: []

    def new do
      %MinHeap{heap: []}
    end

    def insert(heap, value) when is_map(heap) do
      %MinHeap{heap: heap_data} = heap
      new_heap = heap_data ++ [value]
      heapified = heapify_up(new_heap, length(new_heap) - 1)
      %{heap | heap: heapified}
    end

    def extract_min(heap) when is_map(heap) do
      %MinHeap{heap: heap_data} = heap
      case heap_data do
        [] -> {nil, heap}
        [min] -> {min, %{heap | heap: []}}
        [_ | tail] ->
          new_heap = [List.last(tail) | Enum.slice(tail, 0, length(tail) - 1)]
          heapified = heapify_down(new_heap, 0)
          {Enum.at(heapified, 0), %{heap | heap: Enum.slice(heapified, 1, length(heapified) - 1)}}
      end
    end

    def peek(heap) when is_map(heap) do
      %MinHeap{heap: heap_data} = heap
      if Enum.empty?(heap_data), do: nil, else: Enum.at(heap_data, 0)
    end

    def size(heap) when is_map(heap) do
      %MinHeap{heap: heap_data} = heap
      length(heap_data)
    end

    def is_empty?(heap) when is_map(heap) do
      %MinHeap{heap: heap_data} = heap
      Enum.empty?(heap_data)
    end

    def to_sorted_list(heap) when is_map(heap) do
      %MinHeap{heap: heap_data} = heap
      sorted = []

      # Extract all elements in sorted order
      {sorted_list, _} = Enum.reduce(heap_data, {[], heap}, fn _element, {sorted_acc, current_heap} ->
        case extract_min(current_heap) do
          {nil, _} -> {sorted_acc, current_heap}
          {min, new_heap} -> {sorted_acc ++ [min], new_heap}
        end
      end)

      sorted_list
    end

    defp heapify_up(heap, index) do
      parent_index = div(index - 1, 2)

      if index > 0 and Enum.at(heap, index) < Enum.at(heap, parent_index) do
        new_heap = swap(heap, index, parent_index)
        heapify_up(new_heap, parent_index)
      else
        heap
      end
    end

    defp heapify_down(heap, index) do
      size = length(heap)
      left_child = 2 * index + 1
      right_child = 2 * index + 2
      smallest = index

      if left_child < size and Enum.at(heap, left_child) < Enum.at(heap, smallest) do
        smallest = left_child
      end

      if right_child < size and Enum.at(heap, right_child) < Enum.at(heap, smallest) do
        smallest = right_child
      end

      if smallest != index do
        new_heap = swap(heap, index, smallest)
        heapify_down(new_heap, smallest)
      else
        heap
      end
    end

    defp swap(heap, i, j) do
      List.replace_at(heap, i, Enum.at(heap, j))
      |> List.replace_at(j, Enum.at(heap, i))
    end
  end

  # Trie (Prefix Tree) Implementation
  defmodule Trie do
    defstruct root: %{}

    def new do
      %Trie{root: %{}}
    end

    def insert(trie, word) when is_binary(word) do
      %Trie{root: root} = trie
      new_root = insert_word(root, String.graphemes(word))
      %{trie | root: new_root}
    end

    def search(trie, word) when is_binary(word) do
      %Trie{root: root} = trie
      search_word(root, String.graphemes(word))
    end

    def starts_with?(trie, prefix) when is_binary(prefix) do
      %Trie{root: root} = trie
      search_prefix(root, String.graphemes(prefix))
    end

    def get_all_words_with_prefix(trie, prefix) when is_binary(prefix) do
      %Trie{root: root} = trie
      graphemes = String.graphemes(prefix)
      case search_node_path(root, graphemes) do
        nil -> []
        node -> collect_words(node, prefix, [])
      end
    end

    def get_all_words(trie) do
      %Trie{root: root} = trie
      collect_words(root, "", [])
    end

    def count_words(trie) do
      %Trie{root: root} = trie
      count_nodes(root)
    end

    defp insert_word(node, []), do: Map.put(node, :word_end, true)
    defp insert_word(node, [char | rest]) do
      current_node = Map.get(node, char, %{})
      new_node = insert_word(current_node, rest)
      Map.put(node, char, new_node)
    end

    defp search_word(nil, _), do: false
    defp search_word(node, []), do: Map.get(node, :word_end, false)
    defp search_word(node, [char | rest]) do
      case Map.get(node, char) do
        nil -> false
        next_node -> search_word(next_node, rest)
      end
    end

    defp search_prefix(nil, _), do: false
    defp search_prefix(node, []), do: true
    defp search_prefix(node, [char | rest]) do
      case Map.get(node, char) do
        nil -> false
        next_node -> search_prefix(next_node, rest)
      end
    end

    defp search_node_path(nil, _), do: nil
    defp search_node_path(node, []), do: node
    defp search_node_path(node, [char | rest]) do
      case Map.get(node, char) do
        nil -> nil
        next_node -> search_node_path(next_node, rest)
      end
    end

    defp collect_words(node, prefix, acc) do
      acc = if Map.get(node, :word_end), do: [prefix | acc], else: acc

      Enum.reduce(node, acc, fn {key, child_node}, acc_words ->
        if key != :word_end do
          collect_words(child_node, prefix <> key, acc_words)
        else
          acc_words
        end
      end)
    end

    defp count_nodes(nil), do: 0
    defp count_nodes(node) do
      Enum.reduce(node, 0, fn {key, child_node}, count ->
        if key == :word_end do
          count + 1
        else
          count + count_nodes(child_node)
        end
      end)
    end
  end

  # Queue Implementation
  defmodule Queue do
    defstruct front: [], back: []

    def new do
      %Queue{front: [], back: []}
    end

    def enqueue(queue, element) when is_map(queue) do
      %Queue{front: front, back: back} = queue
      %{queue | back: [element | back]}
    end

    def dequeue(queue) when is_map(queue) do
      %Queue{front: front, back: back} = queue
      case front do
        [] ->
          case Enum.reverse(back) do
            [] -> {nil, queue}
            [element | new_front] ->
              new_queue = %{queue | front: new_front, back: []}
              {element, new_queue}
          end
        [element | new_front] ->
          new_queue = %{queue | front: new_front}
          {element, new_queue}
      end
    end

    def peek(queue) when is_map(queue) do
      %Queue{front: front, back: back} = queue
      case front do
        [] -> List.last(back)
        [element | _] -> element
      end
    end

    def is_empty?(queue) when is_map(queue) do
      %Queue{front: front, back: back} = queue
      Enum.empty?(front) and Enum.empty?(back)
    end

    def size(queue) when is_map(queue) do
      %Queue{front: front, back: back} = queue
      length(front) + length(back)
    end

    def to_list(queue) when is_map(queue) do
      %Queue{front: front, back: back} = queue
      front ++ Enum.reverse(back)
    end
  end

  # Stack Implementation
  defmodule Stack do
    defstruct items: []

    def new do
      %Stack{items: []}
    end

    def push(stack, element) when is_map(stack) do
      %Stack{items: items} = stack
      %{stack | items: [element | items]}
    end

    def pop(stack) when is_map(stack) do
      %Stack{items: items} = stack
      case items do
        [] -> {nil, stack}
        [top | rest] -> {top, %{stack | items: rest}}
      end
    end

    def peek(stack) when is_map(stack) do
      %Stack{items: items} = stack
      if Enum.empty?(items), do: nil, else: List.first(items)
    end

    def is_empty?(stack) when is_map(stack) do
      %Stack{items: items} = stack
      Enum.empty?(items)
    end

    def size(stack) when is_map(stack) do
      %Stack{items: items} = stack
      length(items)
    end

    def to_list(stack) when is_map(stack) do
      %Stack{items: items} = stack
      items
    end
  end

  # Graph Implementation
  defmodule Graph do
    defstruct vertices: %{}, edges: %{}

    def new do
      %Graph{vertices: %{}, edges: %{}}
    end

    def add_vertex(graph, vertex) when is_map(graph) do
      %Graph{vertices: vertices, edges: edges} = graph
      new_vertices = Map.put(vertices, vertex, %{})
      %{graph | vertices: new_vertices}
    end

    def add_edge(graph, from, to, weight \\ 1) when is_map(graph) do
      %Graph{vertices: vertices, edges: edges} = graph

      # Ensure vertices exist
      new_vertices = vertices
                    |> Map.put_new(from, %{})
                    |> Map.put_new(to, %{})

      # Add edge
      from_edges = Map.get(edges, from, %{})
      new_from_edges = Map.put(from_edges, to, weight)
      new_edges = Map.put(edges, from, new_from_edges)

      %{graph | vertices: new_vertices, edges: new_edges}
    end

    def get_neighbors(graph, vertex) when is_map(graph) do
      %Graph{edges: edges} = graph
      case Map.get(edges, vertex) do
        nil -> []
        to_edges -> Map.keys(to_edges)
      end
    end

    def get_all_vertices(graph) when is_map(graph) do
      %Graph{vertices: vertices} = graph
      Map.keys(vertices)
    end

    def breadth_first_search(graph, start, target) when is_map(graph) do
      bfs_recursive([{start, [start]}], target, MapSet.new([start]), graph)
    end

    def shortest_path(graph, source, target) when is_map(graph) do
      case breadth_first_search(graph, source, target) do
        nil -> nil
        path -> Enum.reverse(path)
      end
    end

    defp bfs_recursive([], _target, _visited, _graph), do: nil
    defp bfs_recursive([{current, path} | queue], target, visited, graph) do
      if current == target do
        path
      else
        neighbors = get_neighbors(graph, current)
        unvisited_neighbors = Enum.filter(neighbors, fn n -> not MapSet.member?(visited, n) end)

        new_queue = Enum.map(unvisited_neighbors, fn neighbor ->
          {neighbor, path ++ [neighbor]}
        end) ++ queue

        new_visited = Enum.reduce(unvisited_neighbors, visited, &MapSet.put(&2, &1))
        bfs_recursive(new_queue, target, new_visited, graph)
      end
    end
  end

  # Set Implementation
  defmodule CustomSet do
    defstruct items: %{}

    def new do
      %CustomSet{items: %{}}
    end

    def new(elements) when is_list(elements) do
      items = Enum.into(elements, %{}, fn x -> {x, true} end)
      %CustomSet{items: items}
    end

    def add(set, element) when is_map(set) do
      %CustomSet{items: items} = set
      new_items = Map.put(items, element, true)
      %{set | items: new_items}
    end

    def remove(set, element) when is_map(set) do
      %CustomSet{items: items} = set
      new_items = Map.delete(items, element)
      %{set | items: new_items}
    end

    def member?(set, element) when is_map(set) do
      %CustomSet{items: items} = set
      Map.has_key?(items, element)
    end

    def size(set) when is_map(set) do
      %CustomSet{items: items} = set
      map_size(items)
    end

    def is_empty?(set) when is_map(set) do
      %CustomSet{items: items} = set
      map_size(items) == 0
    end

    def to_list(set) when is_map(set) do
      %CustomSet{items: items} = table
      Map.keys(items)
    end

    def union(set1, set2) when is_map(set1) and is_map(set2) do
      %CustomSet{items: items1} = set1
      %CustomSet{items: items2} = set2
      new_items = Map.merge(items1, items2)
      %{set1 | items: new_items}
    end

    def intersection(set1, set2) when is_map(set1) and is_map(set2) do
      %CustomSet{items: items1} = set1
      %CustomSet{items: items2} = set2
      common_keys = Map.keys(items1) |> Enum.filter(&Map.has_key?(items2, &1))
      new_items = Enum.into(common_keys, %{}, fn x -> {x, true} end)
      %{set1 | items: new_items}
    end
  end
end

# Demo usage
defmodule ElixirDataStructuresDemo do
  def run do
    IO.puts("=== Elixir Data Structures Demo ===")

    # Singly Linked List Demo
    IO.puts("\n--- Singly Linked List ---")
    list = ElixirDataStructures.SinglyLinkedList.new()
    list = ElixirDataStructures.SinglyLinkedList.append(list, "first")
    list = ElixirDataStructures.SinglyLinkedList.append(list, "second")
    list = ElixirDataStructures.SinglyLinkedList.prepend(list, "zero")

    IO.puts("List contents: #{inspect(ElixirDataStructures.SinglyLinkedList.to_list(list))}")
    IO.puts("Element at index 1: #{inspect(ElixirDataStructures.SinglyLinkedList.get(list, 1))}")

    # Binary Search Tree Demo
    IO.puts("\n--- Binary Search Tree ---")
    bst = ElixirDataStructures.BinarySearchTree.new()
    bst = ElixirDataStructures.BinarySearchTree.insert(bst, 50)
    bst = ElixirDataStructures.BinarySearchTree.insert(bst, 30)
    bst = ElixirDataStructures.BinarySearchTree.insert(bst, 70)
    bst = ElixirDataStructures.BinarySearchTree.insert(bst, 20)
    bst = ElixirDataStructures.BinarySearchTree.insert(bst, 40)

    IO.puts("In-order traversal: #{inspect(ElixirDataStructures.BinarySearchTree.in_order_traversal(bst))}")
    IO.puts("Search for 40: #{ElixirDataStructures.BinarySearchTree.search(bst, 40)}")
    IO.puts("Tree height: #{ElixirDataStructures.BinarySearchTree.height(bst)}")
    IO.puts("Is balanced: #{ElixirDataStructures.BinarySearchTree.is_balanced?(bst)}")

    # Hash Table Demo
    IO.puts("\n--- Hash Table ---")
    ht = ElixirDataStructures.HashTable.new()
    ht = ElixirDataStructures.HashTable.put(ht, "name", "Alice")
    ht = ElixirDataStructures.HashTable.put(ht, "age", 30)
    ht = ElixirDataStructures.HashTable.put(ht, "city", "New York")

    IO.puts("Hash table entries: #{inspect(ElixirDataStructures.HashTable.entries(ht))}")
    IO.puts("Get 'name': #{inspect(ElixirDataStructures.HashTable.get(ht, "name"))}")
    IO.puts("Has key 'age': #{ElixirDataStructures.HashTable.has_key?(ht, "age")}")

    # Min Heap Demo
    IO.puts("\n--- Min Heap ---")
    heap = ElixirDataStructures.MinHeap.new()
    heap = ElixirDataStructures.MinHeap.insert(heap, 15)
    heap = ElixirDataStructures.MinHeap.insert(heap, 5)
    heap = ElixirDataStructures.MinHeap.insert(heap, 25)
    heap = ElixirDataStructures.MinHeap.insert(heap, 10)

    IO.puts("Heap size: #{ElixirDataStructures.MinHeap.size(heap)}")
    IO.puts("Peek minimum: #{inspect(ElixirDataStructures.MinHeap.peek(heap))}")

    {min1, heap} = ElixirDataStructures.MinHeap.extract_min(heap)
    {min2, heap} = ElixirDataStructures.MinHeap.extract_min(heap)
    IO.puts("Extracted min1: #{min1}")
    IO.puts("Extracted min2: #{min2}")
    IO.puts("Remaining size: #{ElixirDataStructures.MinHeap.size(heap)}")

    # Trie Demo
    IO.puts("\n--- Trie ---")
    trie = ElixirDataStructures.Trie.new()
    trie = ElixirDataStructures.Trie.insert(trie, "apple")
    trie = ElixirDataStructures.Trie.insert(trie, "app")
    trie = ElixirDataStructures.Trie.insert(trie, "application")
    trie = ElixirDataStructures.Trie.insert(trie, "banana")

    IO.puts("Search 'apple': #{ElixirDataStructures.Trie.search(trie, "apple")}")
    IO.puts("Search 'ban': #{ElixirDataStructures.Trie.search(trie, "ban")}")
    IO.puts("Starts with 'app': #{ElixirDataStructures.Trie.starts_with?(trie, "app")}")
    IO.puts("Words starting with 'app': #{inspect(ElixirDataStructures.Trie.get_all_words_with_prefix(trie, "app"))}")

    # Queue Demo
    IO.puts("\n--- Queue ---")
    queue = ElixirDataStructures.Queue.new()
    queue = ElixirDataStructures.Queue.enqueue(queue, "first")
    queue = ElixirDataStructures.Queue.enqueue(queue, "second")
    queue = ElixirDataStructures.Queue.enqueue(queue, "third")

    IO.puts("Queue size: #{ElixirDataStructures.Queue.size(queue)}")
    IO.puts("Queue peek: #{inspect(ElixirDataStructures.Queue.peek(queue))}")

    {item1, queue} = ElixirDataStructures.Queue.dequeue(queue)
    {item2, queue} = ElixirDataStructures.Queue.dequeue(queue)
    IO.puts("Dequeued: #{item1}, #{item2}")
    IO.puts("Remaining size: #{ElixirDataStructures.Queue.size(queue)}")

    # Stack Demo
    IO.puts("\n--- Stack ---")
    stack = ElixirDataStructures.Stack.new()
    stack = ElixirDataStructures.Stack.push(stack, "first")
    stack = ElixirDataStructures.Stack.push(stack, "second")
    stack = ElixirDataStructures.Stack.push(stack, "third")

    IO.puts("Stack size: #{ElixirDataStructures.Stack.size(stack)}")
    IO.puts("Stack peek: #{inspect(ElixirDataStructures.Stack.peek(stack))}")

    {item1, stack} = ElixirDataStructures.Stack.pop(stack)
    {item2, stack} = ElixirDataStructures.Stack.pop(stack)
    IO.puts("Popped: #{item1}, #{item2}")
    IO.puts("Remaining size: #{ElixirDataStructures.Stack.size(stack)}")

    # Graph Demo
    IO.puts("\n--- Graph ---")
    graph = ElixirDataStructures.Graph.new()
    graph = ElixirDataStructures.Graph.add_vertex(graph, "A")
    graph = ElixirDataStructures.Graph.add_vertex(graph, "B")
    graph = ElixirDataStructures.Graph.add_vertex(graph, "C")
    graph = ElixirDataStructures.Graph.add_edge(graph, "A", "B")
    graph = ElixirDataStructures.Graph.add_edge(graph, "B", "C")
    graph = ElixirDataStructures.Graph.add_edge(graph, "A", "C")

    IO.puts("Graph vertices: #{inspect(ElixirDataStructures.Graph.get_all_vertices(graph))}")
    IO.puts("Path from A to C: #{inspect(ElixirDataStructures.Graph.shortest_path(graph, "A", "C"))}")

    # Set Demo
    IO.puts("\n--- Set ---")
    set1 = ElixirDataStructures.CustomSet.new([1, 2, 3, 4])
    set2 = ElixirDataStructures.CustomSet.new([3, 4, 5, 6])
    set1 = ElixirDataStructures.CustomSet.add(set1, 5)

    IO.puts("Set1: #{inspect(ElixirDataStructures.CustomSet.to_list(set1))}")
    IO.puts("Set2: #{inspect(ElixirDataStructures.CustomSet.to_list(set2))}")
    IO.puts("Set1 has 3: #{ElixirDataStructures.CustomSet.member?(set1, 3)}")
    IO.puts("Union: #{inspect(ElixirDataStructures.CustomSet.to_list(ElixirDataStructures.CustomSet.union(set1, set2)))}")
    IO.puts("Intersection: #{inspect(ElixirDataStructures.CustomSet.to_list(ElixirDataStructures.CustomSet.intersection(set1, set2)))}")

    IO.puts("\n=== Data Structures Demo Complete ===")
  end
end

# Run the demo
if __ENV__.module == ElixirDataStructuresDemo do
  ElixirDataStructuresDemo.run()
end
