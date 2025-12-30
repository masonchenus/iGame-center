# Elixir Basics - Fundamental Concepts and Syntax
defmodule ElixirBasics do
  # Pattern Matching
  defmodule PatternMatching do
    def basic_match do
      # Variable binding
      x = 42
      IO.puts("x = #{x}")

      # Pattern matching with tuples
      {a, b, c} = {1, 2, 3}
      IO.puts("a = #{a}, b = #{b}, c = #{c}")

      # Pattern matching with lists
      [head | tail] = [1, 2, 3, 4, 5]
      IO.puts("head = #{head}, tail = #{inspect(tail)}")

      # Pattern matching with maps
      %{name: name, age: age} = %{name: "Alice", age: 30}
      IO.puts("name = #{name}, age = #{age}")
    end

    def pin_operator do
      x = 10
      # Using pin operator to use existing value
      {^x, y} = {10, 20}
      IO.puts("x = #{x}, y = #{y}")
    end

    def function_clause_matching do
      greet("Alice") # -> "Hello, Alice!"
      greet("Bob", "Hi") # -> "Hi, Bob!"
      greet() # -> "Hello, stranger!"
    end

    defp greet(name), do: "Hello, #{name}!"
    defp greet(name, greeting), do: "#{greeting}, #{name}!"
    defp greet, do: "Hello, stranger!"
  end

  # Control Flow
  defmodule ControlFlow do
    def if_unless do
      age = 18

      if age >= 18 do
        IO.puts("Adult")
      else
        IO.puts("Minor")
      end

      unless age >= 18 do
        IO.puts("Minor")
      else
        IO.puts("Adult")
      end
    end

    def case_statement do
      color = "blue"

      case color do
        "red" -> IO.puts("Stop")
        "yellow" -> IO.puts("Caution")
        "green" -> IO.puts("Go")
        _ -> IO.puts("Unknown color")
      end
    end

    def cond_statement do
      temperature = 25

      cond do
        temperature < 0 -> IO.puts("Freezing")
        temperature < 10 -> IO.puts("Cold")
        temperature < 20 -> IO.puts("Cool")
        temperature < 30 -> IO.puts("Warm")
        true -> IO.puts("Hot")
      end
    end
  end

  # Data Types
  defmodule DataTypes do
    def atom_example do
      # Atoms are constants with their name as their value
      status = :ok
      error = :error
      atom_name = :some_atom

      IO.puts("Status: #{status}")
      IO.puts("Atom: #{atom_name}")
    end

    def list_example do
      # Lists are linked lists
      numbers = [1, 2, 3, 4, 5]
      mixed = [1, "hello", :atom, true]

      # List operations
      IO.puts("Head: #{hd(numbers)}")
      IO.puts("Tail: #{inspect(tl(numbers))}")

      # List concatenation
      combined = numbers ++ [6, 7, 8]
      IO.puts("Combined: #{inspect(combined)}")
    end

    def tuple_example do
      # Tuples are fixed-size containers
      person = {"Alice", 30, "Engineer"}
      point = {x: 10, y: 20}

      # Accessing tuple elements
      {name, age, _} = person
      IO.puts("Name: #{name}, Age: #{age}")

      # Keyword lists
      options = [color: "blue", size: "large"]
      IO.puts("Options: #{inspect(options)}")
    end

    def map_example do
      # Maps are key-value stores
      user = %{
        name: "Alice",
        age: 30,
        email: "alice@example.com"
      }

      # Accessing and updating maps
      name = user[:name]
      updated_user = Map.put(user, :age, 31)
      IO.puts("Updated age: #{updated_user[:age]}")
    end
  end

  # Functions
  defmodule Functions do
    def simple_function(x, y) do
      x + y
    end

    def function_with_guard(x) when is_integer(x) and x > 0 do
      "Positive integer"
    end

    def function_with_guard(x) when is_integer(x) and x < 0 do
      "Negative integer"
    end

    def function_with_guard(_), do: "Not an integer"

    def default_parameters(name, greeting \\ "Hello") do
      "#{greeting}, #{name}!"
    end

    def anonymous_functions do
      # Anonymous functions
      add = fn a, b -> a + b end
      double = &(&1 * 2)

      result = add.(5, 3)
      doubled = double.(10)

      IO.puts("Add result: #{result}")
      IO.puts("Double result: #{doubled}")
    end

    def higher_order_functions do
      numbers = [1, 2, 3, 4, 5]

      # Map
      doubled = Enum.map(numbers, &(&1 * 2))
      IO.puts("Doubled: #{inspect(doubled)}")

      # Filter
      evens = Enum.filter(numbers, &(rem(&1, 2) == 0))
      IO.puts("Evens: #{inspect(evens)}")

      # Reduce
      sum = Enum.reduce(numbers, 0, &+/2)
      IO.puts("Sum: #{sum}")
    end
  end

  # Modules and Structs
  defmodule Person do
    defstruct name: "", age: 0, email: ""

    def new(name, age, email) do
      %Person{name: name, age: age, email: email}
    end

    def adult?(%Person{age: age}) do
      age >= 18
    end

    def greet(%Person{name: name}) do
      "Hello, #{name}!"
    end
  end

  defmodule Modules do
    def module_examples do
      # Using the Person struct
      person = Person.new("Alice", 25, "alice@example.com")
      IO.puts(Person.greet(person))
      IO.puts("Adult? #{Person.adult?(person)}")

      # Module attributes
      IO.puts("Module attribute: #{@default_greeting}")

      # Private functions
      result = private_function(5)
      IO.puts("Private function result: #{result}")
    end

    @default_greeting "Welcome to Elixir!"

    defp private_function(x), do: x * x
  end

  # Recursion
  defmodule Recursion do
    def factorial(0), do: 1
    def factorial(n) when n > 0, do: n * factorial(n - 1)

    def fibonacci(0), do: 0
    def fibonacci(1), do: 1
    def fibonacci(n) when n > 1, do: fibonacci(n - 1) + fibonacci(n - 2)

    def sum_list([]), do: 0
    def sum_list([head | tail]), do: head + sum_list(tail)

    def max_list([x]), do: x
    def max_list([head | tail]) do
      max_tail = max_list(tail)
      if head > max_tail, do: head, else: max_tail
    end
  end

  # Comprehensions
  defmodule Comprehensions do
    def list_comprehension do
      # Basic list comprehension
      squares = for x <- 1..10, do: x * x
      IO.puts("Squares: #{inspect(squares)}")

      # With filters
      even_squares = for x <- 1..10, rem(x, 2) == 0, do: x * x
      IO.puts("Even squares: #{inspect(even_squares)}")

      # Multiple generators
      combinations = for x <- 1..3, y <- 1..3, do: {x, y}
      IO.puts("Combinations: #{inspect(combinations)}")
    end

    def map_comprehension do
      # Map comprehension
      users = [
        %{name: "Alice", age: 25},
        %{name: "Bob", age: 30},
        %{name: "Charlie", age: 35}
      ]

      names = for %{name: name} <- users, do: name
      IO.puts("Names: #{inspect(names)}")
    end
  end

  # Error Handling
  defmodule ErrorHandling do
    def safe_divide(a, b) do
      try do
        if b == 0 do
          raise "Division by zero"
        end
        a / b
      rescue
        e in ArithmeticError -> "Error: #{e.message}"
        e in RuntimeError -> "Error: #{e.message}"
      end
    end

    def pattern_match_error do
      try do
        {x, y} = :not_a_tuple
      rescue
        MatchError -> "Pattern match failed"
      end
    end

    def after_example do
      try do
        File.open!("nonexistent.txt")
      rescue
        File.Error -> "Could not open file"
      after
        IO.puts("This always runs")
      end
    end
  end

  # Protocols
  defmodule Greetable do
    defprotocol greet do
      def greet(person)
    end

    defimpl greet, for: Person do
      def greet(%Person{name: name}), do: "Hello, #{name}!"
    end

    defimpl greet, for: String do
      def greet(name), do: "Hello, #{name}!"
    end

    defimpl greet, for: Atom do
      def greet(:admin), do: "Hello, Administrator!"
      def greet(name), do: "Hello, #{name}!"
    end
  end
end

# Demo usage
defmodule ElixirBasicsDemo do
  def run do
    IO.puts("=== Elixir Basics Demo ===")

    # Pattern Matching
    IO.puts("\n--- Pattern Matching ---")
    ElixirBasics.PatternMatching.basic_match()

    # Control Flow
    IO.puts("\n--- Control Flow ---")
    ElixirBasics.ControlFlow.if_unless()

    # Data Types
    IO.puts("\n--- Data Types ---")
    ElixirBasics.DataTypes.atom_example()

    # Functions
    IO.puts("\n--- Functions ---")
    ElixirBasics.Functions.higher_order_functions()

    # Modules
    IO.puts("\n--- Modules ---")
    ElixirBasics.Modules.module_examples()

    # Recursion
    IO.puts("\n--- Recursion ---")
    IO.puts("Factorial(5) = #{ElixirBasics.Recursion.factorial(5)}")
    IO.puts("Fibonacci(10) = #{ElixirBasics.Recursion.fibonacci(10)}")

    # Comprehensions
    IO.puts("\n--- Comprehensions ---")
    ElixirBasics.Comprehensions.list_comprehension()

    # Error Handling
    IO.puts("\n--- Error Handling ---")
    IO.puts("Safe divide: #{ElixirBasics.ErrorHandling.safe_divide(10, 2)}")

    # Protocols
    IO.puts("\n--- Protocols ---")
    person = ElixirBasics.Person.new("Alice", 25, "alice@example.com")
    IO.puts(ElixirBasics.Greetable.greet(person))
    IO.puts(ElixirBasics.Greetable.greet("Bob"))
    IO.puts(ElixirBasics.Greetable.greet(:admin))

    IO.puts("\n=== Demo Complete ===")
  end
end

# Run the demo
ElixirBasicsDemo.run()
