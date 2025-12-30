# Elixir Concurrency - Process-based Concurrency and Message Passing
defmodule ElixirConcurrency do
  # Basic Process Management
  defmodule BasicProcesses do
    def spawn_basic_process do
      # Spawn a simple process
      pid = spawn(fn ->
        :timer.sleep(1000)
        IO.puts("Process finished after 1 second")
      end)

      IO.puts("Spawned process: #{inspect(pid)}")
      pid
    end

    def spawn_with_receive do
      pid = spawn(fn ->
        receive do
          {:hello, name} -> IO.puts("Hello, #{name}!")
          {:goodbye, name} -> IO.puts("Goodbye, #{name}!")
          _ -> IO.puts("Unknown message")
        end
      end)

      # Send messages to the process
      send(pid, {:hello, "Alice"})
      send(pid, {:goodbye, "Bob"})

      pid
    end

    def process_with_state do
      pid = spawn(fn -> state_loop(0) end)

      send(pid, {:increment, 5})
      send(pid, {:decrement, 2})
      send(pid, {:get, self()})

      receive do
        {:result, value} -> IO.puts("Current value: #{value}")
      end

      pid
    end

    defp state_loop(current_state) do
      receive do
        {:increment, amount} ->
          new_state = current_state + amount
          IO.puts("Incremented: #{current_state} + #{amount} = #{new_state}")
          state_loop(new_state)

        {:decrement, amount} ->
          new_state = current_state - amount
          IO.puts("Decremented: #{current_state} - #{amount} = #{new_state}")
          state_loop(new_state)

        {:get, caller} ->
          send(caller, {:result, current_state})
          state_loop(current_state)

        :stop ->
          IO.puts("Process stopping with final state: #{current_state}")
      end
    end
  end

  # GenServer Implementation
  defmodule CounterGenServer do
    use GenServer

    # Client API
    def start_link(initial_value \\ 0) do
      GenServer.start_link(__MODULE__, initial_value)
    end

    def increment(pid), do: GenServer.cast(pid, :increment)
    def decrement(pid), do: GenServer.cast(pid, :decrement)
    def get(pid), do: GenServer.call(pid, :get)
    def set(pid, value), do: GenServer.cast(pid, {:set, value})

    # Server Callbacks
    def init(initial_value) do
      {:ok, initial_value}
    end

    def handle_cast(:increment, current_value) do
      {:noreply, current_value + 1}
    end

    def handle_cast(:decrement, current_value) do
      {:noreply, current_value - 1}
    end

    def handle_cast({:set, value}, _current_value) do
      {:noreply, value}
    end

    def handle_call(:get, _from, current_value) do
      {:reply, current_value, current_value}
    end
  end

  # Agent for simpler state management
  defmodule CounterAgent do
    def start_link(initial_value \\ 0) do
      Agent.start_link(fn -> initial_value end)
    end

    def increment(pid), do: Agent.update(pid, &(&1 + 1))
    def decrement(pid), do: Agent.update(pid, &(&1 - 1))
    def get(pid), do: Agent.get(pid, &(&1))
    def set(pid, value), do: Agent.update(pid, fn _ -> value end)
  end

  # Task and Task.Supervisor
  defmodule TaskExamples do
    def basic_task do
      # Run a task asynchronously
      task = Task.async(fn ->
        :timer.sleep(1000)
        "Task completed after 1 second"
      end)

      # Do other work
      IO.puts("Main process continuing...")

      # Wait for task to complete
      result = Task.await(task)
      IO.puts("Task result: #{result}")
    end

    def multiple_tasks do
      # Start multiple tasks
      tasks = [
        Task.async(fn -> slow_calculation(1000) end),
        Task.async(fn -> slow_calculation(2000) end),
        Task.async(fn -> slow_calculation(3000) end)
      ]

      # Wait for all tasks
      results = Task.await_many(tasks)
      IO.puts("All results: #{inspect(results)}")
    end

    def task_supervisor_example do
      # Start a task supervisor
      {:ok, sup} = Task.Supervisor.start_link()

      # Start supervised tasks
      tasks = [
        Task.Supervisor.async(sup, fn -> slow_calculation(1000) end),
        Task.Supervisor.async(sup, fn -> slow_calculation(2000) end),
        Task.Supervisor.async(sup, fn -> slow_calculation(3000) end)
      ]

      # Wait for all results
      results = Task.await_many(tasks)
      IO.puts("Supervised tasks results: #{inspect(results)}")
    end

    def stream_processing do
      # Process a stream with tasks
      stream = 1..1000
      |> Stream.map(&(&1 * 2))
      |> Stream.filter(&(&1 > 100))

      # Process in parallel with tasks
      results = stream
      |> Task.async_stream(&expensive_operation/1)
      |> Enum.to_list()

      IO.puts("Processed #{length(results)} items")
    end

    defp slow_calculation(delay) do
      :timer.sleep(delay)
      "Calculated after #{delay}ms"
    end

    defp expensive_operation(x) do
      :timer.sleep(10)
      x * x
    end
  end

  # Channel-based communication
  defmodule ChannelExamples do
    def basic_channel do
      # Create a channel
      channel = spawn(fn -> channel_loop([]) end)

      # Subscribe to messages
      send(channel, {:subscribe, self()})

      # Send messages
      send(channel, {:broadcast, "Hello from main process!"})
      send(channel, {:broadcast, "Another message"})

      # Receive broadcast messages
      receive do
        {:broadcast, message} -> IO.puts("Received: #{message}")
      after
        1000 -> IO.puts("No more messages")
      end

      # Clean up
      send(channel, :stop)
    end

    def pub_sub_channel do
      # Publisher process
      publisher = spawn(fn -> publisher_loop() end)

      # Subscriber processes
      subscriber1 = spawn(fn -> subscriber_loop("Subscriber 1") end)
      subscriber2 = spawn(fn -> subscriber_loop("Subscriber 2") end)

      # Subscribe subscribers
      send(publisher, {:subscribe, subscriber1})
      send(publisher, {:subscribe, subscriber2})

      # Publish messages
      send(publisher, {:publish, "Message 1"})
      send(publisher, {:publish, "Message 2"})

      # Let subscribers receive messages
      :timer.sleep(100)

      # Unsubscribe
      send(publisher, {:unsubscribe, subscriber1})

      send(publisher, {:publish, "Message 3 (only subscriber 2 receives this)"})

      :timer.sleep(100)

      # Clean up
      send(publisher, :stop)
      send(subscriber1, :stop)
      send(subscriber2, :stop)
    end

    defp channel_loop(subscribers) do
      receive do
        {:subscribe, pid} ->
          new_subscribers = [pid | subscribers]
          channel_loop(new_subscribers)

        {:unsubscribe, pid} ->
          new_subscribers = List.delete(subscribers, pid)
          channel_loop(new_subscribers)

        {:broadcast, message} ->
          Enum.each(subscribers, fn pid ->
            send(pid, {:broadcast, message})
          end)
          channel_loop(subscribers)

        :stop ->
          IO.puts("Channel stopping")
      end
    end

    defp publisher_loop do
      receive do
        {:subscribe, pid} ->
          # Register subscriber (simplified)
          publisher_loop()

        {:publish, message} ->
          # Broadcast to all subscribers (simplified)
          publisher_loop()

        :stop ->
          IO.puts("Publisher stopping")
      end
    end

    defp subscriber_loop(name) do
      receive do
        {:broadcast, message} ->
          IO.puts("#{name} received: #{message}")
          subscriber_loop(name)

        :stop ->
          IO.puts("#{name} stopping")
      end
    end
  end

  # Flow for parallel data processing
  defmodule FlowExamples do
    def basic_flow do
      # Create a flow
      flow = 1..1_000_000
      |> Flow.from_enumerable()
      |> Flow.map(&(&1 * &1))
      |> Flow.filter(&(rem(&1, 2) == 0))
      |> Flow.reduce(&(&1 + &2))

      # Execute the flow
      result = Flow.run(flow)
      IO.puts("Flow result: #{result}")
    end

    def parallel_word_count do
      text = """
      Elixir is a dynamic, functional language designed for building scalable and maintainable applications.
      Elixir runs on the Erlang Virtual Machine (BEAM), known for powering low-latency, distributed and fault-tolerant systems.
      Elixir is successfully used in web development, the embedded software domain, and the multimedia domain at scale.
      """

      words = String.split(text, " ", trim: true)

      flow = Flow.from_enumerable(words)
      |> Flow.map(&String.downcase/1)
      |> Flow.reduce(%{}, fn word, acc ->
        Map.update(acc, word, 1, &(&1 + 1))
      end)
      |> Flow.finish(&(&1))

      result = Flow.run(flow)
      IO.puts("Word count: #{inspect(result)}")
    end

    def stage_flow do
      # Multi-stage flow with different processing steps
      result = 1..100
      |> Flow.from_enumerable()
      |> Flow.map(&(&1 * 2))                    # Stage 1: Double numbers
      |> Flow.filter(&(rem(&1, 3) == 0))        # Stage 2: Keep multiples of 3
      |> Flow.partition()                       # Stage 3: Partition for parallel processing
      |> Flow.reduce(&(&1 + &2))                # Stage 4: Sum in each partition
      |> Flow.finish(&(&1))

      IO.puts("Stage flow result: #{result}")
    end
  end

  # Concurrency patterns
  defmodule ConcurrencyPatterns do
    # Producer-Consumer pattern
    def producer_consumer do
      producer = spawn(fn -> producer_loop(1..100) end)
      consumer = spawn(fn -> consumer_loop() end)

      send(producer, {:subscribe, consumer})
    end

    # Worker pool pattern
    def worker_pool do
      # Start supervisor for workers
      {:ok, sup} = DynamicSupervisor.start_link(strategy: :one_for_one)

      # Start worker pool
      workers = for i <- 1..5 do
        DynamicSupervisor.start_child(sup, {WorkerPool.Worker, i})
      end

      # Send work to pool
      for i <- 1..10 do
        send(sup, {:work, i, self()})
      end

      # Collect results
      results = for _ <- 1..10, do: receive_work_result()
      IO.puts("Pool results: #{inspect(results)}")
    end

    # Message passing with timeouts
    def timeout_pattern do
      pid = spawn(fn -> slow_operation() end)

      # Try to get result with timeout
      result = receive do
        {:result, value} -> value
      after
        5000 -> :timeout
      end

      case result do
        :timeout -> IO.puts("Operation timed out")
        value -> IO.puts("Got result: #{value}")
      end
    end

    # Circuit breaker pattern
    def circuit_breaker do
      breaker = spawn(fn -> circuit_breaker_loop(:closed, 0) end)

      # Make requests
      for i <- 1..10 do
        send(breaker, {:request, i, self()})
        receive do
          {:response, request_id, result} ->
            IO.puts("Request #{request_id}: #{result}")
        end
      end
    end

    defp producer_loop(range) do
      receive do
        {:subscribe, consumer} ->
          Enum.each(range, fn item ->
            send(consumer, {:item, item})
            :timer.sleep(100)
          end)
          send(consumer, :done)
      end
    end

    defp consumer_loop do
      receive do
        {:item, item} ->
          IO.puts("Processing item: #{item}")
          consumer_loop()

        :done ->
          IO.puts("Consumer done")
      end
    end

    defp slow_operation do
      :timer.sleep(2000)
      send(self(), {:result, "Operation completed"})
    end

    defp receive_work_result do
      receive do
        {:work_result, result} -> result
      end
    end

    defp circuit_breaker_loop(state, failure_count) do
      receive do
        {:request, request_id, from} ->
          case state do
            :closed ->
              if failure_count < 3 do
                send(from, {:response, request_id, "Success"})
                circuit_breaker_loop(:closed, failure_count)
              else
                send(from, {:response, request_id, "Circuit breaker open"})
                circuit_breaker_loop(:open, failure_count)
              end

            :open ->
              send(from, {:response, request_id, "Circuit breaker open"})
              circuit_breaker_loop(:half_open, failure_count)

            :half_open ->
              send(from, {:response, request_id, "Success"})
              circuit_breaker_loop(:closed, 0)
          end
      end
    end
  end

  # Worker pool module
  defmodule WorkerPool do
    defmodule Worker do
      use GenServer

      def start_link(worker_id) do
        GenServer.start_link(__MODULE__, worker_id)
      end

      def init(worker_id) do
        {:ok, worker_id}
      end

      def handle_cast({:work, work_item, requester}, worker_id) do
        result = process_work(work_item)
        send(requester, {:work_result, result})
        {:noreply, worker_id}
      end

      defp process_work(item) do
        :timer.sleep(100)  # Simulate work
        "Processed item #{item} by worker #{inspect(self())}"
      end
    end
  end
end

# Demo usage
defmodule ElixirConcurrencyDemo do
  def run do
    IO.puts("=== Elixir Concurrency Demo ===")

    # Basic processes
    IO.puts("\n--- Basic Processes ---")
    ElixirConcurrency.BasicProcesses.spawn_basic_process()

    # Process with receive
    IO.puts("\n--- Process with Receive ---")
    ElixirConcurrency.BasicProcesses.spawn_with_receive()

    # GenServer
    IO.puts("\n--- GenServer Counter ---")
    {:ok, counter} = ElixirConcurrency.CounterGenServer.start_link(10)
    ElixirConcurrency.CounterGenServer.increment(counter)
    value = ElixirConcurrency.CounterGenServer.get(counter)
    IO.puts("Counter value: #{value}")

    # Agent
    IO.puts("\n--- Agent Counter ---")
    {:ok, agent_counter} = ElixirConcurrency.CounterAgent.start_link(5)
    ElixirConcurrency.CounterAgent.increment(agent_counter)
    agent_value = ElixirConcurrency.CounterAgent.get(agent_counter)
    IO.puts("Agent counter value: #{agent_value}")

    # Tasks
    IO.puts("\n--- Task Examples ---")
    ElixirConcurrency.TaskExamples.basic_task()

    # Channels
    IO.puts("\n--- Channel Examples ---")
    ElixirConcurrency.ChannelExamples.basic_channel()

    # Flow
    IO.puts("\n--- Flow Examples ---")
    ElixirConcurrency.FlowExamples.basic_flow()

    # Concurrency patterns
    IO.puts("\n--- Concurrency Patterns ---")
    ElixirConcurrency.ConcurrencyPatterns.timeout_pattern()

    IO.puts("\n=== Concurrency Demo Complete ===")
  end
end

# Run the demo
ElixirConcurrencyDemo.run()
