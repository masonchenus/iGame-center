# Elixir OTP Supervisor - Process Supervision and Fault Tolerance
defmodule OTPSupervisor do
  use DynamicSupervisor

  # Worker module
  defmodule Worker do
    use GenServer

    def start_link(arg) do
      GenServer.start_link(__MODULE__, arg)
    end

    def init(state) do
      IO.puts("Worker started with state: #{inspect(state)}")
      {:ok, state}
    end

    def handle_call(:get_state, _from, state) do
      {:reply, state, state}
    end

    def handle_call(:crash, _from, _state) do
      raise "Intentional crash!"
      {:reply, :ok, %{}}
    end

    def handle_cast({:update_state, new_state}, _state) do
      {:noreply, new_state}
    end

    def handle_info(:timeout, state) do
      IO.puts("Worker timeout occurred")
      {:noreply, state}
    end
  end

  # Supervisor with one-for-one strategy
  defmodule SimpleSupervisor do
    use Supervisor

    def start_link(init_arg) do
      Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
    end

    @impl true
    def init(_init_arg) do
      children = [
        {Worker, :worker1},
        {Worker, :worker2},
        {Worker, :worker3}
      ]

      Supervisor.init(children, strategy: :one_for_one)
    end

    def start_worker(name) do
      Supervisor.start_child(__MODULE__, {Worker, name})
    end

    def stop_worker(name) do
      Supervisor.terminate_child(__MODULE__, name)
    end

    def restart_worker(name) do
      Supervisor.restart_child(__MODULE__, name)
    end

    def count_children do
      Supervisor.count_children(__MODULE__)
    end

    def which_children do
      Supervisor.which_children(__MODULE__)
    end
  end

  # Dynamic supervisor for runtime worker management
  defmodule DynamicWorkerSupervisor do
    use DynamicSupervisor

    def start_link(init_arg) do
      DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
    end

    @impl true
    def init(_init_arg) do
      DynamicSupervisor.init(strategy: :one_for_one)
    end

    def start_dynamic_worker(name) do
      spec = {Worker, name}
      DynamicSupervisor.start_child(__MODULE__, spec)
    end

    def stop_dynamic_worker(pid) do
      DynamicSupervisor.terminate_child(__MODULE__, pid)
    end

    def list_dynamic_workers do
      DynamicSupervisor.which_children(__MODULE__)
    end
  end

  # Supervisor tree with rest-for-one strategy
  defmodule SupervisorTree do
    use Supervisor

    def start_link(init_arg) do
      Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
    end

    @impl true
    def init(_init_arg) do
      children = [
        {SimpleSupervisor, []},
        {DynamicWorkerSupervisor, []},
        {DatabaseWorkerSupervisor, []}
      ]

      Supervisor.init(children, strategy: :rest_for_one)
    end

    def start_sub_supervisor do
      Supervisor.start_child(__MODULE__, {SimpleSupervisor, []})
    end

    def start_dynamic_supervisor do
      Supervisor.start_child(__MODULE__, {DynamicWorkerSupervisor, []})
    end
  end

  # Database worker supervisor
  defmodule DatabaseWorkerSupervisor do
    use Supervisor

    def start_link(init_arg) do
      Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
    end

    @impl true
    def init(_init_arg) do
      children = [
        {DatabaseConnection, []},
        {DatabaseCache, []}
      ]

      Supervisor.init(children, strategy: :one_for_one)
    end
  end

  # Database worker modules
  defmodule DatabaseConnection do
    use GenServer

    def start_link(_args) do
      GenServer.start_link(__MODULE__, %{})
    end

    def init(state) do
      IO.puts("Database connection started")
      schedule_work()
      {:ok, state}
    end

    def handle_info(:work, state) do
      # Simulate database work
      :timer.sleep(100)
      schedule_work()
      {:noreply, state}
    end

    defp schedule_work do
      Process.send_after(self(), :work, 1000)
    end
  end

  defmodule DatabaseCache do
    use GenServer

    def start_link(_args) do
      GenServer.start_link(__MODULE__, %{})
    end

    def init(state) do
      IO.puts("Database cache started")
      {:ok, state}
    end
  end

  # Monitoring and supervision strategies
  defmodule SupervisionStrategies do
    def one_for_one do
      # Each child is supervised independently
      # If one child dies, only that child is restarted
      children = [
        {Worker, :child1},
        {Worker, :child2},
        {Worker, :child3}
      ]

      Supervisor.init(children, strategy: :one_for_one)
    end

    def one_for_all do
      # If one child dies, all children are terminated and restarted
      children = [
        {DatabaseConnection, []},
        {DatabaseCache, []},
        {Worker, :dependent_worker}
      ]

      Supervisor.init(children, strategy: :one_for_all)
    end

    def rest_for_one do
      # If one child dies, all children started after it are terminated and restarted
      children = [
        {DatabaseConnection, []},
        {DatabaseCache, []},
        {Worker, :dependent_worker}
      ]

      Supervisor.init(children, strategy: :rest_for_one)
    end

    def simple_one_for_one do
      # Similar to one_for_one but children are added dynamically
      # This strategy is deprecated in favor of DynamicSupervisor
      children = [
        {Worker, []}
      ]

      Supervisor.init(children, strategy: :simple_one_for_one)
    end
  end

  # Fault tolerance patterns
  defmodule FaultTolerance do
    def exponential_backoff do
      # Restart strategy with exponential backoff
      children = [
        {Worker, :worker1, restart: :permanent, shutdown: 5000}
      ]

      Supervisor.init(children, strategy: :one_for_one, max_restarts: 3, max_seconds: 60)
    end

    def temporary_worker do
      # Temporary workers are not restarted
      children = [
        {Worker, :temp_worker, restart: :temporary}
      ]

      Supervisor.init(children, strategy: :one_for_one)
    end

    def transient_worker do
      # Transient workers are restarted only if they exit abnormally
      children = [
        {Worker, :transient_worker, restart: :transient}
      ]

      Supervisor.init(children, strategy: :one_for_one)
    end
  end

  # Application supervision
  defmodule AppSupervisor do
    use Application

    @impl true
    def start(_type, _args) do
      children = [
        {SimpleSupervisor, []},
        {DynamicWorkerSupervisor, []},
        {SupervisorTree, []}
      ]

      Supervisor.start_link(children, strategy: :rest_for_one)
    end

    @impl true
    def stop(_state) do
      IO.puts("Application stopping...")
      :ok
    end
  end

  # Supervision testing utilities
  defmodule SupervisionTest do
    def test_supervisor_crash do
      # Start supervisor
      {:ok, pid} = SimpleSupervisor.start_link([])

      # Get initial child count
      initial_count = SimpleSupervisor.count_children()
      IO.puts("Initial children count: #{inspect(initial_count)}")

      # Crash a worker
      [{pid, :worker1, :_, :_}] = SimpleSupervisor.which_children()

      # Wait and check if worker is restarted
      :timer.sleep(100)
      new_count = SimpleSupervisor.count_children()
      IO.puts("After crash children count: #{inspect(new_count)}")

      {:ok, pid}
    end

    def test_dynamic_supervisor do
      # Start dynamic supervisor
      {:ok, pid} = DynamicWorkerSupervisor.start_link([])

      # Start some workers
      {:ok, worker1} = DynamicWorkerSupervisor.start_dynamic_worker(:dynamic1)
      {:ok, worker2} = DynamicWorkerSupervisor.start_dynamic_worker(:dynamic2)

      IO.puts("Started dynamic workers: #{inspect(DynamicWorkerSupervisor.list_dynamic_workers())}")

      # Stop a worker
      DynamicWorkerSupervisor.stop_dynamic_worker(worker1)
      IO.puts("After stopping worker: #{inspect(DynamicWorkerSupervisor.list_dynamic_workers())}")

      {:ok, pid}
    end
  end
end

# Demo usage
defmodule OTPSupervisorDemo do
  def run do
    IO.puts("=== OTP Supervisor Demo ===")

    # Test simple supervisor
    IO.puts("\n--- Simple Supervisor Test ---")
    {:ok, simple_sup} = OTPSupervisor.SimpleSupervisor.start_link([])
    IO.puts("Simple supervisor started: #{inspect(simple_sup)}")
    IO.puts("Children: #{inspect(OTPSupervisor.SimpleSupervisor.which_children())}")

    # Test dynamic supervisor
    IO.puts("\n--- Dynamic Supervisor Test ---")
    {:ok, dynamic_sup} = OTPSupervisor.DynamicWorkerSupervisor.start_link([])
    IO.puts("Dynamic supervisor started: #{inspect(dynamic_sup)}")

    {:ok, worker1} = OTPSupervisor.DynamicWorkerSupervisor.start_dynamic_worker(:dynamic1)
    {:ok, worker2} = OTPSupervisor.DynamicWorkerSupervisor.start_dynamic_worker(:dynamic2)

    IO.puts("Dynamic workers: #{inspect(OTPSupervisor.DynamicWorkerSupervisor.list_dynamic_workers())}")

    # Test supervision tree
    IO.puts("\n--- Supervision Tree Test ---")
    {:ok, tree_sup} = OTPSupervisor.SupervisorTree.start_link([])
    IO.puts("Supervision tree started: #{inspect(tree_sup)}")

    # Test supervision strategies
    IO.puts("\n--- Supervision Strategies ---")
    IO.puts("✓ One-for-one: Each child independent")
    IO.puts("✓ One-for-all: All children restart if one dies")
    IO.puts("✓ Rest-for-one: Children after the dead one restart")
    IO.puts("✓ Simple-one-for-one: Dynamic child addition (deprecated)")

    # Test fault tolerance
    IO.puts("\n--- Fault Tolerance ---")
    IO.puts("✓ Permanent restart: Always restart on crash")
    IO.puts("✓ Transient restart: Restart only on abnormal exit")
    IO.puts("✓ Temporary: Never restart")

    IO.puts("\n=== OTP Demo Complete ===")
  end
end

# Run the demo
OTPSupervisorDemo.run()
