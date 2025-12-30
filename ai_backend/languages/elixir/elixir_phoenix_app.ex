# Elixir Phoenix App - Phoenix Framework Web Application
defmodule ElixirPhoenixApp do
  # Basic Phoenix App Structure
  defmodule MyAppWeb do
    use Phoenix.Component

    # Main Layout Component
    attr :current_user, :map, default: nil
    attr :flash, :map, default: %{}
    attr :socket, :map, default: %{}

    slot :inner_block

    def render(assigns) do
      ~H"""
      <div class="min-h-screen bg-gray-50">
        <header class="bg-white shadow-sm">
          <div class="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8">
            <div class="flex h-16 justify-between items-center">
              <div class="flex items-center">
                <h1 class="text-2xl font-bold text-gray-900">MyApp</h1>
              </div>
              <nav class="flex items-center space-x-4">
                <%= if @current_user do %>
                  <.link navigate={~p"/dashboard"} class="text-gray-600 hover:text-gray-900">
                    Dashboard
                  </.link>
                  <.link navigate={~p"/profile"} class="text-gray-600 hover:text-gray-900">
                    Profile
                  </.link>
                  <.link navigate={~p"/users/settings"} class="text-gray-600 hover:text-gray-900">
                    Settings
                  </.link>
                <% else %>
                  <.link navigate={~p"/users/log_in"} class="text-gray-600 hover:text-gray-900">
                    Log in
                  </.link>
                  <.link navigate={~p"/users/register"} class="text-gray-600 hover:text-gray-900">
                    Register
                  </.link>
                <% end %>
              </nav>
            </div>
          </div>
        </header>

        <main class="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8 py-8">
          <div class="mb-4">
            <%= if @flash["info"] do %>
              <div class="rounded-md bg-blue-50 p-4 mb-4">
                <p class="text-sm text-blue-700"><%= @flash["info"] %></p>
              </div>
            <% end %>

            <%= if @flash["error"] do %>
              <div class="rounded-md bg-red-50 p-4 mb-4">
                <p class="text-sm text-red-700"><%= @flash["error"] %></p>
              </div>
            <% end %>
          </div>

          <%= render_slot(@inner_block) %>
        </main>
      </div>
      """
    end
  end

  # Page Controller
  defmodule PageController do
    use MyAppWeb, :controller

    def home(conn, _params) do
      # Fetch some data for the home page
      recent_posts = MyApp.Posts.list_recent_posts(5)
      featured_users = MyApp.Users.list_featured_users(3)

      render(conn, :home,
        page_title: "Welcome to MyApp",
        recent_posts: recent_posts,
        featured_users: featured_users
      )
    end

    def about(conn, _params) do
      render(conn, :about, page_title: "About Us")
    end

    def contact(conn, _params) do
      render(conn, :contact, page_title: "Contact Us")
    end

    def privacy(conn, _params) do
      render(conn, :privacy, page_title: "Privacy Policy")
    end

    def terms(conn, _params) do
      render(conn, :terms, page_title: "Terms of Service")
    end
  end

  # User Controller
  defmodule UserController do
    use MyAppWeb, :controller
    import Ecto.Query, warn: false

    alias MyApp.Accounts
    alias MyApp.Accounts.{User, UserNotifier, UserToken}

    # Registration
    def register(conn, _params) do
      changeset = Accounts.change_user_registration(%User{})
      render(conn, :register, changeset: changeset, page_title: "Register")
    end

    def create(conn, %{"user" => user_params}) do
      case Accounts.register_user(user_params) do
        {:ok, user} ->
          Accounts.deliver_user_confirmation_instructions(
            user,
            &Routes.user_confirmation_url(conn, :confirm, &1)
          )

          conn
          |> put_flash(:info, "User created successfully.")
          |> redirect(to: "/")

        {:error, %Ecto.Changeset{} = changeset} ->
          render(conn, :register, changeset: changeset, page_title: "Register")
      end
    end

    # Login/Logout
    def login(conn, %{"user" => %{"email" => email, "password" => password}}) do
      case Accounts.get_user_by_email_and_password(email, password) do
        nil ->
          render(conn, :login,
            error: "Invalid email or password",
            page_title: "Log in"
          )

        user ->
          conn
          |> put_session(:user_token, Accounts.get_user_session_token(user))
          |> redirect(to: "/dashboard")
      end
    end

    def login(conn, _params) do
      render(conn, :login, page_title: "Log in")
    end

    def logout(conn, _params) do
      user_token = get_session(conn, :user_token)
      user_token && Accounts.delete_session_token(user_token)

      conn
      |> configure_session(drop: true)
      |> redirect(to: "/")
    end

    # Profile Management
    def profile(conn, _params) do
      user = conn.assigns.current_user
      changeset = Accounts.change_user_registration(user)
      render(conn, :profile, user: user, changeset: changeset, page_title: "Profile")
    end

    def update_profile(conn, %{"user" => user_params}) do
      case Accounts.update_user(conn.assigns.current_user, user_params) do
        {:ok, user} ->
          conn
          |> put_flash(:info, "Profile updated successfully.")
          |> redirect(to: "/profile")

        {:error, %Ecto.Changeset{} = changeset} ->
          render(conn, :profile, user: user, changeset: changeset, page_title: "Profile")
      end
    end

    # Settings
    def settings(conn, _params) do
      render(conn, :settings, page_title: "Settings")
    end

    def update_settings(conn, %{"settings" => settings_params}) do
      user = conn.assigns.current_user

      case MyApp.Settings.update_user_settings(user, settings_params) do
        {:ok, _} ->
          conn
          |> put_flash(:info, "Settings updated successfully.")
          |> redirect(to: "/users/settings")

        {:error, _} ->
          conn
          |> put_flash(:error, "Failed to update settings.")
          |> redirect(to: "/users/settings")
      end
    end

    # Account Deletion
    def delete(conn, _params) do
      user = conn.assigns.current_user

      case Accounts.delete_user(user) do
        {:ok, _} ->
          conn
          |> configure_session(drop: true)
          |> redirect(to: "/")

        {:error, _} ->
          conn
          |> put_flash(:error, "Failed to delete account.")
          |> redirect(to: "/profile")
      end
    end
  end

  # Post Controller
  defmodule PostController do
    use MyAppWeb, :controller

    alias MyApp.Posts
    alias MyApp.Posts.{Post, PostLike, Comment}

    def index(conn, %{"search" => search_term}) when search_term != "" do
      posts = Posts.search_posts(search_term)
      render(conn, :index, posts: posts, page_title: "Search Results")
    end

    def index(conn, _params) do
      posts = Posts.list_posts()
      render(conn, :index, posts: posts, page_title: "All Posts")
    end

    def show(conn, %{"id" => id}) do
      post = Posts.get_post!(id)
      comment_changeset = Posts.change_comment(%Comment{})
      render(conn, :show, post: post, comment_changeset: comment_changeset, page_title: post.title)
    end

    def new(conn, _params) do
      changeset = Posts.change_post(%Post{})
      render(conn, :new, changeset: changeset, page_title: "New Post")
    end

    def create(conn, %{"post" => post_params}) do
      case Posts.create_post(conn.assigns.current_user, post_params) do
        {:ok, post} ->
          conn
          |> put_flash(:info, "Post created successfully.")
          |> redirect(to: "/posts/#{post}")

        {:error, %Ecto.Changeset{} = changeset} ->
          render(conn, :new, changeset: changeset, page_title: "New Post")
      end
    end

    def edit(conn, %{"id" => id}) do
      post = Posts.get_post!(id)
      changeset = Posts.change_post(post)
      render(conn, :edit, post: post, changeset: changeset, page_title: "Edit Post")
    end

    def update(conn, %{"id" => id, "post" => post_params}) do
      post = Posts.get_post!(id)

      case Posts.update_post(post, post_params) do
        {:ok, post} ->
          conn
          |> put_flash(:info, "Post updated successfully.")
          |> redirect(to: "/posts/#{post}")

        {:error, %Ecto.Changeset{} = changeset} ->
          render(conn, :edit, post: post, changeset: changeset, page_title: "Edit Post")
      end
    end

    def delete(conn, %{"id" => id}) do
      post = Posts.get_post!(id)

      case Posts.delete_post(post) do
        {:ok, _} ->
          conn
          |> put_flash(:info, "Post deleted successfully.")
          |> redirect(to: "/posts")

        {:error, _} ->
          conn
          |> put_flash(:error, "Failed to delete post.")
          |> redirect(to: "/posts/#{post}")
      end
    end

    def like(conn, %{"id" => id}) do
      post = Posts.get_post!(id)
      user = conn.assigns.current_user

      case Posts.like_post(post, user) do
        {:ok, _} ->
          conn
          |> put_flash(:info, "Post liked!")
          |> redirect(to: "/posts/#{post}")

        {:error, changeset} ->
          conn
          |> put_flash(:error, "Failed to like post.")
          |> render(:show, post: post, comment_changeset: Posts.change_comment(%Comment{}))
      end
    end

    def unlike(conn, %{"id" => id}) do
      post = Posts.get_post!(id)
      user = conn.assigns.current_user

      case Posts.unlike_post(post, user) do
        {:ok, _} ->
          conn
          |> put_flash(:info, "Post unliked!")
          |> redirect(to: "/posts/#{post}")

        {:error, _} ->
          conn
          |> put_flash(:error, "Failed to unlike post.")
          |> redirect(to: "/posts/#{post}")
      end
    end

    def create_comment(conn, %{"post_id" => post_id, "comment" => comment_params}) do
      post = Posts.get_post!(post_id)
      user = conn.assigns.current_user

      case Posts.create_comment(post, user, comment_params) do
        {:ok, _comment} ->
          conn
          |> put_flash(:info, "Comment added successfully.")
          |> redirect(to: "/posts/#{post}")

        {:error, %Ecto.Changeset{} = changeset} ->
          render(conn, :show, post: post, comment_changeset: changeset)
      end
    end
  end

  # API Controller for JSON responses
  defmodule ApiController do
    use MyAppWeb, :controller

    def index(conn, _params) do
      json(conn, %{
        message: "Welcome to MyApp API",
        version: "1.0",
        endpoints: [
          "/api/users",
          "/api/posts",
          "/api/posts/:id",
          "/api/users/:id/posts"
        ]
      })
    end

    def users(conn, _params) do
      users = MyApp.Accounts.list_users()
      json(conn, %{data: users})
    end

    def user(conn, %{"id" => id}) do
      case MyApp.Accounts.get_user(id) do
        nil ->
          conn
          |> put_status(:not_found)
          |> json(%{error: "User not found"})

        user ->
          json(conn, %{data: user})
      end
    end

    def posts(conn, _params) do
      posts = MyApp.Posts.list_posts()
      json(conn, %{data: posts})
    end

    def post(conn, %{"id" => id}) do
      case MyApp.Posts.get_post(id) do
        nil ->
          conn
          |> put_status(:not_found)
          |> json(%{error: "Post not found"})

        post ->
          json(conn, %{data: post})
      end
    end

    def user_posts(conn, %{"user_id" => user_id}) do
      case MyApp.Accounts.get_user(user_id) do
        nil ->
          conn
          |> put_status(:not_found)
          |> json(%{error: "User not found"})

        user ->
          posts = MyApp.Posts.list_user_posts(user)
          json(conn, %{data: posts})
      end
    end

    def search(conn, %{"q" => query}) do
      posts = MyApp.Posts.search_posts(query)
      users = MyApp.Accounts.search_users(query)

      json(conn, %{
        posts: posts,
        users: users,
        query: query
      })
    end
  end

  # Live View for Real-time Features
  defmodule DashboardLive do
    use MyAppWeb, :live_view
    alias MyApp.{Posts, Notifications, Activities}

    @impl true
    def mount(_params, session, socket) do
      if connected?(socket) do
        current_user = get_current_user(session)

        # Subscribe to real-time updates
        Phoenix.PubSub.subscribe(MyApp.PubSub, "user:#{current_user.id}")
        Phoenix.PubSub.subscribe(MyApp.PubSub, "global_notifications")
      end

      {:ok,
       socket
       |> assign(:current_user, get_current_user(session))
       |> assign(:recent_posts, Posts.list_recent_posts(10))
       |> assign(:notifications, Notifications.list_user_notifications(session["user_id"]))
       |> assign(:activities, Activities.list_user_activities(session["user_id"]))
      }
    end

    @impl true
    def handle_event("like_post", %{"post_id" => post_id}, socket) do
      post = Posts.get_post!(post_id)
      user = socket.assigns.current_user

      case Posts.like_post(post, user) do
        {:ok, _} ->
          {:noreply,
           socket
           |> put_flash(:info, "Post liked!")
           |> assign(:recent_posts, Posts.list_recent_posts(10))
          }

        {:error, _} ->
          {:noreply, put_flash(socket, :error, "Failed to like post")}
      end
    end

    @impl true
    def handle_event("unlike_post", %{"post_id" => post_id}, socket) do
      post = Posts.get_post!(post_id)
      user = socket.assigns.current_user

      case Posts.unlike_post(post, user) do
        {:ok, _} ->
          {:noreply,
           socket
           |> put_flash(:info, "Post unliked!")
           |> assign(:recent_posts, Posts.list_recent_posts(10))
          }

        {:error, _} ->
          {:noreply, put_flash(socket, :error, "Failed to unlike post")}
      end
    end

    @impl true
    def handle_event("mark_notification_read", %{"notification_id" => notification_id}, socket) do
      Notifications.mark_as_read(notification_id)
      notifications = Notifications.list_user_notifications(socket.assigns.current_user.id)

      {:noreply, assign(socket, :notifications, notifications)}
    end

    @impl true
    def handle_info({:new_post, post}, socket) do
      recent_posts = [post | socket.assigns.recent_posts] |> Enum.take(10)
      {:noreply, assign(socket, :recent_posts, recent_posts)}
    end

    @impl true
    def handle_info({:new_notification, notification}, socket) do
      notifications = [notification | socket.assigns.notifications]
      {:noreply, assign(socket, :notifications, notifications)}
    end

    @impl true
    def render(assigns) do
      ~H"""
      <div class="dashboard">
        <div class="grid grid-cols-1 lg:grid-cols-3 gap-6">
          <!-- Main Content -->
          <div class="lg:col-span-2 space-y-6">
            <!-- Recent Posts -->
            <div class="bg-white rounded-lg shadow p-6">
              <h2 class="text-lg font-semibold mb-4">Recent Posts</h2>
              <div class="space-y-4">
                <%= for post <- @recent_posts do %>
                  <div class="border-b border-gray-200 pb-4 last:border-b-0">
                    <div class="flex items-center justify-between mb-2">
                      <h3 class="font-medium text-gray-900">
                        <.link navigate={~p"/posts/#{post}"} class="hover:text-blue-600">
                          <%= post.title %>
                        </.link>
                      </h3>
                      <div class="flex items-center space-x-2">
                        <button
                          phx-click="like_post"
                          phx-value-post_id={post.id}
                          class="text-sm text-gray-500 hover:text-red-500"
                        >
                          ❤️ <%= length(post.likes) %>
                        </button>
                      </div>
                    </div>
                    <p class="text-gray-600 text-sm mb-2">
                      <%= String.slice(post.content, 0, 200) %>...
                    </p>
                    <div class="flex items-center text-xs text-gray-400">
                      <span>by <%= post.user.name %></span>
                      <span class="mx-2">•</span>
                      <span><%= format_date(post.inserted_at) %></span>
                    </div>
                  </div>
                <% end %>
              </div>
            </div>
          </div>

          <!-- Sidebar -->
          <div class="space-y-6">
            <!-- Notifications -->
            <div class="bg-white rounded-lg shadow p-6">
              <h2 class="text-lg font-semibold mb-4">Notifications</h2>
              <div class="space-y-3">
                <%= for notification <- @notifications do %>
                  <div class={"p-3 rounded-lg #{if notification.read, do: "bg-gray-50", else: "bg-blue-50"}"}>
                    <p class="text-sm font-medium text-gray-900">
                      <%= notification.title %>
                    </p>
                    <p class="text-xs text-gray-600 mt-1">
                      <%= notification.message %>
                    </p>
                    <div class="flex justify-between items-center mt-2">
                      <span class="text-xs text-gray-400">
                        <%= format_date(notification.inserted_at) %>
                      </span>
                      <%= if not notification.read do %>
                        <button
                          phx-click="mark_notification_read"
                          phx-value-notification_id={notification.id}
                          class="text-xs text-blue-600 hover:text-blue-800"
                        >
                          Mark as read
                        </button>
                      <% end %>
                    </div>
                  </div>
                <% end %>
              </div>
            </div>

            <!-- Activities -->
            <div class="bg-white rounded-lg shadow p-6">
              <h2 class="text-lg font-semibold mb-4">Recent Activities</h2>
              <div class="space-y-3">
                <%= for activity <- @activities do %>
                  <div class="flex items-start space-x-3">
                    <div class="flex-shrink-0">
                      <div class="w-8 h-8 bg-blue-100 rounded-full flex items-center justify-center">
                        <%= activity_icon(activity.type) %>
                      </div>
                    </div>
                    <div class="flex-1">
                      <p class="text-sm text-gray-900">
                        <%= activity.description %>
                      </p>
                      <p class="text-xs text-gray-400 mt-1">
                        <%= format_date(activity.inserted_at) %>
                      </p>
                    </div>
                  </div>
                <% end %>
              </div>
            </div>
          </div>
        </div>
      </div>
      """
    end

    defp get_current_user(session) do
      # In a real app, you'd fetch this from the database
      case session["user_token"] do
        nil -> nil
        token ->
          MyApp.Accounts.get_user_by_session_token(token)
      end
    end

    defp format_date(date) do
      Calendar.strftime(date, "%b %d, %Y")
    end

    defp activity_icon(type) do
      case type do
        :post_created -> "📝"
        :post_liked -> "❤️"
        :comment_added -> "💬"
        :user_followed -> "👤"
        _ -> "📰"
      end
    end
  end

  # Application Context
  defmodule MyApp do
    use Ecto.Schema
    import Ecto.Changeset
    import Ecto.Query

    @primary_key {:id, :binary_id, autogenerate: true}
    schema "users" do
      field :name, :string
      field :email, :string
      field :encrypted_password, :string
      field :confirmed_at, :naive_datetime
      field :role, :string, default: "user"

      has_many :posts, MyApp.Posts.Post
      has_many :comments, MyApp.Posts.Comment
      has_many :post_likes, MyApp.Posts.PostLike
      has_many :activities, MyApp.Activities.Activity

      timestamps()
    end

    def create_user(attrs) do
      %__MODULE__{}
      |> cast(attrs, [:name, :email, :password])
      |> validate_required([:name, :email, :password])
      |> validate_format(:email, ~r/@/)
      |> unique_constraint(:email)
      |> put_password_hash()
      |> MyApp.Repo.insert()
    end

    defp put_password_hash(changeset) do
      if password = get_change(changeset, :password) do
        changeset
        |> put_change(:encrypted_password, Bcrypt.hash_pwd_salt(password))
        |> delete_change(:password)
      else
        changeset
      end
    end

    def get_by_email(email) do
      from(u in __MODULE__, where: u.email == ^email)
      |> MyApp.Repo.one()
    end

    def get_by_email_and_password(email, password) do
      case get_by_email(email) do
        nil -> nil
        user ->
          if Bcrypt.verify_pass(password, user.encrypted_password) do
            user
          else
            nil
          end
      end
    end
  end

  # Posts Context
  defmodule MyApp.Posts do
    use Ecto.Schema
    import Ecto.Changeset
    import Ecto.Query

    @primary_key {:id, :binary_id, autogenerate: true}
    schema "posts" do
      field :title, :string
      field :content, :string
      field :status, :string, default: "published"

      belongs_to :user, MyApp
      has_many :comments, MyApp.Posts.Comment
      has_many :post_likes, MyApp.Posts.PostLike

      timestamps()
    end

    def create_post(user, attrs) do
      %__MODULE__{}
      |> cast(attrs, [:title, :content, :status])
      |> validate_required([:title, :content])
      |> put_assoc(:user, user)
      |> MyApp.Repo.insert()
    end

    def list_posts do
      from(p in __MODULE__,
        preload: [:user, :comments, :post_likes],
        order_by: [desc: p.inserted_at]
      )
      |> MyApp.Repo.all()
    end

    def get_post!(id) do
      from(p in __MODULE__,
        where: p.id == ^id,
        preload: [:user, :comments, :post_likes]
      )
      |> MyApp.Repo.one!()
    end

    def search_posts(query) do
      from(p in __MODULE__,
        where: ilike(p.title, ^"%#{query}%") or ilike(p.content, ^"%#{query}%"),
        preload: [:user, :comments, :post_likes],
        order_by: [desc: p.inserted_at]
      )
      |> MyApp.Repo.all()
    end

    def like_post(post, user) do
      case MyApp.Posts.PostLike.get_by_user_and_post(user.id, post.id) do
        nil ->
          %MyApp.Posts.PostLike{}
          |> MyApp.Posts.PostLike.changeset(%{user_id: user.id, post_id: post.id})
          |> MyApp.Repo.insert()

        existing_like ->
          MyApp.Repo.delete(existing_like)
      end
    end
  end

  # Utility Functions
  defmodule Utils do
    def format_date(date) do
      Calendar.strftime(date, "%b %d, %Y at %I:%M %p")
    end

    def truncate(text, length \\ 100) do
      if String.length(text) > length do
        String.slice(text, 0, length) <> "..."
      else
        text
      end
    end

    def slugify(title) do
      title
      |> String.downcase()
      |> String.replace(~r/[^a-z0-9]+/, "-")
      |> String.replace(~r/^-+|-+$/, "")
    end

    def gravatar_url(email, size \\ 80) do
      hash = :crypto.hash(:md5, String.downcase(email)) |> Base.encode16()
      "https://www.gravatar.com/avatar/#{hash}?s=#{size}"
    end
  end
end

# Phoenix App Router
defmodule ElixirPhoenixApp.Router do
  use ElixirPhoenixApp, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  # Public routes
  scope "/", ElixirPhoenixApp do
    pipe_through :browser

    get "/", PageController, :home
    get "/about", PageController, :about
    get "/contact", PageController, :contact
    get "/privacy", PageController, :privacy
    get "/terms", PageController, :terms

    # User authentication
    get "/users/register", UserController, :register
    post "/users/register", UserController, :create
    get "/users/log_in", UserController, :login
    post "/users/log_in", UserController, :login
    post "/users/log_out", UserController, :logout
  end

  # Protected routes
  scope "/", ElixirPhoenixApp do
    pipe_through [:browser, :require_authenticated_user]

    # Dashboard
    get "/dashboard", DashboardLive, :index

    # User profile
    get "/profile", UserController, :profile
    put "/profile", UserController, :update_profile
    get "/users/settings", UserController, :settings
    put "/users/settings", UserController, :update_settings
    delete "/users", UserController, :delete

    # Posts
    get "/posts", PostController, :index
    get "/posts/new", PostController, :new
    post "/posts", PostController, :create
    get "/posts/:id", PostController, :show
    get "/posts/:id/edit", PostController, :edit
    put "/posts/:id", PostController, :update
    delete "/posts/:id", PostController, :delete
    post "/posts/:id/like", PostController, :like
    post "/posts/:id/unlike", PostController, :unlike
    post "/posts/:post_id/comments", PostController, :create_comment
  end

  # API routes
  scope "/api", ElixirPhoenixApp do
    pipe_through :api

    get "/", ApiController, :index
    get "/users", ApiController, :users
    get "/users/:id", ApiController, :user
    get "/users/:user_id/posts", ApiController, :user_posts
    get "/posts", ApiController, :posts
    get "/posts/:id", ApiController, :post
    get "/search", ApiController, :search
  end

  # Enable LiveDashboard for development
  if Mix.env() == :dev do
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser
      live_dashboard "/dashboard", metrics: ElixirPhoenixApp.Telemetry
    end
  end

  # Authentication plug
  defp require_authenticated_user(conn, _opts) do
    if get_session(conn, :user_token) do
      conn
    else
      conn
      |> redirect(to: "/users/log_in")
      |> halt()
    end
  end
end

# Demo usage
defmodule ElixirPhoenixAppDemo do
  def run do
    IO.puts("=== Elixir Phoenix App Demo ===")

    # Show Phoenix app structure
    IO.puts("\n--- Phoenix App Structure ---")
    IO.puts("Main Components:")
    IO.puts("• MyAppWeb - Main web module")
    IO.puts("• PageController - Basic page handling")
    IO.puts("• UserController - User management")
    IO.puts("• PostController - Post CRUD operations")
    IO.puts("• ApiController - JSON API endpoints")
    IO.puts("• DashboardLive - Real-time dashboard")
    IO.puts("• Router - URL routing and pipelines")

    # Show routing structure
    IO.puts("\n--- Routing Structure ---")
    IO.puts("Public Routes:")
    IO.puts("  GET /              - Home page")
    IO.puts("  GET /about         - About page")
    IO.puts("  GET /contact       - Contact page")
    IO.puts("  GET /privacy       - Privacy policy")
    IO.puts("  GET /users/register - User registration")
    IO.puts("  GET /users/log_in  - User login")

    IO.puts("\nProtected Routes:")
    IO.puts("  GET /dashboard     - User dashboard")
    IO.puts("  GET /profile       - User profile")
    IO.puts("  GET /posts         - List posts")
    IO.puts("  GET /posts/new     - Create post")
    IO.puts("  GET /posts/:id     - Show post")

    IO.puts("\nAPI Routes:")
    IO.puts("  GET /api/          - API info")
    IO.puts("  GET /api/users     - List users")
    IO.puts("  GET /api/posts     - List posts")
    IO.puts("  GET /api/search    - Search content")

    # Show controller functionality
    IO.puts("\n--- Controller Features ---")
    IO.puts("User Management:")
    IO.puts("• User registration with validation")
    IO.puts("• Email/password authentication")
    IO.puts("• Profile management")
    IO.puts("• Settings management")
    IO.puts("• Account deletion")

    IO.puts("\nPost Management:")
    IO.puts("• Create, read, update, delete posts")
    IO.puts("• Post liking/unliking")
    IO.puts("• Comment system")
    IO.puts("• Search functionality")
    IO.puts("• Real-time updates")

    # Show LiveView features
    IO.puts("\n--- LiveView Features ---")
    IO.puts("• Real-time dashboard")
    IO.puts("• Interactive post liking")
    IO.puts("• Live notifications")
    IO.puts("• Activity tracking")
    IO.puts("• Phoenix PubSub integration")

    # Show context features
    IO.puts("\n--- Context Architecture ---")
    IO.puts("• MyApp - User management context")
    IO.puts("• MyApp.Posts - Post management context")
    IO.puts("• Ecto schemas and changesets")
    IO.puts("• Query composition")
    IO.puts("• Data validation")

    # Show security features
    IO.puts("\n--- Security Features ---")
    IO.puts("• CSRF protection")
    IO.puts("• Secure session management")
    IO.puts("• Password hashing with Bcrypt")
    IO.puts("• Input validation")
    IO.puts("• Authorization pipelines")

    # Show real-time features
    IO.puts("\n--- Real-time Features ---")
    IO.puts("• Phoenix Channels for real-time updates")
    IO.puts("• LiveView for interactive UIs")
    IO.puts("• PubSub for broadcasting events")
    IO.puts("• Database change notifications")

    IO.puts("\n=== Phoenix App Demo Complete ===")
  end
end

# Run the demo
if __ENV__.module == ElixirPhoenixAppDemo do
  ElixirPhoenixAppDemo.run()
end
