# Elixir Ecto Queries - Database Query Examples and Patterns
defmodule ElixirEctoQueries do
  # Basic Ecto Schema Definitions
  defmodule User do
    use Ecto.Schema
    import Ecto.Changeset

    @primary_key {:id, :binary_id, autogenerate: true}
    @foreign_key_type :binary_id
    schema "users" do
      field :name, :string
      field :email, :string
      field :age, :integer
      field :bio, :string
      field :is_active, :boolean, default: true
      field :role, :string, default: "user"
      field :avatar_url, :string
      field :last_login_at, :naive_datetime
      field :settings, :map, default: %{}

      has_many :posts, ElixirEctoQueries.Post
      has_many :comments, ElixirEctoQueries.Comment
      has_many :likes, ElixirEctoQueries.Like
      has_many :followers, ElixirEctoQueries.Follower, foreign_key: :following_id
      has_many :following, ElixirEctoQueries.Follower, foreign_key: :follower_id

      timestamps()
    end

    def changeset(user, attrs) do
      user
      |> cast(attrs, [:name, :email, :age, :bio, :is_active, :role, :avatar_url, :last_login_at, :settings])
      |> validate_required([:name, :email])
      |> validate_format(:email, ~r/@/)
      |> validate_number(:age, greater_than: 0, less_than: 150)
      |> unique_constraint(:email)
    end

    def registration_changeset(user, attrs) do
      user
      |> cast(attrs, [:name, :email, :password])
      |> validate_required([:name, :email, :password])
      |> validate_format(:email, ~r/@/)
      |> validate_length(:password, min: 8)
      |> unique_constraint(:email)
      |> put_password_hash()
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
  end

  defmodule Post do
    use Ecto.Schema
    import Ecto.Changeset

    @primary_key {:id, :binary_id, autogenerate: true}
    @foreign_key_type :binary_id
    schema "posts" do
      field :title, :string
      field :content, :string
      field :slug, :string
      field :status, :string, default: "draft"
      field :view_count, :integer, default: 0
      field :like_count, :integer, default: 0
      field :comment_count, :integer, default: 0
      field :published_at, :naive_datetime
      field :tags, {:array, :string}, default: []

      belongs_to :user, ElixirEctoQueries.User
      has_many :comments, ElixirEctoQueries.Comment
      has_many :likes, ElixirEctoQueries.Like
      has_many :post_tags, ElixirEctoQueries.PostTag

      timestamps()
    end

    def changeset(post, attrs) do
      post
      |> cast(attrs, [:title, :content, :status, :view_count, :like_count, :comment_count, :published_at, :tags, :user_id])
      |> validate_required([:title, :content, :user_id])
      |> validate_length(:title, min: 3, max: 255)
      |> validate_length(:content, min: 10)
      |> unique_constraint(:slug)
      |> put_slug()
    end

    defp put_slug(changeset) do
      if title = get_change(changeset, :title) do
        slug = title
               |> String.downcase()
               |> String.replace(~r/[^a-z0-9]+/, "-")
               |> String.replace(~r/^-+|-+$/, "")
        put_change(changeset, :slug, slug)
      else
        changeset
      end
    end
  end

  defmodule Comment do
    use Ecto.Schema
    import Ecto.Changeset

    @primary_key {:id, :binary_id, autogenerate: true}
    @foreign_key_type :binary_id
    schema "comments" do
      field :content, :string
      field :is_approved, :boolean, default: false

      belongs_to :user, ElixirEctoQueries.User
      belongs_to :post, ElixirEctoQueries.Post
      belongs_to :parent_comment, ElixirEctoQueries.Comment, type: :binary_id

      timestamps()
    end

    def changeset(comment, attrs) do
      comment
      |> cast(attrs, [:content, :is_approved, :user_id, :post_id, :parent_comment_id])
      |> validate_required([:content, :user_id, :post_id])
      |> validate_length(:content, min: 1, max: 1000)
      |> foreign_key_constraint(:user_id)
      |> foreign_key_constraint(:post_id)
    end
  end

  defmodule Like do
    use Ecto.Schema
    import Ecto.Changeset

    @primary_key {:id, :binary_id, autogenerate: true}
    @foreign_key_type :binary_id
    schema "likes" do
      belongs_to :user, ElixirEctoQueries.User
      belongs_to :post, ElixirEctoQueries.Post

      timestamps()
    end

    def changeset(like, attrs) do
      like
      |> cast(attrs, [:user_id, :post_id])
      |> validate_required([:user_id, :post_id])
      |> foreign_key_constraint(:user_id)
      |> foreign_key_constraint(:post_id)
      |> unique_constraint([:user_id, :post_id])
    end
  end

  defmodule Follower do
    use Ecto.Schema
    import Ecto.Changeset

    @primary_key {:id, :binary_id, autogenerate: true}
    @foreign_key_type :binary_id
    schema "followers" do
      belongs_to :follower, ElixirEctoQueries.User, type: :binary_id
      belongs_to :following, ElixirEctoQueries.User, type: :binary_id

      timestamps()
    end

    def changeset(follower, attrs) do
      follower
      |> cast(attrs, [:follower_id, :following_id])
      |> validate_required([:follower_id, :following_id])
      |> foreign_key_constraint(:follower_id)
      |> foreign_key_constraint(:following_id)
      |> unique_constraint([:follower_id, :following_id])
      |> check_constraint(:follower_id, name: :cannot_follow_yourself, check: "follower_id != following_id")
    end
  end

  defmodule PostTag do
    use Ecto.Schema
    import Ecto.Changeset

    @primary_key {:id, :binary_id, autogenerate: true}
    @foreign_key_type :binary_id
    schema "post_tags" do
      belongs_to :post, ElixirEctoQueries.Post
      belongs_to :tag, ElixirEctoQueries.Tag

      timestamps()
    end

    def changeset(post_tag, attrs) do
      post_tag
      |> cast(attrs, [:post_id, :tag_id])
      |> validate_required([:post_id, :tag_id])
      |> foreign_key_constraint(:post_id)
      |> foreign_key_constraint(:tag_id)
      |> unique_constraint([:post_id, :tag_id])
    end
  end

  defmodule Tag do
    use Ecto.Schema
    import Ecto.Changeset

    @primary_key {:id, :binary_id, autogenerate: true}
    @foreign_key_type :binary_id
    schema "tags" do
      field :name, :string
      field :description, :string

      has_many :post_tags, ElixirEctoQueries.PostTag

      timestamps()
    end

    def changeset(tag, attrs) do
      tag
      |> cast(attrs, [:name, :description])
      |> validate_required([:name])
      |> unique_constraint(:name)
    end
  end

  # Repository Pattern
  defmodule UserRepo do
    import Ecto.Query
    alias ElixirEctoQueries.{User, Post, Repo}

    def get(id), do: Repo.get(User, id)
    def get!(id), do: Repo.get!(User, id)
    def get_by_email(email), do: Repo.get_by(User, email: email)

    def list_users do
      from(u in User, order_by: [desc: u.inserted_at])
      |> Repo.all()
    end

    def list_active_users do
      from(u in User, where: u.is_active == true, order_by: [desc: u.inserted_at])
      |> Repo.all()
    end

    def search_users(search_term) when is_binary(search_term) and search_term != "" do
      pattern = "%#{search_term}%"
      from(u in User,
        where: ilike(u.name, ^pattern) or ilike(u.email, ^pattern),
        order_by: [desc: u.inserted_at]
      )
      |> Repo.all()
    end

    def get_users_by_role(role) do
      from(u in User, where: u.role == ^role, order_by: [asc: u.name])
      |> Repo.all()
    end

    def get_users_older_than(age) do
      from(u in User, where: u.age > ^age, order_by: [asc: u.age])
      |> Repo.all()
    end

    def get_users_with_posts do
      from(u in User,
        preload: [:posts],
        order_by: [desc: u.inserted_at]
      )
      |> Repo.all()
    end

    def get_user_with_posts_and_comments(user_id) do
      from(u in User,
        where: u.id == ^user_id,
        preload: [posts: [:comments, :likes]],
        order_by: [desc: u.posts.inserted_at]
      )
      |> Repo.one!()
    end

    def get_users_with_post_count do
      from(u in User,
        left_join: p in Post, on: p.user_id == u.id,
        group_by: u.id,
        select: %{u | post_count: count(p.id)},
        order_by: [desc: count(p.id)]
      )
      |> Repo.all()
    end

    def get_users_with_most_posts(limit \\ 10) do
      from(u in User,
        left_join: p in Post, on: p.user_id == u.id,
        group_by: u.id,
        select: %{user: u, post_count: count(p.id)},
        order_by: [desc: count(p.id)],
        limit: ^limit
      )
      |> Repo.all()
    end

    def get_recent_users(days \\ 7) do
      from(u in User,
        where: u.inserted_at >= ago(^days, "day"),
        order_by: [desc: u.inserted_at]
      )
      |> Repo.all()
    end

    def get_inactive_users(days \\ 30) do
      from(u in User,
        where: is_nil(u.last_login_at) or u.last_login_at < ago(^days, "day"),
        order_by: [asc: u.last_login_at]
      )
      |> Repo.all()
    end

    def get_users_statistics do
      Repo.one!(
        from(u in User,
          select: %{
            total_users: count(),
            active_users: count(filter: u.is_active),
            admins: count(filter: u.role == "admin"),
            average_age: avg(u.age)
          }
        )
      )
    end

    def paginate_users(page \\ 1, page_size \\ 20) do
      offset = (page - 1) * page_size

      users = from(u in User,
        order_by: [desc: u.inserted_at],
        limit: ^page_size,
        offset: ^offset
      )
      |> Repo.all()

      total_count = Repo.one!(from(u in User, select: count()))

      %{users: users, total_count: total_count, page: page, page_size: page_size}
    end

    def bulk_update_user_ages do
      from(u in User, where: is_nil(u.age))
      |> Repo.update_all(set: [age: 25])
    end

    def delete_inactive_users do
      from(u in User, where: u.is_active == false)
      |> Repo.delete_all()
    end
  end

  defmodule PostRepo do
    import Ecto.Query
    alias ElixirEctoQueries.{Post, User, Comment, Like, Repo}

    def list_posts do
      from(p in Post,
        preload: [:user, :comments, :likes],
        order_by: [desc: p.inserted_at]
      )
      |> Repo.all()
    end

    def list_published_posts do
      from(p in Post,
        where: p.status == "published",
        preload: [:user, :comments, :likes],
        order_by: [desc: p.published_at]
      )
      |> Repo.all()
    end

    def get_post_with_comments(post_id) do
      from(p in Post,
        where: p.id == ^post_id,
        preload: [user: [], comments: {from(c in Comment, order_by: [asc: c.inserted_at]), [:user]}]
      )
      |> Repo.one!()
    end

    def search_posts(search_term) when is_binary(search_term) and search_term != "" do
      pattern = "%#{search_term}%"
      from(p in Post,
        where: ilike(p.title, ^pattern) or ilike(p.content, ^pattern),
        where: p.status == "published",
        preload: [:user, :comments, :likes],
        order_by: [desc: p.published_at]
      )
      |> Repo.all()
    end

    def get_posts_by_user(user_id) do
      from(p in Post,
        where: p.user_id == ^user_id,
        preload: [:comments, :likes],
        order_by: [desc: p.inserted_at]
      )
      |> Repo.all()
    end

    def get_posts_by_tag(tag_name) do
      from(p in Post,
        join: pt in "post_tags", on: pt.post_id == p.id,
        join: t in "tags", on: t.id == pt.tag_id,
        where: t.name == ^tag_name and p.status == "published",
        preload: [:user, :comments, :likes],
        order_by: [desc: p.published_at]
      )
      |> Repo.all()
    end

    def get_popular_posts(limit \\ 10) do
      from(p in Post,
        where: p.status == "published",
        preload: [:user],
        order_by: [desc: p.view_count, desc: p.like_count],
        limit: ^limit
      )
      |> Repo.all()
    end

    def get_recent_posts(days \\ 7, limit \\ 10) do
      from(p in Post,
        where: p.status == "published" and p.inserted_at >= ago(^days, "day"),
        preload: [:user],
        order_by: [desc: p.inserted_at],
        limit: ^limit
      )
      |> Repo.all()
    end

    def get_posts_with_comments_count do
      from(p in Post,
        left_join: c in Comment, on: c.post_id == p.id,
        group_by: p.id,
        select: %{p | comment_count: count(c.id)},
        order_by: [desc: p.comment_count]
      )
      |> Repo.all()
    end

    def get_posts_statistics do
      Repo.one!(
        from(p in Post,
          select: %{
            total_posts: count(),
            published_posts: count(filter: p.status == "published"),
            draft_posts: count(filter: p.status == "draft"),
            total_views: sum(p.view_count),
            total_likes: sum(p.like_count),
            average_comments: avg(p.comment_count)
          }
        )
      )
    end

    def increment_post_view_count(post_id) do
      from(p in Post, where: p.id == ^post_id)
      |> Repo.update_all(inc: [view_count: 1])
    end

    def update_post_like_count(post_id) do
      count = Repo.one!(from(l in Like, where: l.post_id == ^post_id, select: count()))
      from(p in Post, where: p.id == ^post_id)
      |> Repo.update_all(set: [like_count: ^count])
    end

    def delete_old_drafts(days \\ 30) do
      from(p in Post,
        where: p.status == "draft" and p.inserted_at < ago(^days, "day")
      )
      |> Repo.delete_all()
    end

    def bulk_publish_scheduled_posts do
      from(p in Post,
        where: p.status == "scheduled" and p.published_at <= ^NaiveDateTime.utc_now()
      )
      |> Repo.update_all(set: [status: "published", published_at: ^NaiveDateTime.utc_now()])
    end
  end

  defmodule CommentRepo do
    import Ecto.Query
    alias ElixirEctoQueries.{Comment, User, Post, Repo}

    def list_comments do
      from(c in Comment,
        preload: [:user, :post],
        order_by: [desc: c.inserted_at]
      )
      |> Repo.all()
    end

    def list_comments_by_post(post_id) do
      from(c in Comment,
        where: c.post_id == ^post_id and c.is_approved == true,
        preload: [:user],
        order_by: [asc: c.inserted_at]
      )
      |> Repo.all()
    end

    def list_comments_by_user(user_id) do
      from(c in Comment,
        where: c.user_id == ^user_id,
        preload: [:post],
        order_by: [desc: c.inserted_at]
      )
      |> Repo.all()
    end

    def list_pending_comments do
      from(c in Comment,
        where: c.is_approved == false,
        preload: [:user, :post],
        order_by: [asc: c.inserted_at]
      )
      |> Repo.all()
    end

    def get_comments_with_replies do
      from(c in Comment,
        where: is_nil(c.parent_comment_id),
        preload: [:user, :post, replies: [:user]],
        order_by: [asc: c.inserted_at]
      )
      |> Repo.all()
    end

    def approve_comment(comment_id) do
      from(c in Comment, where: c.id == ^comment_id)
      |> Repo.update_all(set: [is_approved: true])
    end

    def bulk_approve_old_comments(days \\ 7) do
      from(c in Comment,
        where: c.is_approved == false and c.inserted_at < ago(^days, "day")
      )
      |> Repo.update_all(set: [is_approved: true])
    end

    def delete_spam_comments do
      # This would need to be implemented with proper spam detection
      from(c in Comment,
        where: c.is_approved == false and c.inserted_at < ago(^30, "day")
      )
      |> Repo.delete_all()
    end
  end

  defmodule AnalyticsRepo do
    import Ecto.Query
    alias ElixirEctoQueries.{User, Post, Comment, Like, Repo}

    def get_daily_active_users(days \\ 30) do
      from(u in User,
        where: u.last_login_at >= ago(^days, "day"),
        select: %{date: date(u.last_login_at), count: count()},
        group_by: [date(u.last_login_at)],
        order_by: [asc: date(u.last_login_at)]
      )
      |> Repo.all()
    end

    def get_posts_created_per_day(days \\ 30) do
      from(p in Post,
        where: p.inserted_at >= ago(^days, "day"),
        select: %{date: date(p.inserted_at), count: count()},
        group_by: [date(p.inserted_at)],
        order_by: [asc: date(p.inserted_at)]
      )
      |> Repo.all()
    end

    def get_user_engagement_metrics do
      Repo.one!(
        from(u in User,
          left_join: p in Post, on: p.user_id == u.id,
          left_join: c in Comment, on: c.user_id == u.id,
          left_join: l in Like, on: l.user_id == u.id,
          select: %{
            total_users: count(u.id),
            active_users: count(filter: not is_nil(u.last_login_at)),
            users_with_posts: count(filter: not is_nil(p.id)),
            users_with_comments: count(filter: not is_nil(c.id)),
            users_with_likes: count(filter: not is_nil(l.id)),
            total_posts: count(p.id),
            total_comments: count(c.id),
            total_likes: count(l.id)
          }
        )
      )
    end

    def get_top_contributors(limit \\ 10) do
      from(u in User,
        left_join: p in Post, on: p.user_id == u.id,
        left_join: c in Comment, on: c.user_id == u.id,
        group_by: u.id,
        select: %{
          user: u,
          post_count: count(p.id),
          comment_count: count(c.id),
          total_contributions: count(p.id) + count(c.id)
        },
        order_by: [desc: count(p.id) + count(c.id)],
        limit: ^limit
      )
      |> Repo.all()
    end

    def get_content_performance do
      from(p in Post,
        where: p.status == "published",
        select: %{
          post: p,
          engagement_score: p.view_count + p.like_count * 2 + p.comment_count * 3
        },
        order_by: [desc: p.view_count + p.like_count * 2 + p.comment_count * 3]
      )
      |> Repo.all()
    end
  end

  # Complex Query Examples
  defmodule ComplexQueries do
    import Ecto.Query
    alias ElixirEctoQueries.{User, Post, Comment, Like, Repo}

    def get_user_feed(user_id, limit \\ 20) do
      # Get posts from users that the current user follows
      from(p in Post,
        join: f in "followers", on: f.following_id == p.user_id,
        where: f.follower_id == ^user_id and p.status == "published",
        preload: [:user, :comments, :likes],
        order_by: [desc: p.inserted_at],
        limit: ^limit
      )
      |> Repo.all()
    end

    def get_related_posts(post_id, limit \\ 5) do
      # Get posts by the same author or with similar tags
      post = Repo.get!(Post, post_id)

      # Posts by same author
      author_posts = from(p in Post,
        where: p.user_id == ^post.user_id and p.id != ^post_id and p.status == "published",
        preload: [:user],
        limit: ^(div(limit, 2))
      )
      |> Repo.all()

      # Posts with similar tags (simplified - in real app would use proper tagging)
      similar_posts = from(p in Post,
        where: p.id != ^post_id and p.status == "published",
        order_by: [desc: p.inserted_at],
        limit: ^(limit - div(limit, 2))
      )
      |> Repo.all()

      Enum.uniq_by(author_posts ++ similar_posts, & &1.id) |> Enum.take(limit)
    end

    def get_trending_posts(days \\ 7, limit \\ 10) do
      from(p in Post,
        where: p.status == "published" and p.inserted_at >= ago(^days, "day"),
        select: %{p | trending_score: p.view_count + p.like_count * 3 + p.comment_count * 5},
        order_by: [desc: p.view_count + p.like_count * 3 + p.comment_count * 5],
        limit: ^limit
      )
      |> Repo.all()
    end

    def get_user_activity_summary(user_id, days \\ 30) do
      Repo.one!(
        from(u in User,
          where: u.id == ^user_id,
          left_join: p in Post, on: p.user_id == u.id and p.inserted_at >= ago(^days, "day"),
          left_join: c in Comment, on: c.user_id == u.id and c.inserted_at >= ago(^days, "day"),
          left_join: l in Like, on: l.user_id == u.id and l.inserted_at >= ago(^days, "day"),
          select: %{
            posts_created: count(p.id),
            comments_made: count(c.id),
            posts_liked: count(l.id),
            last_activity: max(p.inserted_at, c.inserted_at, l.inserted_at)
          }
        )
      )
    end

    def get_content_statistics do
      Repo.transaction(fn ->
        total_users = Repo.one!(from(u in User, select: count()))
        total_posts = Repo.one!(from(p in Post, select: count()))
        total_comments = Repo.one!(from(c in Comment, select: count()))
        total_likes = Repo.one!(from(l in Like, select: count()))

        published_posts = Repo.one!(from(p in Post, where: p.status == "published", select: count()))
        active_users_today = Repo.one!(from(u in User, where: u.last_login_at >= today(), select: count()))

        %{
          total_users: total_users,
          total_posts: total_posts,
          total_comments: total_comments,
          total_likes: total_likes,
          published_posts: published_posts,
          active_users_today: active_users_today,
          avg_posts_per_user: if(total_users > 0, do: Float.round(total_posts / total_users, 2), else: 0),
          avg_comments_per_post: if(total_posts > 0, do: Float.round(total_comments / total_posts, 2), else: 0)
        }
      end)
    end

    def search_with_facets(search_term, filters \\ %{}) do
      query = from(p in Post,
        where: p.status == "published",
        preload: [:user, :comments, :likes]
      )

      # Add search term
      query = if search_term != "" do
        pattern = "%#{search_term}%"
        from(p in query,
          where: ilike(p.title, ^pattern) or ilike(p.content, ^pattern)
        )
      else
        query
      end

      # Add filters
      query = case filters do
        %{user_id: user_id} when user_id != nil ->
          from(p in query, where: p.user_id == ^user_id)
        %{date_from: date_from, date_to: date_to} when date_from != nil and date_to != nil ->
          from(p in query, where: p.inserted_at >= ^date_from and p.inserted_at <= ^date_to)
        %{min_likes: min_likes} when min_likes != nil ->
          from(p in query, where: p.like_count >= ^min_likes)
        _ ->
          query
      end

      from(p in query, order_by: [desc: p.inserted_at])
      |> Repo.all()
    end

    defp today do
      Date.utc_today() |> NaiveDateTime.new!(~T[00:00:00])
    end
  end

  # Migration Examples
  defmodule Migrations do
    defmodule CreateUsers do
      use Ecto.Migration

      def change do
        create table(:users, primary_key: false) do
          add :id, :binary_id, primary_key: true, default: fragment("gen_random_uuid()")
          add :name, :text, null: false
          add :email, :text, null: false
          add :encrypted_password, :text
          add :age, :integer
          add :bio, :text
          add :is_active, :boolean, default: true, null: false
          add :role, :text, default: "user", null: false
          add :avatar_url, :text
          add :last_login_at, :naive_datetime
          add :settings, :map, default: %{}

          timestamps()
        end

        create index(:users, [:email], unique: true)
        create index(:users, [:is_active])
        create index(:users, [:role])
        create index(:users, [:last_login_at])
      end
    end

    defmodule CreatePosts do
      use Ecto.Migration

      def change do
        create table(:posts, primary_key: false) do
          add :id, :binary_id, primary_key: true, default: fragment("gen_random_uuid()")
          add :title, :text, null: false
          add :content, :text, null: false
          add :slug, :text, null: false
          add :status, :text, default: "draft", null: false
          add :view_count, :integer, default: 0, null: false
          add :like_count, :integer, default: 0, null: false
          add :comment_count, :integer, default: 0, null: false
          add :published_at, :naive_datetime
          add :tags, {:array, :text}, default: []
          add :user_id, references(:users, type: :binary_id, on_delete: :nothing), null: false

          timestamps()
        end

        create index(:posts, [:user_id])
        create index(:posts, [:status])
        create index(:posts, [:slug], unique: true)
        create index(:posts, [:published_at])
        create index(:posts, [:inserted_at])
      end
    end

    defmodule CreateComments do
      use Ecto.Migration

      def change do
        create table(:comments, primary_key: false) do
          add :id, :binary_id, primary_key: true, default: fragment("gen_random_uuid()")
          add :content, :text, null: false
          add :is_approved, :boolean, default: false, null: false
          add :user_id, references(:users, type: :binary_id, on_delete: :nothing), null: false
          add :post_id, references(:posts, type: :binary_id, on_delete: :nothing), null: false
          add :parent_comment_id, references(:comments, type: :binary_id, on_delete: :nothing)

          timestamps()
        end

        create index(:comments, [:user_id])
        create index(:comments, [:post_id])
        create index(:comments, [:parent_comment_id])
        create index(:comments, [:is_approved])
        create index(:comments, [:inserted_at])
      end
    end
  end
end

# Demo usage
defmodule ElixirEctoQueriesDemo do
  def run do
    IO.puts("=== Elixir Ecto Queries Demo ===")

    # Show Ecto schema definitions
    IO.puts("\n--- Ecto Schema Definitions ---")
    IO.puts("• User - User management with profile data")
    IO.puts("• Post - Blog posts with metadata")
    IO.puts("• Comment - Nested comments system")
    IO.puts("• Like - Post liking functionality")
    IO.puts("• Follower - User following relationships")
    IO.puts("• PostTag - Many-to-many post tagging")
    IO.puts("• Tag - Content categorization")

    # Show repository patterns
    IO.puts("\n--- Repository Patterns ---")
    IO.puts("User Repository:")
    IO.puts("• Basic CRUD operations")
    IO.puts("• Search and filtering")
    IO.puts("• Preloading associations")
    IO.puts("• Aggregations and statistics")
    IO.puts("• Pagination")
    IO.puts("• Bulk operations")

    IO.puts("\nPost Repository:")
    IO.puts("• Status-based filtering")
    IO.puts("• Tag-based queries")
    IO.puts("• Popular content queries")
    IO.puts("• Counter updates")
    IO.puts("• Scheduled content publishing")

    # Show complex queries
    IO.puts("\n--- Complex Query Examples ---")
    IO.puts("• User feed generation")
    IO.puts("• Related content discovery")
    IO.puts("• Trending posts calculation")
    IO.puts("• Activity summaries")
    IO.puts("• Full-text search with facets")
    IO.puts("• Analytics and reporting")

    # Show query patterns
    IO.puts("\n--- Query Patterns ---")
    IO.puts("• Eager loading with preload")
    IO.puts("• Joins and associations")
    IO.puts("• Grouping and aggregations")
    IO.puts("• Window functions")
    IO.puts("• Full-text search")
    IO.puts("• Pagination strategies")

    # Show data validation
    IO.puts("\n--- Data Validation ---")
    IO.puts("• Changesets for data validation")
    IO.puts("• Custom validation functions")
    IO.puts("• Association constraints")
    IO.puts("• Database-level constraints")
    IO.puts("• Type casting and coercion")

    # Show performance considerations
    IO.puts("\n--- Performance Considerations ---")
    IO.puts("• Proper indexing strategies")
    IO.puts("• Query optimization")
    IO.puts("• Connection pooling")
    IO.puts("• Eager loading vs lazy loading")
    IO.puts("• Batch operations")

    # Show migrations
    IO.puts("\n--- Database Migrations ---")
    IO.puts("• Table creation with proper types")
    IO.puts("• Index definitions")
    IO.puts("• Foreign key constraints")
    IO.puts("• Default values and constraints")
    IO.puts("• Rollback procedures")

    # Show transaction examples
    IO.puts("\n--- Transaction Examples ---")
    IO.puts("• Multi-table operations")
    IO.puts("• Consistency guarantees")
    IO.puts("• Error handling")
    IO.puts("• Isolation levels")
    IO.puts("• Deadlock prevention")

    IO.puts("\n=== Ecto Queries Demo Complete ===")
  end
end

# Run the demo
if __ENV__.module == ElixirEctoQueriesDemo do
  ElixirEctoQueriesDemo.run()
end
