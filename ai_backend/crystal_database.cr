# Crystal Database - SQLite Integration Example
require "sqlite3"

class DatabaseDemo
  def initialize(db_path : String = "demo.db")
    @db = SQLite3::Database.new(db_path)
    setup_database
  end
  
  def setup_database
    @db.execute <<-SQL
      CREATE TABLE IF NOT EXISTS users (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        email TEXT UNIQUE NOT NULL,
        age INTEGER,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      );
    SQL
    
    @db.execute <<-SQL
      CREATE TABLE IF NOT EXISTS products (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        price REAL NOT NULL,
        category TEXT,
        in_stock BOOLEAN DEFAULT 1
      );
    SQL
    
    puts "Database tables created successfully"
  end
  
  def insert_sample_data
    # Insert sample users
    users = [
      {"name" => "Alice Johnson", "email" => "alice@example.com", "age" => 28},
      {"name" => "Bob Smith", "email" => "bob@example.com", "age" => 35},
      {"name" => "Charlie Brown", "email" => "charlie@example.com", "age" => 22},
      {"name" => "Diana Prince", "email" => "diana@example.com", "age" => 31}
    ]
    
    users.each do |user|
      @db.execute(
        "INSERT INTO users (name, email, age) VALUES (?, ?, ?)",
        [user["name"], user["email"], user["age"]]
      )
    end
    
    # Insert sample products
    products = [
      {"name" => "Laptop", "price" => 999.99, "category" => "Electronics"},
      {"name" => "Coffee Mug", "price" => 12.99, "category" => "Kitchen"},
      {"name" => "Book: Programming in Crystal", "price" => 45.00, "category" => "Books"},
      {"name" => "Wireless Mouse", "price" => 29.99, "category" => "Electronics"}
    ]
    
    products.each do |product|
      @db.execute(
        "INSERT INTO products (name, price, category) VALUES (?, ?, ?)",
        [product["name"], product["price"], product["category"]]
      )
    end
    
    puts "Sample data inserted"
  end
  
  def query_users
    puts "\n=== All Users ==="
    @db.query("SELECT * FROM users") do |rs|
      rs.each do
        id = rs.read(Int32)
        name = rs.read(String)
        email = rs.read(String)
        age = rs.read(Int32)
        created_at = rs.read(String)
        puts "ID: #{id}, Name: #{name}, Email: #{email}, Age: #{age}, Created: #{created_at}"
      end
    end
  end
  
  def query_products_by_category(category : String)
    puts "\n=== Products in #{category} ==="
    @db.query("SELECT * FROM products WHERE category = ?", [category]) do |rs|
      rs.each do
        id = rs.read(Int32)
        name = rs.read(String)
        price = rs.read(Float64)
        product_category = rs.read(String)
        in_stock = rs.read(Bool)
        puts "ID: #{id}, Name: #{name}, Price: $#{price}, Category: #{product_category}, In Stock: #{in_stock}"
      end
    end
  end
  
  def get_user_stats
    puts "\n=== User Statistics ==="
    @db.query("SELECT COUNT(*) as user_count FROM users") do |rs|
      count = rs.read(Int32)
      puts "Total Users: #{count}"
    end
    
    @db.query("SELECT AVG(age) as avg_age FROM users") do |rs|
      avg = rs.read(Float64)
      puts "Average Age: #{avg.round(2)}"
    end
  end
  
  def update_user_age(user_id : Int32, new_age : Int32)
    @db.execute(
      "UPDATE users SET age = ? WHERE id = ?",
      [new_age, user_id]
    )
    puts "Updated user #{user_id} age to #{new_age}"
  end
  
  def close
    @db.close
    puts "Database connection closed"
  end
end

# Demo usage
if __FILE__ == $0
  db_demo = DatabaseDemo.new
  
  begin
    db_demo.insert_sample_data
    db_demo.query_users
    db_demo.query_products_by_category("Electronics")
    db_demo.get_user_stats
    
    # Update example
    db_demo.update_user_age(1, 29)
    db_demo.query_users
  ensure
    db_demo.close
  end
end
