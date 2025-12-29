using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;

/// <summary>
/// Classic Snake Game implemented in C#
/// Demonstrates game loop, collision detection, and graphics rendering
/// </summary>
public class SnakeGame : Form
{
    private const int GRID_SIZE = 20;
    private const int GRID_WIDTH = 30;
    private const int GRID_HEIGHT = 20;
    private const int CELL_SIZE = 25;
    
    private List<Point> snake;
    private Point food;
    private Direction currentDirection;
    private Direction nextDirection;
    private int score;
    private bool gameOver;
    private System.Windows.Forms.Timer gameTimer;
    
    private enum Direction
    {
        Up, Down, Left, Right
    }
    
    public SnakeGame()
    {
        InitializeGame();
        InitializeUI();
        StartGame();
    }
    
    private void InitializeGame()
    {
        // Initialize snake in center of screen
        snake = new List<Point>();
        snake.Add(new Point(GRID_WIDTH / 2, GRID_HEIGHT / 2));
        snake.Add(new Point(GRID_WIDTH / 2 - 1, GRID_HEIGHT / 2));
        snake.Add(new Point(GRID_WIDTH / 2 - 2, GRID_HEIGHT / 2));
        
        currentDirection = Direction.Right;
        nextDirection = Direction.Right;
        score = 0;
        gameOver = false;
        
        GenerateFood();
    }
    
    private void InitializeUI()
    {
        this.Size = new Size(GRID_WIDTH * CELL_SIZE + 40, GRID_HEIGHT * CELL_SIZE + 80);
        this.Text = $"Snake Game - Score: {score}";
        this.StartPosition = FormStartPosition.CenterScreen;
        this.FormBorderStyle = FormBorderStyle.FixedSingle;
        this.MaximizeBox = false;
        
        // Setup game timer
        gameTimer = new System.Windows.Forms.Timer();
        gameTimer.Interval = 150; // Game speed
        gameTimer.Tick += GameLoop;
        
        // Enable key events
        this.KeyPreview = true;
        this.KeyDown += HandleKeyPress;
    }
    
    private void StartGame()
    {
        gameTimer.Start();
    }
    
    private void GenerateFood()
    {
        Random random = new Random();
        Point newFood;
        
        do
        {
            newFood = new Point(random.Next(GRID_WIDTH), random.Next(GRID_HEIGHT));
        } while (snake.Contains(newFood));
        
        food = newFood;
    }
    
    private void GameLoop(object sender, EventArgs e)
    {
        if (gameOver) return;
        
        // Update direction
        currentDirection = nextDirection;
        
        // Calculate new head position
        Point head = snake[0];
        Point newHead = head;
        
        switch (currentDirection)
        {
            case Direction.Up:
                newHead.Y--;
                break;
            case Direction.Down:
                newHead.Y++;
                break;
            case Direction.Left:
                newHead.X--;
                break;
            case Direction.Right:
                newHead.X++;
                break;
        }
        
        // Check wall collision
        if (newHead.X < 0 || newHead.X >= GRID_WIDTH || 
            newHead.Y < 0 || newHead.Y >= GRID_HEIGHT)
        {
            EndGame();
            return;
        }
        
        // Check self collision
        if (snake.Contains(newHead))
        {
            EndGame();
            return;
        }
        
        // Add new head
        snake.Insert(0, newHead);
        
        // Check food collision
        if (newHead == food)
        {
            score += 10;
            this.Text = $"Snake Game - Score: {score}";
            GenerateFood();
            
            // Increase speed every 50 points
            if (score % 50 == 0 && gameTimer.Interval > 50)
            {
                gameTimer.Interval -= 10;
            }
        }
        else
        {
            // Remove tail
            snake.RemoveAt(snake.Count - 1);
        }
        
        // Redraw
        this.Invalidate();
    }
    
    private void HandleKeyPress(object sender, KeyEventArgs e)
    {
        if (gameOver)
        {
            if (e.KeyCode == Keys.Space)
            {
                InitializeGame();
                this.Invalidate();
            }
            return;
        }
        
        // Prevent reverse direction
        switch (e.KeyCode)
        {
            case Keys.Up:
                if (currentDirection != Direction.Down)
                    nextDirection = Direction.Up;
                break;
            case Keys.Down:
                if (currentDirection != Direction.Up)
                    nextDirection = Direction.Down;
                break;
            case Keys.Left:
                if (currentDirection != Direction.Right)
                    nextDirection = Direction.Left;
                break;
            case Keys.Right:
                if (currentDirection != Direction.Left)
                    nextDirection = Direction.Right;
                break;
            case Keys.Escape:
                this.Close();
                break;
        }
    }
    
    private void EndGame()
    {
        gameOver = true;
        gameTimer.Stop();
        this.Text = $"Game Over! Final Score: {score} - Press SPACE to restart";
        this.Invalidate();
    }
    
    protected override void OnPaint(PaintEventArgs e)
    {
        base.OnPaint(e);
        Graphics graphics = e.Graphics;
        
        // Clear background
        graphics.FillRectangle(Brushes.Black, 0, 0, this.Width, this.Height);
        
        if (gameOver)
        {
            // Draw game over message
            using (Font gameOverFont = new Font("Arial", 24, FontStyle.Bold))
            using (Font scoreFont = new Font("Arial", 16))
            {
                SizeF gameOverSize = graphics.MeasureString("GAME OVER", gameOverFont);
                SizeF scoreSize = graphics.MeasureString($"Final Score: {score}", scoreFont);
                SizeF restartSize = graphics.MeasureString("Press SPACE to restart", scoreFont);
                
                float centerX = (this.Width - gameOverSize.Width) / 2;
                float centerY = this.Height / 2 - 50;
                
                graphics.DrawString("GAME OVER", gameOverFont, Brushes.Red, centerX, centerY);
                graphics.DrawString($"Final Score: {score}", scoreFont, Brushes.White, 
                    (this.Width - scoreSize.Width) / 2, centerY + 40);
                graphics.DrawString("Press SPACE to restart", scoreFont, Brushes.Yellow,
                    (this.Width - restartSize.Width) / 2, centerY + 70);
            }
            return;
        }
        
        // Draw grid
        using (Pen gridPen = new Pen(Color.DarkGray, 1))
        {
            for (int x = 0; x <= GRID_WIDTH; x++)
            {
                graphics.DrawLine(gridPen, x * CELL_SIZE + 20, 40, x * CELL_SIZE + 20, GRID_HEIGHT * CELL_SIZE + 40);
            }
            for (int y = 0; y <= GRID_HEIGHT; y++)
            {
                graphics.DrawLine(gridPen, 20, y * CELL_SIZE + 40, GRID_WIDTH * CELL_SIZE + 20, y * CELL_SIZE + 40);
            }
        }
        
        // Draw snake
        for (int i = 0; i < snake.Count; i++)
        {
            Point segment = snake[i];
            Rectangle cellRect = new Rectangle(
                segment.X * CELL_SIZE + 21,
                segment.Y * CELL_SIZE + 41,
                CELL_SIZE - 2,
                CELL_SIZE - 2
            );
            
            if (i == 0)
            {
                // Snake head
                graphics.FillRectangle(Brushes.LightGreen, cellRect);
                graphics.DrawRectangle(Pens.DarkGreen, cellRect);
                
                // Draw eyes
                using (Brush eyeBrush = Brushes.Black)
                {
                    int eyeSize = 3;
                    graphics.FillRectangle(eyeBrush, cellRect.X + 5, cellRect.Y + 5, eyeSize, eyeSize);
                    graphics.FillRectangle(eyeBrush, cellRect.X + CELL_SIZE - 8, cellRect.Y + 5, eyeSize, eyeSize);
                }
            }
            else
            {
                // Snake body
                graphics.FillRectangle(Brushes.Green, cellRect);
                graphics.DrawRectangle(Pens.DarkGreen, cellRect);
            }
        }
        
        // Draw food
        Rectangle foodRect = new Rectangle(
            food.X * CELL_SIZE + 21,
            food.Y * CELL_SIZE + 41,
            CELL_SIZE - 2,
            CELL_SIZE - 2
        );
        graphics.FillEllipse(Brushes.Red, foodRect);
        graphics.DrawEllipse(Pens.DarkRed, foodRect);
    }
    
    protected override void OnFormClosed(FormClosedEventArgs e)
    {
        gameTimer?.Stop();
        gameTimer?.Dispose();
        base.OnFormClosed(e);
    }
}

/// <summary>
/// Game utilities and helper functions for C# games
/// </summary>
public static class GameUtils
{
    /// <summary>
    /// Calculate distance between two points
    /// </summary>
    public static double CalculateDistance(Point p1, Point p2)
    {
        return Math.Sqrt(Math.Pow(p2.X - p1.X, 2) + Math.Pow(p2.Y - p1.Y, 2));
    }
    
    /// <summary>
    /// Check if two rectangles intersect
    /// </summary>
    public static bool RectanglesIntersect(Rectangle r1, Rectangle r2)
    {
        return r1.IntersectsWith(r2);
    }
    
    /// <summary>
    /// Clamp a value between minimum and maximum
    /// </summary>
    public static int Clamp(int value, int min, int max)
    {
        return Math.Max(min, Math.Min(max, value));
    }
    
    /// <summary>
    /// Generate random number within range
    /// </summary>
    public static int RandomRange(int min, int max, Random random = null)
    {
        random ??= new Random();
        return random.Next(min, max + 1);
    }
    
    /// <summary>
    /// Linear interpolation between two values
    /// </summary>
    public static float Lerp(float start, float end, float t)
    {
        return start + (end - start) * t;
    }
    
    /// <summary>
    /// Check if point is within rectangle bounds
    /// </summary>
    public static bool PointInRectangle(Point point, Rectangle rectangle)
    {
        return point.X >= rectangle.Left && 
               point.X <= rectangle.Right && 
               point.Y >= rectangle.Top && 
               point.Y <= rectangle.Bottom;
    }
    
    /// <summary>
    /// Convert grid coordinates to screen coordinates
    /// </summary>
    public static Point GridToScreen(Point gridPoint, int cellSize, int offsetX = 0, int offsetY = 0)
    {
        return new Point(
            gridPoint.X * cellSize + offsetX,
            gridPoint.Y * cellSize + offsetY
        );
    }
    
    /// <summary>
    /// Convert screen coordinates to grid coordinates
    /// </summary>
    public static Point ScreenToGrid(Point screenPoint, int cellSize, int offsetX = 0, int offsetY = 0)
    {
        return new Point(
            (screenPoint.X - offsetX) / cellSize,
            (screenPoint.Y - offsetY) / cellSize
        );
    }
}

/// <summary>
/// Main program entry point
/// </summary>
public static class Program
{
    [STAThread]
    public static void Main()
    {
        Application.EnableVisualStyles();
        Application.SetCompatibleTextRenderingDefault(false);
        Application.Run(new SnakeGame());
    }
}
