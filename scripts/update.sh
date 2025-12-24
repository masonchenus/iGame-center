#!/bin/bash
# update.sh - Pull latest changes and optionally restart service

# Set project directory
PROJECT_DIR="/Users/mason/Game Center Project/"

# Change to project directory
cd "$PROJECT_DIR" || { echo "Project directory not found!"; exit 1; }

# Pull latest changes from Git
echo "Pulling latest changes..."
git pull origin main

# Optional: restart service (uncomment if needed)
# SERVICE_NAME="my_service"
# echo "Restarting $SERVICE_NAME..."
# sudo systemctl restart "$SERVICE_NAME"

echo "Update completed successfully!"
