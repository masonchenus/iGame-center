# A simple dictionary to store user privileges.
# In a real application, this would be stored in a database.
USER_PRIVILEGES = {
    "admin": ["create_slides", "delete_slides", "download_slides", "present_slides"],
    "editor": ["create_slides"],
    "viewer": [],
}

def has_permission(user: str, permission: str) -> bool:
    """
    Checks if a user has a specific permission.
    """
    if user in USER_PRIVILEGES:
        return permission in USER_PRIVILEGES[user]
    return False
