import unittest
import sys
import os

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from modules.privileges import has_permission, USER_PRIVILEGES


class TestPrivileges(unittest.TestCase):
    """Comprehensive tests for privileges module."""

    def test_admin_has_create_slides_permission(self):
        """Test admin user has create_slides permission."""
        self.assertTrue(has_permission("admin", "create_slides"))

    def test_admin_has_delete_slides_permission(self):
        """Test admin user has delete_slides permission."""
        self.assertTrue(has_permission("admin", "delete_slides"))

    def test_admin_has_download_slides_permission(self):
        """Test admin user has download_slides permission."""
        self.assertTrue(has_permission("admin", "download_slides"))

    def test_admin_has_present_slides_permission(self):
        """Test admin user has present_slides permission."""
        self.assertTrue(has_permission("admin", "present_slides"))

    def test_admin_has_all_permissions(self):
        """Test admin has all defined permissions."""
        permissions = ["create_slides", "delete_slides", "download_slides", "present_slides"]
        for perm in permissions:
            self.assertTrue(has_permission("admin", perm),
                          f"Admin should have {perm} permission")

    def test_editor_has_create_slides_permission(self):
        """Test editor user has create_slides permission."""
        self.assertTrue(has_permission("editor", "create_slides"))

    def test_editor_lacks_delete_permission(self):
        """Test editor user lacks delete_slides permission."""
        self.assertFalse(has_permission("editor", "delete_slides"))

    def test_editor_lacks_download_permission(self):
        """Test editor user lacks download_slides permission."""
        self.assertFalse(has_permission("editor", "download_slides"))

    def test_editor_lacks_present_permission(self):
        """Test editor user lacks present_slides permission."""
        self.assertFalse(has_permission("editor", "present_slides"))

    def test_viewer_has_no_permissions(self):
        """Test viewer user has no permissions."""
        permissions = ["create_slides", "delete_slides", "download_slides", "present_slides"]
        for perm in permissions:
            self.assertFalse(has_permission("viewer", perm),
                           f"Viewer should not have {perm} permission")

    def test_unknown_user_has_no_permissions(self):
        """Test unknown user has no permissions."""
        unknown_users = ["guest", "anonymous", "unknown", "", "null"]
        permissions = ["create_slides", "delete_slides", "download_slides", "present_slides"]
        
        for user in unknown_users:
            for perm in permissions:
                self.assertFalse(has_permission(user, perm),
                               f"Unknown user '{user}' should not have {perm}")

    def test_invalid_permission_returns_false(self):
        """Test checking for non-existent permission returns False."""
        invalid_permissions = ["invalid_perm", "read_slides", "update_slides", "", "delete_all"]
        
        for perm in invalid_permissions:
            self.assertFalse(has_permission("admin", perm))
            self.assertFalse(has_permission("editor", perm))
            self.assertFalse(has_permission("viewer", perm))

    def test_case_sensitive_usernames(self):
        """Test that usernames are case-sensitive."""
        self.assertFalse(has_permission("Admin", "create_slides"))
        self.assertFalse(has_permission("ADMIN", "create_slides"))
        self.assertFalse(has_permission("Editor", "create_slides"))
        self.assertFalse(has_permission("VIEWER", "create_slides"))

    def test_case_sensitive_permissions(self):
        """Test that permissions are case-sensitive."""
        self.assertFalse(has_permission("admin", "CREATE_SLIDES"))
        self.assertFalse(has_permission("admin", "Create_Slides"))
        self.assertFalse(has_permission("admin", "create_Slides"))

    def test_user_privileges_structure(self):
        """Test USER_PRIVILEGES dictionary has correct structure."""
        self.assertIsInstance(USER_PRIVILEGES, dict)
        self.assertIn("admin", USER_PRIVILEGES)
        self.assertIn("editor", USER_PRIVILEGES)
        self.assertIn("viewer", USER_PRIVILEGES)
        
        # Check all values are lists
        for user, perms in USER_PRIVILEGES.items():
            self.assertIsInstance(perms, list)

    def test_admin_permissions_list_length(self):
        """Test admin has exactly 4 permissions."""
        self.assertEqual(len(USER_PRIVILEGES["admin"]), 4)

    def test_editor_permissions_list_length(self):
        """Test editor has exactly 1 permission."""
        self.assertEqual(len(USER_PRIVILEGES["editor"]), 1)

    def test_viewer_permissions_list_is_empty(self):
        """Test viewer has empty permissions list."""
        self.assertEqual(len(USER_PRIVILEGES["viewer"]), 0)
        self.assertEqual(USER_PRIVILEGES["viewer"], [])

    def test_whitespace_in_username(self):
        """Test usernames with whitespace are not recognized."""
        self.assertFalse(has_permission(" admin", "create_slides"))
        self.assertFalse(has_permission("admin ", "create_slides"))
        self.assertFalse(has_permission(" admin ", "create_slides"))

    def test_whitespace_in_permission(self):
        """Test permissions with whitespace are not recognized."""
        self.assertFalse(has_permission("admin", " create_slides"))
        self.assertFalse(has_permission("admin", "create_slides "))
        self.assertFalse(has_permission("admin", " create_slides "))

    def test_none_values(self):
        """Test handling of None values."""
        # This will cause TypeError - catching expected behavior
        with self.assertRaises(TypeError):
            has_permission(None, "create_slides")
        
        with self.assertRaises(TypeError):
            has_permission("admin", None)

    def test_numeric_values(self):
        """Test handling of numeric values."""
        self.assertFalse(has_permission(123, "create_slides"))
        self.assertFalse(has_permission("admin", 123))


if __name__ == '__main__':
    unittest.main()