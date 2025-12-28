import unittest
import sys
import os
from unittest.mock import patch, MagicMock, mock_open
import tempfile
import shutil

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from modules.slides_generator import slides, download_slide, present_slide


class TestSlidesGenerator(unittest.TestCase):
    """Comprehensive tests for slides_generator module."""

    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()

    def tearDown(self):
        """Clean up test fixtures."""
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)

    @patch('modules.slides_generator.has_permission')
    @patch('modules.slides_generator.Presentation')
    def test_slides_admin_can_create(self, mock_prs_class, mock_has_perm):
        """Test admin user can create slides."""
        mock_has_perm.return_value = True
        mock_prs = MagicMock()
        mock_prs_class.return_value = mock_prs
        
        result = slides("Test presentation", "admin")
        
        self.assertNotIn("do not have permission", result)
        self.assertIn("Presentation created", result)
        self.assertIn("/tmp/presentation.pptx", result)
        mock_has_perm.assert_called_once_with("admin", "create_slides")

    @patch('modules.slides_generator.has_permission')
    def test_slides_viewer_cannot_create(self, mock_has_perm):
        """Test viewer user cannot create slides."""
        mock_has_perm.return_value = False
        
        result = slides("Test presentation", "viewer")
        
        self.assertEqual(result, "You do not have permission to create slides.")
        mock_has_perm.assert_called_once_with("viewer", "create_slides")

    @patch('modules.slides_generator.has_permission')
    def test_slides_editor_can_create(self, mock_has_perm):
        """Test editor user can create slides."""
        mock_has_perm.return_value = True
        
        with patch('modules.slides_generator.Presentation') as mock_prs_class:
            mock_prs = MagicMock()
            mock_prs_class.return_value = mock_prs
            
            result = slides("Editor presentation", "editor")
            
            self.assertNotIn("do not have permission", result)
            self.assertIn("Presentation created", result)

    @patch('modules.slides_generator.has_permission')
    def test_slides_unknown_user_cannot_create(self, mock_has_perm):
        """Test unknown user cannot create slides."""
        mock_has_perm.return_value = False
        
        result = slides("Test", "unknown_user")
        
        self.assertEqual(result, "You do not have permission to create slides.")

    @patch('modules.slides_generator.has_permission')
    @patch('modules.slides_generator.Presentation')
    def test_slides_creates_title_slide(self, mock_prs_class, mock_has_perm):
        """Test slides function creates a title slide."""
        mock_has_perm.return_value = True
        mock_prs = MagicMock()
        mock_slide = MagicMock()
        mock_prs.slides.add_slide.return_value = mock_slide
        mock_prs_class.return_value = mock_prs
        
        prompt = "My Presentation Topic"
        slides(prompt, "admin")
        
        # Verify title slide layout was used
        mock_prs.slide_layouts.__getitem__.assert_called_with(0)
        # Verify slide was added
        self.assertTrue(mock_prs.slides.add_slide.called)

    @patch('modules.slides_generator.has_permission')
    @patch('modules.slides_generator.Presentation')
    def test_slides_sets_title_and_subtitle(self, mock_prs_class, mock_has_perm):
        """Test slides function sets title and subtitle."""
        mock_has_perm.return_value = True
        mock_prs = MagicMock()
        mock_slide = MagicMock()
        mock_title = MagicMock()
        mock_subtitle = MagicMock()
        
        mock_slide.shapes.title = mock_title
        mock_slide.placeholders = {1: mock_subtitle}
        mock_prs.slides.add_slide.return_value = mock_slide
        mock_prs_class.return_value = mock_prs
        
        prompt = "AI and Machine Learning"
        slides(prompt, "admin")
        
        # Verify title was set
        self.assertEqual(mock_title.text, "Presentation Title")
        # Verify subtitle was set to prompt
        self.assertEqual(mock_subtitle.text, prompt)

    @patch('modules.slides_generator.has_permission')
    @patch('modules.slides_generator.Presentation')
    def test_slides_saves_to_tmp(self, mock_prs_class, mock_has_perm):
        """Test slides saves presentation to /tmp directory."""
        mock_has_perm.return_value = True
        mock_prs = MagicMock()
        mock_prs_class.return_value = mock_prs
        
        result = slides("Test", "admin")
        
        mock_prs.save.assert_called_once_with("/tmp/presentation.pptx")
        self.assertIn("/tmp/presentation.pptx", result)

    @patch('modules.slides_generator.has_permission')
    @patch('modules.slides_generator.Presentation')
    def test_slides_with_empty_prompt(self, mock_prs_class, mock_has_perm):
        """Test slides function with empty prompt."""
        mock_has_perm.return_value = True
        mock_prs = MagicMock()
        mock_prs_class.return_value = mock_prs
        
        result = slides("", "admin")
        
        self.assertIn("Presentation created", result)

    @patch('modules.slides_generator.has_permission')
    @patch('modules.slides_generator.Presentation')
    def test_slides_with_special_characters_prompt(self, mock_prs_class, mock_has_perm):
        """Test slides with special characters in prompt."""
        mock_has_perm.return_value = True
        mock_prs = MagicMock()
        mock_prs_class.return_value = mock_prs
        
        special_prompts = [
            "Title with <html> tags",
            "Title with \"quotes\"",
            "Title with 'apostrophes'",
            "Unicode: 你好 مرحبا",
            "Emoji: 🚀 💻"
        ]
        
        for prompt in special_prompts:
            result = slides(prompt, "admin")
            self.assertIn("Presentation created", result)

    @patch('modules.slides_generator.has_permission')
    def test_download_slide_admin_can_download(self, mock_has_perm):
        """Test admin user can download slides."""
        mock_has_perm.return_value = True
        
        result = download_slide("admin", "slide123")
        
        self.assertNotIn("do not have permission", result)
        self.assertIn("Downloading slide", result)
        self.assertIn("slide123", result)
        mock_has_perm.assert_called_once_with("admin", "download_slides")

    @patch('modules.slides_generator.has_permission')
    def test_download_slide_viewer_cannot_download(self, mock_has_perm):
        """Test viewer user cannot download slides."""
        mock_has_perm.return_value = False
        
        result = download_slide("viewer", "slide123")
        
        self.assertEqual(result, "You do not have permission to download slides.")

    @patch('modules.slides_generator.has_permission')
    def test_download_slide_editor_cannot_download(self, mock_has_perm):
        """Test editor user cannot download slides."""
        mock_has_perm.return_value = False
        
        result = download_slide("editor", "slide456")
        
        self.assertEqual(result, "You do not have permission to download slides.")

    @patch('modules.slides_generator.has_permission')
    def test_download_slide_with_various_slide_ids(self, mock_has_perm):
        """Test download_slide with various slide ID formats."""
        mock_has_perm.return_value = True
        
        slide_ids = ["123", "abc-def", "slide_001", "", "special!@#"]
        
        for slide_id in slide_ids:
            result = download_slide("admin", slide_id)
            self.assertIn("Downloading slide", result)
            self.assertIn(slide_id, result)

    @patch('modules.slides_generator.has_permission')
    def test_present_slide_admin_can_present(self, mock_has_perm):
        """Test admin user can present slides."""
        mock_has_perm.return_value = True
        
        result = present_slide("admin", "slide789")
        
        self.assertNotIn("do not have permission", result)
        self.assertIn("Presenting slide", result)
        self.assertIn("slide789", result)
        mock_has_perm.assert_called_once_with("admin", "present_slides")

    @patch('modules.slides_generator.has_permission')
    def test_present_slide_viewer_cannot_present(self, mock_has_perm):
        """Test viewer user cannot present slides."""
        mock_has_perm.return_value = False
        
        result = present_slide("viewer", "slide789")
        
        self.assertEqual(result, "You do not have permission to present slides.")

    @patch('modules.slides_generator.has_permission')
    def test_present_slide_editor_cannot_present(self, mock_has_perm):
        """Test editor user cannot present slides."""
        mock_has_perm.return_value = False
        
        result = present_slide("editor", "slide999")
        
        self.assertEqual(result, "You do not have permission to present slides.")

    @patch('modules.slides_generator.has_permission')
    def test_present_slide_with_various_slide_ids(self, mock_has_perm):
        """Test present_slide with various slide ID formats."""
        mock_has_perm.return_value = True
        
        slide_ids = ["1", "presentation-final", "slide_v2", "", "test@123"]
        
        for slide_id in slide_ids:
            result = present_slide("admin", slide_id)
            self.assertIn("Presenting slide", result)
            self.assertIn(slide_id, result)

    @patch('modules.slides_generator.has_permission')
    def test_all_functions_check_permissions(self, mock_has_perm):
        """Test that all functions properly check permissions."""
        mock_has_perm.return_value = False
        
        # Test slides
        result1 = slides("test", "user")
        self.assertIn("do not have permission", result1)
        
        # Test download_slide
        result2 = download_slide("user", "id")
        self.assertIn("do not have permission", result2)
        
        # Test present_slide
        result3 = present_slide("user", "id")
        self.assertIn("do not have permission", result3)
        
        # Verify has_permission was called 3 times
        self.assertEqual(mock_has_perm.call_count, 3)


if __name__ == '__main__':
    unittest.main()