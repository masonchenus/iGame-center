import logging
import os

# Ensure logs directory exists
log_dir = os.path.join(os.path.dirname(__file__), "logs")
os.makedirs(log_dir, exist_ok=True)

# Usage logger
usage_logger = logging.getLogger("usage")
usage_logger.setLevel(logging.INFO)
usage_handler = logging.FileHandler(os.path.join(log_dir, "usage.log"))
usage_handler.setFormatter(logging.Formatter("%(asctime)s,%(message)s"))
usage_logger.addHandler(usage_handler)

# Error logger
error_logger = logging.getLogger("errors")
error_logger.setLevel(logging.ERROR)
error_handler = logging.FileHandler(os.path.join(log_dir, "errors.log"))
error_handler.setFormatter(logging.Formatter("%(asctime)s,%(message)s"))
error_logger.addHandler(error_handler)

# Billing logger
billing_logger = logging.getLogger("billing")
billing_logger.setLevel(logging.INFO)
billing_handler = logging.FileHandler(os.path.join(log_dir, "billing.log"))
billing_handler.setFormatter(logging.Formatter("%(asctime)s,%(message)s"))
billing_logger.addHandler(billing_handler)
